{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TimeChecker where

import           AppTypes
import           Chess
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Foldable            (Foldable (foldl'))
import qualified Data.HashMap.Strict      as HMS
import           Data.Int                 (Int64)
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time
import           Database                 (saveGame)

renderAlert :: Alert -> T.Text
renderAlert H1   = "Less than one hour left for "
renderAlert M30  = "Less than 30 minutes left for "
renderAlert M15  = "Less than 15 minutes left for "
renderAlert M5   = "Less than 5 minutes left for "
renderAlert M1   = "Less than 1 minutes left for "
renderAlert S30  = "Less than 30 seconds left for "
renderAlert S10  = "Less than 10 seconds left for "
renderAlert Lost = "Time elapsed, the game is over. "

countdown :: UTCTime -> UTCTime -> Maybe Alert
countdown n tg
    |   diffUTCTime tg n <= 1 = Just Lost
    |   diffUTCTime tg n < 10 = Just S10
    |   diffUTCTime tg n < 30 = Just S30
    |   diffUTCTime tg n < 60 = Just M1
    |   diffUTCTime tg n < 300 = Just M5
    |   diffUTCTime tg n < 900 = Just M15
    |   diffUTCTime tg n < 1800 = Just M30
    |   diffUTCTime tg n < 3600 = Just H1
    |   otherwise = Nothing

checkGameTime :: GameState -> UTCTime -> Maybe Alert
checkGameTime game@GameState{..} now = case status of
    Finished _ -> Nothing
    Started -> case timeforMoves of
        Nothing -> Nothing
        Just t_formoves ->
            let started = fromMaybe now lastTimeMoved
            in  countdown now (addUTCTime t_formoves started)
    Ready -> case timeBeforeStart of
        Nothing            -> Nothing
        Just t_beforestart -> countdown now (addUTCTime t_beforestart createdOn)
    InPreparation -> Nothing

checkAllTimes :: MonadIO m => BotConfig -> m ()
checkAllTimes env = do
    let tok = token env
        mvar = memstore env
        p = pipe env
    liftIO $ print "checkAllTimes: Started..."
    verdict <- liftIO $ isEmptyMVar mvar
    if verdict then pure () else liftIO $ do
        now <- getCurrentTime
        hmap <- readMVar mvar
        let cid_alert_updated = foldl' (\acc g -> case check g now of Just updated_game -> updated_game : acc; Nothing -> acc) [] hmap
            just_updated = map (\(a,_,b) -> (a,b)) cid_alert_updated
            merged_new_old = HMS.union (HMS.fromList just_updated) hmap
        mapConcurrently_ (\(cid, alert, new_state) ->
            let reply = if lastSidePlayed new_state == Just W then "White" else "Black"
                thenLost = sendMessage cid (renderAlert Lost `T.append` reply `T.append` " has just lost.") tok
                thenTimeIsRunning = sendMessage cid (renderAlert alert `T.append` reply `T.append` " to make their move, hurry up!") tok
            in  case alert of Lost -> thenLost; _ -> thenTimeIsRunning
            ) cid_alert_updated
        modifyMVar_ mvar (\_ -> pure merged_new_old)
        liftIO $ print "checkAllTimes: Sleeping for 5 minutes."
        threadDelay 300000000
    where
        getNotification game alert =
            let (notified_w, notified_b) = notified game
            in  if lastSidePlayed game == Just W then (B, "Black", (notified_w, alert : notified_b)) else (W, "White", (alert : notified_w, notified_b))
        colourAlerts game W = fst $ notified game
        colourAlerts game B = snd $ notified game
        check g now = case checkGameTime g now of
            Just Lost ->
                let (name, reason) = if lastSidePlayed g == Just W then ("White", BlackOvertime) else ("Black", WhiteOvertime)
                    notification =
                        let (notified_w, notified_b) = notified g
                        in  if lastSidePlayed g == Just W then (notified_w, Lost : notified_b) else (Lost : notified_w, notified_b)
                    new_state = g { notified = notification, status = Finished reason }
                in  Just (game_chatid g, Lost, new_state)
            Just H1 ->
                let (toPlay, toPlay_txt, notification) = getNotification g H1
                    new_state = g { notified = notification }
                in  if (isNothing . lastSidePlayed $ g) || H1 `elem` colourAlerts g toPlay then Nothing else
                    Just (game_chatid g, H1, new_state)
            Just M15 ->
                let (toPlay, toPlay_txt, notification) = getNotification g M15
                    new_state = g { notified = notification }
                in  if (isNothing . lastSidePlayed $ g) || M15 `elem` colourAlerts g toPlay then Nothing else
                    Just (game_chatid g, M15, new_state)
            Just M5 ->
                let (toPlay, toPlay_txt, notification) = getNotification g M5
                    new_state = g { notified = notification }
                in  if (isNothing . lastSidePlayed $ g) || M5`elem` colourAlerts g toPlay then Nothing else
                    Just (game_chatid g, M5, new_state)
            Just M1 ->
                let (toPlay, toPlay_txt, notification) = getNotification g M1
                    new_state = g { notified = notification }
                in  if (isNothing . lastSidePlayed $ g) || M1 `elem` colourAlerts g toPlay then Nothing else
                    Just (game_chatid g, M1, new_state)
            Nothing -> Nothing
            Just _ -> Nothing

{-
--

{- TESTS -}

--
test_checkAllGames :: MVar (HMS.HashMap Int64 GameState) -> MVar T.Text -> IO ()
test_checkAllGames mvar logger = do
    verdict <- liftIO $ isEmptyMVar mvar
    print "test_checkAllGames ran once!"
    if verdict then pure () else liftIO $ do
        now <- getCurrentTime
        hmap <- readMVar mvar
        updated <- mapConcurrently (\g -> case checkGameTime g now of
            Just Lost ->
                let (winner, result) = if lastSidePlayed g == Just W then ("White", BlackOvertime) else ("Black", WhiteOvertime)
                    notification = (Just now, Just now)
                in  do
                    modifyMVar_  logger (\l -> pure $ T.append l (renderAlert Lost `T.append` winner `T.append` "just won"))
                    pure (g { notified = notification, status = Finished result } )
            Just H1 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    modifyMVar_  logger (\l -> pure $ T.append l (renderAlert H1 `T.append` toPlay_txt `T.append` " now!"))
                    pure $ g { notified = notification }
            Just M15 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    modifyMVar_  logger (\l -> pure $ T.append l (renderAlert M15 `T.append` toPlay_txt `T.append` " now!"))
                    pure $ g { notified = notification }
            Just M5 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    modifyMVar_  logger (\l -> pure $ T.append l (renderAlert M5 `T.append` toPlay_txt `T.append` " now!"))
                    pure $ g { notified = notification }
            Just M1 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    modifyMVar_  logger (\l -> pure $ T.append l (renderAlert M1 `T.append` toPlay_txt `T.append` " now!"))
                    pure $ g { notified = notification }
            Nothing -> pure g
            Just other_alert -> pure g
            ) hmap
        modifyMVar_ mvar (\_ -> pure updated)
        readMVar logger >>= print
        threadDelay $ 30 * 1000000

test_runCheckAllGames :: MonadIO m => MVar (HMS.HashMap Int64 GameState) -> MVar T.Text -> m ()
test_runCheckAllGames mvar logger = liftIO $ go mvar logger
    where
        caught_txt = "Exception caught"
        restart_txt = "\nRestarting check time worker now..."
        async_ :: MonadIO m => m a -> m ()
        async_ _ = pure ()
        go mvar logger =
            print "Time checked: started" >>
            async_ (forever $ test_checkAllGames mvar logger) `catch` \e -> do
            print $ caught_txt ++ show (e :: SomeException) ++ restart_txt
            go mvar logger

main = do
    now <- getCurrentTime
    let g1 = (initGameState Priv now 1 1) { lastTimeMoved = Just now, timeforMoves = Just $ 20 * 60, status = Started, lastSidePlayed = Just W  }
        g2 = (initGameState Priv now 2 1) { lastTimeMoved = Just now, timeforMoves = Just $ 10 * 60, status = Started, lastSidePlayed = Just B  }
        g3 = (initGameState Priv now 3 1) { lastTimeMoved = Just now, timeforMoves = Just $ 5 * 60, status = Started, lastSidePlayed = Just B  }
        hmap = HMS.insert 3 g3 $ HMS.insert 2 g2 $ HMS.insert (1::Int64) g1 HMS.empty
    logger <- newMVar (mempty :: T.Text)
    mvar <- newMVar hmap
    -- test_runCheckAllGames mvar logger
    async . forever $ test_checkAllGames mvar logger
    withAsync (forever $ threadDelay (35 * 1000000) >> pure ()) wait
-}
