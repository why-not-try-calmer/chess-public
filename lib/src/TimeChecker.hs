{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TimeChecker where


import           AppTypes
import           Chess
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import qualified Data.HashMap.Strict      as HMS
import           Data.Int                 (Int64)
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time
data Alert = H1 | M30 | M15 | M5 | M1 | S30 | S10 | Lost deriving (Show)

renderAlert :: Alert -> T.Text
renderAlert H1   = "Less than one hour left for "
renderAlert M30  = "Less than 30 minutes left for "
renderAlert M15  = "Less than 15 minutes left for "
renderAlert M5   = "Less than 5 minutes leftfor "
renderAlert M1   = "Less than 1 minutes left for "
renderAlert S30  = "Less than 30 seconds left for "
renderAlert S10  = "Less than 10 seconds left for "
renderAlert Lost = "Time elapsed, the game is over "

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
    verdict <- liftIO $ isEmptyMVar mvar
    if verdict then pure () else liftIO $ do
        now <- getCurrentTime
        hmap <- readMVar mvar
        updated <- mapConcurrently (\g -> case checkGameTime g now of
            Just Lost ->
                let (winner, result) = if lastSidePlayed g == Just W then ("White", BlackOvertime) else ("Black", WhiteOvertime)
                    notification = (Just now, Just now)
                in  do
                    sendMessage (game_chatid g) (renderAlert Lost `T.append` winner `T.append` "just won") tok
                    pure (g { notified = notification, status = Finished result } )
            Just H1 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    sendMessage (game_chatid g) (renderAlert H1 `T.append` toPlay_txt `T.append` " to make their move, hurry up!") tok
                    pure $ g { notified = notification }
            Just M15 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    sendMessage (game_chatid g) (renderAlert M15 `T.append` toPlay_txt `T.append` " to make their move, hurry up!") tok
                    pure $ g { notified = notification }
            Just M5 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    sendMessage (game_chatid g) (renderAlert M5 `T.append` toPlay_txt `T.append` " to make their move, hurry up!") tok
                    pure $ g { notified = notification }
            Just M1 ->
                let (toPlay, toPlay_txt, notification) = if lastSidePlayed g == Just W then (B, "Black", (fst . notified $ g, Just now)) else (W, "White", (Just now, snd .  notified $ g))
                in  if isNothing $ lastSidePlayed g then pure g else do
                    sendMessage (game_chatid g) (renderAlert M1 `T.append` toPlay_txt `T.append` " to make their move, hurry up!") tok
                    pure $ g { notified = notification }
            Nothing -> pure g
            Just other_alert -> pure g
            ) hmap
        modifyMVar_ mvar (\_ -> pure updated)
        threadDelay 300000000

runCheckAllGames :: MonadIO m => BotConfig -> m ()
runCheckAllGames = liftIO . go
    where
        caught_txt = "Exception caught"
        restart_txt = "\nRestarting check time worker now..."
        async_ :: MonadIO m => m a -> m ()
        async_ _ = pure ()
        go env = async_ (forever $ checkAllTimes env) `catch` \e -> do
            print $ caught_txt ++ show (e :: SomeException) ++ restart_txt
            go env

test :: IO ()
test = do
    now <- getCurrentTime
    let g1 = (initGameState Priv now 1 1) { timeforMoves = Just 1, status = Started, lastSidePlayed = Just W  }
        g2 = (initGameState Priv now 2 2) { timeforMoves = Just $ 10 * 60, status = Started, lastSidePlayed = Just B  }
        hmap' = HMS.insert 2 g2 $ HMS.insert (1::Int64) g1 HMS.empty
    logger <- newMVar (mempty :: T.Text)
    mvar <- newMVar hmap'
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
        Nothing -> do
            print "Nothing found"
            pure g
        Just other_alert -> do
            print $ "other_alert: " ++ show other_alert
            pure g
        ) hmap
    modifyMVar_ mvar (\_ -> pure updated)
    readMVar logger >>= print
    -- threadDelay 1000000