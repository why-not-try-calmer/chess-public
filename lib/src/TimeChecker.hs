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
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time
data Alert = H1 | M30 | M15 | M5 | M1 | S30 | S10 deriving (Show)

renderAlert :: Alert -> T.Text
renderAlert H1  = "Less than one hour left for "
renderAlert M30 = "Less than 30 minutes left for "
renderAlert M15 = "Less than 15 minutes left for "
renderAlert M5  = "Less than 5 minutes leftfor "
renderAlert M1  = "Less than 1 minutes left for "
renderAlert S30 = "Less than 30 seconds left for "
renderAlert S10 = "Less than 10 seconds left for "

countdown :: UTCTime -> UTCTime -> Maybe Alert
countdown n tg
    |   diffUTCTime tg n <= 1 = throw . userError $ "Boum!"
    |   diffUTCTime tg n < 10 = Just S10
    |   diffUTCTime tg n < 30 = Just S30
    |   diffUTCTime tg n < 60 = Just M1
    |   diffUTCTime tg n < 300 = Just M5
    |   diffUTCTime tg n < 900 = Just M15
    |   diffUTCTime tg n < 1800 = Just M30
    |   diffUTCTime tg  n < 3600 = Just H1
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
            Just H1 ->  case lastSidePlayed g of
                Just W ->
                    let (sideToPlay, sideToPlay_txt) = (B, "Black")
                        notification = (Just now, snd . notified $ g)
                    in  do
                        sendMessage (game_chatid g) (renderAlert H1 `T.append` sideToPlay_txt `T.append` " to make their move, hurry up!") tok
                        pure (g { notified = notification })
                Just B ->
                    let (sideToPlay, sideToPlay_txt) = (W, "White")
                        notification = (fst . notified $ g, Just now)
                    in  do
                        sendMessage (game_chatid g) (renderAlert H1 `T.append` sideToPlay_txt `T.append` " to make their move, hurry up!") tok
                        pure $ g { notified = notification }
                Nothing -> pure g
            Just M5 ->
                let side_txt = maybe ("White" :: T.Text) (\s -> if s == B then ("Black" :: T.Text) else ("White" :: T.Text)) (lastSidePlayed g)
                    whoseTurn = (\s -> if s == B then Just W else Just B) =<< lastSidePlayed g
                in  do
                    sendMessage (game_chatid g) (renderAlert M5 `T.append` "White to make their  first move, hurry up!") tok
                    pure g
            _ -> pure g
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

{-
test :: IO ()
test = do
    mvar <- newMVar 0
    print "Forking worker thread..."
    async . forever $ incIt mvar `catch` \e ->
        print $ "caught this exception: " ++ show (e :: SomeException)
    print "Reading from forked thread continuously..."
    threadDelay 3000000 >> readMVar mvar >>= print . show
    where
        incIt mvar = do
            readMVar mvar >>= \i -> print $ "reading: " ++ show i
            modifyMVar_ mvar (pure . (+1)) >> threadDelay 1000000
-}