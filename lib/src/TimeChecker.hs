{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TimeChecker where

import           AppTypes
import           Chess                    (renderChessError, startFEN, tryMove)
import           CloudFunctions           (SVGToPNG (SVGToPNG), reqCallCFunc)
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception        (throw)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Foldable            (Foldable (foldl'))
import qualified Data.HashMap.Strict      as HMS
import           Data.List                (sortOn)
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                as T
import           Data.Time
import           Database                 (bsonToGame, openPipe, renderDbError,
                                           saveAllGames, saveGame,
                                           tryRestoreGame)
import           TgramAPITypes            (ChatId)

renderAlert :: Alert -> T.Text
renderAlert H1     = "Less than one hour left for "
renderAlert M30    = "Less than 30 minutes left for "
renderAlert M15    = "Less than 15 minutes left for "
renderAlert M5     = "Less than 5 minutes left for "
renderAlert M1     = "Less than 1 minutes left for "
renderAlert S30    = "Less than 30 seconds left for "
renderAlert S10    = "Less than 10 seconds left for "
renderAlert Lost   = "Time elapsed, the game is over. "
renderAlert Forced = "Time elapsed, playing now your most voted move."

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
    Started -> case timeforMoves of
        Nothing -> Nothing
        Just delay ->
            let started = fromMaybe createdOn lastTimeMoved
            in  countdown now $ addUTCTime delay started
    _ -> Nothing

check :: GameState -> UTCTime -> Maybe (ChatId, Alert, GameState)
check g now = case checkGameTime g now of
    Just Lost ->
        let toPlay = case lastSidePlayed g of Just B -> W; _ -> W
            nextToPlay = case toPlay of W -> B; B -> W
            (name, reason) = if toPlay == W then ("White", BlackOvertime) else ("Black", WhiteOvertime)
        in  case roomType g of
            Pub ->
                let pos = fromMaybe (FEN startFEN) $ lastPosition g
                in  case best_move of
                    Nothing -> getLost
                    Just mv -> case tryMove (Just pos) mv of
                        Left err          -> throw . userError $ T.unpack mv
                        Right (move, fen) -> getForced move fen
                where
                    countScores hmap = HMS.foldl' (\acc (Move mv) -> case HMS.lookup mv acc of
                        Nothing -> HMS.insert mv (1::Int) acc
                        Just counter -> HMS.update (\_ -> Just $ counter + 1) mv acc) HMS.empty hmap
                    pickBest = Just . fst . head . sortOn (Down . snd) . HMS.toList
                    best_move = pickBest . countScores =<< playersVotes g
                    getLost =
                        let notification = if lastSidePlayed g == Just W then (notified_w, Lost : notified_b) else (Lost : notified_w, notified_b)
                                where (notified_w, notified_b) = notified g
                            new_state = g { notified = notification, status = Finished reason }
                        in  Just (game_chatid g, Lost, new_state)
                    getForced move fen = Just (game_chatid g, Forced, g { playersVotes = Nothing, lastSidePlayed = Just nextToPlay, lastMove = Just move, lastPosition = Just fen, lastTimeMoved = Just now })
            Priv ->
                let notification =
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
    where
        getNotification game alert =
            let (notified_w, notified_b) = notified game
            in  if lastSidePlayed game == Just W then (B, "Black", (notified_w, alert : notified_b)) else (W, "White", (alert : notified_w, notified_b))
        colourAlerts game W = fst $ notified game
        colourAlerts game B = snd $ notified game

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
        concurrently_ (modifyMVar_ mvar (\_ -> pure merged_new_old))(saveAllGames p just_updated)
        mapConcurrently_ (\(cid, alert, new_state) ->
            let reply = if lastSidePlayed new_state == Just W then "White" else "Black"
                thenLost = sendMessage cid (renderAlert Lost `T.append` reply `T.append` " won on time. Game over.") tok
                thenForced =
                    let (Move mv) = fromMaybe undefined $ lastMove new_state
                        (FEN fen) = fromMaybe undefined $ lastPosition new_state
                        svg = SVGToPNG cid (Just mv) fen
                    in  concurrently_
                            (sendMessage cid "Time elapsed. Forcing to most voted for move now." tok)
                            (reqCallCFunc tok (endpoint env) svg)
                thenTimeIsRunning = sendMessage cid (renderAlert alert `T.append` reply `T.append` " to make their move, hurry up!") tok
            in  case alert of Lost -> thenLost; Forced -> thenForced; _ -> thenTimeIsRunning
            ) cid_alert_updated
        print "checkAllTimes: Sleeping for 5 minutes."
        threadDelay 300000000
