{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database where

import           Prelude                        hiding (lookup)
import qualified Data.Text as T
import qualified Database.MongoDB.Transport.Tls as DbTLS
import Control.Exception
import Database.MongoDB
import Control.Monad.IO.Class
import Chess
import Data.Int (Int64)
import Data.Time
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromMaybe)
import Keyboards hiding (W, B)
import Data.Foldable

data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

initMongCredsFrom :: T.Text -> MongoCreds
{-
    FIX ME: MongoAtlas tends to shuffle around the role of 'primary' versus 'secondary' shard
    Make sure to call selectOK to avoid failing to authenticate
-}
initMongCredsFrom txt = 
    let [host_name, db_name, password] = T.splitOn ":" txt
    in  MongoCreds (T.unpack host_name) db_name password

openPipe :: MongoCreds -> IO (Either DbError Pipe)
openPipe creds = try (DbTLS.connect (shard creds) (PortNumber 27017)) >>= \case
    Left e -> let e'= e :: SomeException in do
        print ("Error while trying to connect: " ++ show e)
        pure . Left $ PipeNotAcquired
    Right pipe -> do
        authorized <- access pipe UnconfirmedWrites "admin" $ auth (user creds) (pwd creds)
        if authorized then pure . Right $ pipe
        else pure . Left $ DbLoginFailed

runMongo :: MonadIO m => Pipe -> Action m a -> m a
runMongo pipe = access pipe master "pubchess"

gameToBson :: GameState -> Document
gameToBson g@GameState{..} =
    let move = maybe mempty (\(Move mv) -> mv) lastMove
        position = maybe mempty (\(FEN mv) -> mv) lastPosition
        side_played = maybe mempty (\case B -> "B" :: T.Text ; W -> "W" :: T.Text) lastSidePlayed
        last_time_moved = maybe mempty toHumanTime lastTimeMoved
        created_on = toHumanTime createdOn
        time_before_start = maybe mempty (T.pack . show) timeBeforeStart
        time_for_moves = maybe mempty (T.pack . show) timeforMoves
        max_players = maybe mempty (T.pack . show) maxPlayers
        min_players = maybe mempty (T.pack . show) minPlayers
        status' = case status of
            Started      -> "Started"
            Finished res -> T.pack . show $ res
            _            -> mempty
        players_votes =
            let swap (a,b) = (b,a)
                as_tuples = maybe mempty (map ((\(Move mv, i) -> (mv, i :: Int)) . swap) . HMS.toList) playersVotes
            in  foldl' (\acc (l, v) -> (l =: v) : acc) mempty as_tuples
        notified' =
            let (mbnotified_w, mbnotified_b) = notified
                notified_w = maybe mempty (T.pack . show) mbnotified_w
                notified_b = maybe mempty (T.pack . show) mbnotified_b
            in  ["notified_white" =: notified_w, "notified_black" =: notified_b]
        room_type = T.pack . show $ roomType
    in  [
            "last_move" =: move,
            "last_position" =: position,
            "last_side_played" =: side_played,
            "last_time_moved" =: last_time_moved,
            "created_on" =: created_on,
            "time_before_start" =: time_before_start,
            "time_for_moves" =: time_for_moves,
            "max_players" =: max_players,
            "min_players" =: min_players,
            "white_players" =: whitePlayers,
            "black_players" =: blackPlayers,
            "referees" =: referees,
            "status" =: status',
            "players_votes" =: players_votes,
            "game_chatid" =: game_chatid,
            "notified" =: notified',
            "room_type" =: room_type
        ]

data DbError = PipeNotAcquired | DbLoginFailed | NoGameFound Int64 | DeleteAllFailed

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError DbLoginFailed = "Pipe acquired, but login failed."
renderDbError DeleteAllFailed = "Unable to delete all games"
renderDbError (NoGameFound cid) = "This game could not be retrieved from the database: " `T.append` (T.pack . show $ cid)

saveGame :: MonadIO m => Pipe -> Int64 -> GameState -> m ()
saveGame pipe cid game = runMongo pipe $ upsert (select ["game_chatid" =: cid] "games") (gameToBson game)

tryRestoreGame :: MonadIO m => Pipe -> Int64 -> m (Either DbError Document)
tryRestoreGame pipe cid = runMongo pipe $ findOne (select ["game_chatid" =: cid] "games") >>= \case
    Just doc -> pure . Right $ doc
    Nothing  -> pure . Left . NoGameFound $ cid

parseToLocale :: String -> UTCTime
parseToLocale = parseTimeOrError True defaultTimeLocale timeFormat

bsonToGame :: Document -> Maybe GameState
bsonToGame doc =
    let last_move = Just . Move =<< (lookup "last_move" doc :: Maybe T.Text)
        last_position = Just . FEN =<< (lookup "last_position" doc :: Maybe T.Text)
        last_side_played = (\v -> if v == "B" then Just B else Just W ) =<< (lookup "last_side_played" doc :: Maybe T.Text)
        last_time_moved = (Just . parseToLocale . T.unpack) =<< (lookup "last_time_played" doc :: Maybe T.Text)
        created_on = case lookup "created_on" doc :: Maybe T.Text of
            Just txt -> parseToLocale . T.unpack $ txt
            Nothing  -> parseToLocale ""
        time_before_start = lookup "time_before_start" doc :: Maybe NominalDiffTime
        time_between_moves = lookup "time_between_moves" doc :: Maybe NominalDiffTime
        max_players = lookup "max_players" doc :: Maybe Int
        min_players = lookup "min_players" doc :: Maybe Int
        white_players = fromMaybe mempty (lookup "white_players" doc :: Maybe [Int])
        black_players = fromMaybe mempty (lookup "black_players" doc :: Maybe [Int])
        referees = fromMaybe mempty (lookup "referees" doc :: Maybe [Int])
        status =
            let caseByCase val = if (val :: T.Text) == "Started" then Started else proceedWith val
                proceedWith val
                    |   val == "WhiteIsMate" = Finished WhiteIsMate
                    |   val == "BlackIsMate" = Finished BlackIsMate
                    |   val == "Pat" = Finished Pat
                    |   val == "PatByRepetition" = Finished PatByRepetition
                    |   val == "WhiteResigned" = Finished WhiteResigned
                    |   val == "BlackResigned" = Finished BlackResigned
                    |   otherwise = Started
            in  maybe undefined caseByCase (lookup "status" doc :: Maybe T.Text)
        players_votes =
            let subdoc = fromMaybe undefined (lookup "players_votes" doc :: Maybe Document)
                fields = map (\field ->
                    let (uid, mv) = (typed . value $ field :: Int, label field :: T.Text)
                    in  (uid, Move mv)) subdoc
            in  if null fields then Nothing else Just $ HMS.fromList fields
        game_chatid = fromMaybe undefined (lookup "game_chatid" doc :: Maybe Int64)
        notified =
            let subdoc = fromMaybe undefined (lookup "notified" doc :: Maybe Document)
                notified_w = fromMaybe mempty (lookup "notified_white" subdoc :: Maybe T.Text)
                notified_b = fromMaybe mempty (lookup "notified_black" subdoc :: Maybe T.Text)
                notified_w' = if T.null notified_w then Nothing else Just . parseToLocale . T.unpack $ notified_w
                notified_b' = if T.null notified_b then Nothing else Just . parseToLocale . T.unpack $ notified_b
            in  (notified_b', notified_w')
        room_type = maybe undefined (\v -> if v == "Priv" then Priv else Pub) (lookup "room_type" doc :: Maybe T.Text)
        keyboard = case room_type of Priv -> PrivK mempty mempty mempty mempty; Pub -> PubK mempty mempty mempty mempty mempty mempty mempty
    in  Just $ GameState last_move last_position last_side_played last_time_moved created_on time_before_start time_between_moves max_players min_players white_players black_players referees status players_votes game_chatid notified keyboard room_type

eitherDbErrorOrEffect :: Applicative f => WriteResult -> f (Either DbError ())
eitherDbErrorOrEffect res = if failed res then pure . Left $ DeleteAllFailed else pure . Right $ ()

removeAllGames :: MonadIO m => Pipe -> m (Either DbError ())
removeAllGames pipe = runMongo pipe $ deleteAll "games" [([], mempty)] >>= eitherDbErrorOrEffect

removeThisGame :: MonadIO m => Pipe -> Int64 -> m ()
removeThisGame pipe cid = runMongo pipe $ deleteOne (select ["game_chatid" =: cid] "games")

createMockGame :: Pipe -> IO ()
createMockGame pipe = getCurrentTime >>= \now ->
    let game = initGameState Priv now 1 1
        votes = Just $ HMS.insert (2::Int) (Move "e2e4") HMS.empty
        updated = game { playersVotes =  votes }
    in  saveGame pipe 1 updated

{-
test_restore :: IO ()
test_restore = openPipe >>= \case
    Right pipe -> tryRestoreGame pipe 42 >>= \case
        Left err -> print . renderDbError $ err
        Right doc -> case bsonToGame doc of
            Just gs -> print . show $ gs
            Nothing -> print "Got a document, but could not parse into a valid GameState."
    Left err   -> print . renderDbError $ err

test_save :: IO ()
test_save = getCurrentTime >>= \now -> openPipe >>= \case
    Right pipe ->
        let game = initGameState Priv now 7 42
            game' = game { lastMove = Just $ Move "e6e4", lastPosition = Just $ FEN "bliblablo", whitePlayers = [1,2,3] }
        in  saveGame pipe 42 game'
    Left err -> print . renderDbError $ err
-}