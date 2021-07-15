{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module Database where

import           AppTypes
import           Chess                          (timeFormat, toHumanTime)
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Foldable                  (Foldable (foldl', null))
import qualified Data.HashMap.Strict            as HMS
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Data.Time
import           Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DbTLS
import           Prelude                        hiding (lookup)
import           TgramAPITypes                  (Chat, ChatId, UserId)

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
        time_for_moves = maybe 0 floor timeforMoves :: Int
        max_players = fromMaybe 0 maxPlayers :: Int
        min_players = fromMaybe 0 minPlayers :: Int
        status' = case status of
            Started      -> "Started"
            Finished res -> T.pack . show $ res
            _            -> mempty
        players_votes =
            let swap (a,b) = (b,a)
                as_tuples = maybe mempty (map ((\(Move mv, i) -> (mv, i :: UserId)) . swap) . HMS.toList) playersVotes
            in  foldl' (\acc (l, v) -> (l =: v) : acc) mempty as_tuples
        notified' =
            let (notified_white, notified_black) = let (w, b) = notified in (map (T.pack . show) w, map (T.pack . show) b)
            in  ["notified_white" =: notified_white, "notified_black" =: notified_black]
        room_type = T.pack . show $ roomType
    in  [
            "last_move" =: move,
            "last_position" =: position,
            "last_side_played" =: side_played,
            "last_time_moved" =: last_time_moved,
            "created_on" =: created_on,
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

data DbError = PipeNotAcquired | DbLoginFailed | NoGameFound ChatId | NoGamesFound | DeleteAllFailed | FailedToStoreAll

renderDbError :: DbError -> T.Text
renderDbError PipeNotAcquired = "Failed to open a connection against the database."
renderDbError DbLoginFailed = "Pipe acquired, but login failed."
renderDbError DeleteAllFailed = "Unable to delete all games"
renderDbError (NoGameFound cid) = "This game could not be retrieved from the database: " `T.append` (T.pack . show $ cid)
renderDbError NoGamesFound = "Unable to retrieve any game from 'games' with the 'status' field set to 'Started'"
renderDbError FailedToStoreAll = "Unable to store all the games. Have you checked with TimeChecker?"

saveGame :: MonadIO m => Pipe -> ChatId -> GameState -> m ()
saveGame pipe cid game = runMongo pipe $ upsert (select ["game_chatid" =: cid] "games") (gameToBson game)

saveAllGames :: MonadIO m => Pipe -> [(ChatId, GameState)] -> m (Either DbError ())
saveAllGames pipe updatedGames =
    let selectors = map (\(cid, game) -> (["game_chatid" =: (cid :: ChatId)], gameToBson game, mempty)) updatedGames
    in  runMongo pipe $ updateAll "games" selectors >>=
            \res -> if failed res then pure . Left $ FailedToStoreAll else pure $ Right ()

tryRestoreGame :: MonadIO m => Pipe -> ChatId -> m (Either DbError Document)
tryRestoreGame pipe cid = runMongo pipe $ findOne (select ["game_chatid" =: cid] "games") >>= \case
    Just doc -> pure . Right $ doc
    Nothing  -> pure . Left . NoGameFound $ cid

tryRestoreAllGames :: MonadIO m => Pipe -> m (Either DbError [Document])
tryRestoreAllGames pipe = do
    games <- runMongo pipe $ find (select ["status" =: ("Started" :: T.Text) ] "games") >>= rest
    if null games then pure . Left $ NoGamesFound else pure . Right $ games

parseToLocale :: String -> UTCTime
parseToLocale = parseTimeOrError True defaultTimeLocale timeFormat

bsonToGame :: Document -> Maybe GameState
bsonToGame doc =
    let last_move = (\s -> if T.null s then Nothing else Just . Move $ s) =<< (lookup "last_move" doc :: Maybe T.Text)
        last_position = (\s -> if T.null s then Nothing else Just . FEN $ s) =<< (lookup "last_position" doc :: Maybe T.Text)
        last_side_played = (\s -> if T.null s then Nothing else if s == "B" then Just B else Just W) =<< (lookup "last_side_played" doc :: Maybe T.Text)
        last_time_moved = (\date_str -> if T.null date_str then Nothing else Just . parseToLocale . T.unpack $ date_str) =<< lookup "last_time_moved" doc
        created_on = case lookup "created_on" doc :: Maybe T.Text of
            Just txt -> parseToLocale . T.unpack $ txt
            Nothing  -> parseToLocale ""
        time_for_moves =
            let t = lookup "time_for_moves" doc :: Maybe Int
            in  secondsToNominalDiffTime . fromIntegral <$> t
        max_players = lookup "max_players" doc :: Maybe Int
        min_players = lookup "min_players" doc :: Maybe Int
        white_players = fromMaybe mempty (lookup "white_players" doc :: Maybe [UserId])
        black_players = fromMaybe mempty (lookup "black_players" doc :: Maybe [UserId])
        referees = fromMaybe mempty (lookup "referees" doc :: Maybe [UserId])
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
                    let (uid, mv) = (typed . value $ field :: UserId, label field :: T.Text)
                    in  (uid, Move mv)) subdoc
            in  if null fields then Nothing else Just $ HMS.fromList fields
        game_chatid = fromMaybe undefined (lookup "game_chatid" doc :: Maybe ChatId)
        notified =
            let subdoc = fromMaybe undefined (lookup "notified" doc :: Maybe Document)
                notified_w = fromMaybe mempty (lookup "notified_white" subdoc :: Maybe [T.Text])
                notified_b = fromMaybe mempty (lookup "notified_black" subdoc :: Maybe [T.Text])
                parseAlert v
                    | v == "H1" = Just H1
                    | v == "M30" = Just M30
                    | v == "M15" = Just M15
                    | v == "M5" = Just M5
                    | v == "M1" = Just M1
                    | v == "S30" = Just S30
                    | v == "S10" = Just S10
                    | v == "Lost" = Just Lost
                    | otherwise = Nothing
            in  (fromMaybe mempty $ traverse parseAlert notified_w, fromMaybe mempty $ traverse parseAlert notified_b)
        room_type = maybe undefined (\v -> if v == "Priv" then Priv else Pub) (lookup "room_type" doc :: Maybe T.Text)
        keyboard = case room_type of Priv -> PrivK mempty mempty mempty mempty; Pub -> PubK mempty mempty mempty mempty mempty mempty
    in  Just $ GameState last_move last_position last_side_played last_time_moved created_on time_for_moves max_players min_players white_players black_players referees status players_votes game_chatid notified keyboard room_type

eitherDbErrorOrEffect :: Applicative f => WriteResult -> f (Either DbError ())
eitherDbErrorOrEffect res = if failed res then pure . Left $ DeleteAllFailed else pure . Right $ ()

removeAllGames :: MonadIO m => Pipe -> m (Either DbError ())
removeAllGames pipe = runMongo pipe $ deleteAll "games" [([], mempty)] >>= eitherDbErrorOrEffect

removeThisGame :: MonadIO m => Pipe -> ChatId -> m ()
removeThisGame pipe cid = runMongo pipe $ deleteOne (select ["game_chatid" =: cid] "games")
