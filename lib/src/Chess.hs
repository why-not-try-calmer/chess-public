{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Chess where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.HashMap.Strict    as HMS
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           Data.Time              (NominalDiffTime, UTCTime (UTCTime),
                                         defaultTimeLocale, formatTime,
                                         getCurrentTime)
import           Game.Chess
import           Keyboards              (Keyboard, makePrivStartKeyboard,
                                         makePubStartKeyboard)

{-

Setup
- user sets up the game, becomes referee by default
- user sets up:
    - a time window for joining
    - a time step for each move
    - a minimal and optionnally a maximum number of players for each team
- other users join B or W
- when there is the same number of players in each team, the referee can start the game
- if there is a different number any team has an odd number of players

-}

startFEN :: T.Text
startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

newtype Move = Move T.Text deriving (Show)
newtype FEN = FEN T.Text deriving (Show)
newtype BadParse = BadParse T.Text
data IllegalMove = CheckedKing Move | WouldCheckKing Move | LostCastlingRights Move | PieceDoesNotMove Move | PlainIllegal Move
data ChessError = ErrorBadParse BadParse | ErrorIllegalMove IllegalMove
data Result = WhiteIsMate | BlackIsMate | Pat | PatByRepetition | WhiteResigned | BlackResigned | WhiteOvertime | BlackOvertime deriving (Show, Eq)
type RestoreMessageId = Int
data Status = InPreparation | Ready | Started | Finished Result deriving (Show, Eq)
data RoomType = Priv | Pub deriving (Show)

renderStatus :: Status -> T.Text
renderStatus InPreparation = "Game is still in preparation."
renderStatus Ready = "Game is ready, starting now."
renderStatus Started = "Game is ongoing."
renderStatus (Finished res)
    | res == WhiteIsMate  = "White is mate!"
    | res == BlackIsMate = "Black is mate!"
    | res == Pat = "That's a pat (equality, no winner, no loser)"
    | res == PatByRepetition = "You've repeated the same move thrice; this is a pat by repetition."
    | res == WhiteResigned = "White resigned; Black won!"
    | res == BlackResigned = "Black resigned; White won!"
    | otherwise = "unknown status"

data Side = B | W deriving (Eq, Show)

data GameState = GameState {
    lastMove        :: Maybe Move,
    lastPosition    :: Maybe FEN,
    lastSidePlayed  :: Maybe Side,
    lastTimeMoved   :: Maybe UTCTime,
    createdOn       :: UTCTime,
    timeBeforeStart :: Maybe NominalDiffTime, -- only in pub games
    timeforMoves    :: Maybe NominalDiffTime,
    maxPlayers      :: Maybe Int, -- only in pub games
    minPlayers      :: Maybe Int, -- only in pub games
    whitePlayers    :: [Int],
    blackPlayers    :: [Int],
    referees        :: [Int],
    status          :: Status,
    playersVotes    :: Maybe (HMS.HashMap Int Move), -- only in pub games
    game_chatid     :: Int64,
    notified        :: (Maybe UTCTime, Maybe UTCTime), -- White, black
    keyboard        :: Keyboard,
    roomType        :: RoomType
} deriving (Show)

labels :: [T.Text]
labels = [
    "last_move",
    "last_position",
    "last_side_played",
    "last_time_moved",
    "created_on",
    "time_before_start",
    "time_between_moves",
    "max_players",
    "min_players",
    "white_players",
    "black_players",
    "referees",
    "status", --
    "players_votes",
    "game_chatid",
    "notified",
    "keyboard",
    "room_type"
    ]

initGameState :: RoomType -> UTCTime -> Int -> Int64 -> GameState
initGameState Pub now uid cid = GameState Nothing Nothing Nothing Nothing now Nothing Nothing Nothing Nothing mempty mempty [uid] InPreparation Nothing cid (Nothing, Nothing) makePubStartKeyboard Pub
initGameState Priv now uid cid = GameState Nothing Nothing Nothing Nothing now Nothing Nothing Nothing Nothing mempty mempty [uid] InPreparation Nothing cid (Nothing, Nothing) makePrivStartKeyboard Priv

renderChessError :: ChessError -> T.Text
renderChessError (ErrorBadParse (BadParse txt)) = "Unable to parse this move request: " `T.append` txt
renderChessError (ErrorIllegalMove (CheckedKing (Move mov))) = "Illegal! You cannot play " `T.append` mov `T.append` " while your King is checked!"
renderChessError (ErrorIllegalMove (WouldCheckKing (Move mov))) = "Illegal! Playing " `T.append` mov `T.append` " would have your kind checked."
renderChessError (ErrorIllegalMove (LostCastlingRights (Move mov))) = "Illegal! You cannot play " `T.append` mov `T.append` " as you lost your castling rights earlier in the game."
renderChessError (ErrorIllegalMove (PieceDoesNotMove (Move mov))) = "Illegal! The piece cannot accomplish the move :" `T.append` mov `T.append` " . This is just not how chess works."
renderChessError (ErrorIllegalMove (PlainIllegal (Move mov))) = "This move is illegal: " `T.append` mov

renderResult :: Result -> T.Text
renderResult WhiteResigned = "White has just resigned! Congratulations to Black!"
renderResult BlackResigned = "Black has just resigned! Congratulations to White!"
renderResult WhiteIsMate = "White are mate! Congratulations to Black!"
renderResult BlackIsMate = "Black are mate! Congratulations to Black!"
renderResult _ = "This result has not been implemented yet."

tryMove :: Maybe FEN -> T.Text -> Either ChessError (Move, FEN)
{- Maps a position and move into a subsequent position. The position argument should be fed
a Nothing when the game is starting. -}
tryMove Nothing mv = tryMove (Just . FEN $ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") mv
tryMove (Just (FEN fen_t)) mv = case fromFEN (T.unpack fen_t) of
    Just pos -> case fromUCI pos (T.unpack mv) of
        Just legal -> Right (Move mv, FEN . T.pack . toFEN . unsafeDoPly pos $ legal)
        Nothing    -> Left . ErrorIllegalMove . PlainIllegal . Move $ mv
    Nothing -> Left . ErrorBadParse . BadParse $ "Failed to parse this FEN string: " `T.append` fen_t

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

humanTime :: MonadIO m => m T.Text
humanTime = do
    now <- liftIO getCurrentTime
    pure . T.pack . formatTime defaultTimeLocale timeFormat $ now

toHumanTime :: UTCTime -> T.Text
toHumanTime = T.pack . formatTime defaultTimeLocale timeFormat
