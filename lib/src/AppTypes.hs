{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module AppTypes where

import           Control.Concurrent   (MVar)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as HMS
import qualified Data.Text            as T
import           Data.Time
import           Database.MongoDB     (Pipe)
import           Servant.Server       (Handler, ServerError (ServerError))
import           TgramAPITypes        (ChatId, InlineKeyboardButton, UserId)

data BotConfig = BotConfig {
    token      :: T.Text,
    memstore   :: Games,
    endpoint   :: T.Text,
    pipe       :: Pipe,
    mongoCreds :: MongoCreds
}

data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

type Games = MVar (HMS.HashMap ChatId GameState)

newtype Bot a = Bot { runBot :: ReaderT BotConfig Handler a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadError ServerError)

data Side = W | B deriving (Show, Eq)
newtype Move = Move T.Text deriving (Show, Eq)
newtype FEN = FEN T.Text deriving (Show)
newtype BadParse = BadParse T.Text
data IllegalMove = CheckedKing Move | WouldCheckKing Move | LostCastlingRights Move | PieceDoesNotMove Move | PlainIllegal Move
data ChessError = ErrorBadParse BadParse | ErrorIllegalMove IllegalMove | NeedPromotion T.Text
data Result = Aborted | WhiteIsMate | BlackIsMate | Pat | PatByRepetition | WhiteResigned | BlackResigned | WhiteOvertime | BlackOvertime deriving (Show, Eq)
type RestoreMessageId = Int
data Status = InPreparation | Ready | Started | Finished Result deriving (Show, Eq)
data RoomType = Priv | Pub deriving (Show, Eq)

data GameState = GameState {
    lastMove       :: Maybe Move,
    lastPosition   :: Maybe FEN,
    lastSidePlayed :: Maybe Side,
    lastTimeMoved  :: Maybe UTCTime,
    createdOn      :: UTCTime,
    timeforMoves   :: Maybe NominalDiffTime,
    maxPlayers     :: Maybe Int, -- only in pub games
    minPlayers     :: Maybe Int, -- only in pub games
    whitePlayers   :: [UserId],
    blackPlayers   :: [UserId],
    referees       :: [UserId],
    status         :: Status,
    playersVotes   :: Maybe (HMS.HashMap UserId Move), -- only in pub games
    game_chatid    :: ChatId,
    notified       :: ([Alert], [Alert]), -- alerts for W, B
    keyboard       :: Keyboard,
    roomType       :: RoomType
} deriving (Show)

data Alert = H1 | M30 | M15 | M5 | M1 | S30 | S10 | Lost | Forced deriving (Eq, Show)

data Keyboard =
    PrivK {
    side_row        :: [InlineKeyboardButton],
    label_time_move :: [InlineKeyboardButton],
    time_move_row1  :: [InlineKeyboardButton],
    time_move_row2  :: [InlineKeyboardButton]
    } |
    PubK {
    side_row        :: [InlineKeyboardButton],
    label_time_move :: [InlineKeyboardButton],
    time_move_row1  :: [InlineKeyboardButton],
    time_move_row2  :: [InlineKeyboardButton],
    time_move_row3  :: [InlineKeyboardButton],
    start_button    :: [InlineKeyboardButton]
    } |
    Moves { moves :: [InlineKeyboardButton] }
    deriving (Show)
