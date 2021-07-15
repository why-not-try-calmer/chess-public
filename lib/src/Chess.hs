{-# LANGUAGE OverloadedStrings #-}

module Chess where

import           AppTypes
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.HashMap.Strict    as HMS
import qualified Data.Text              as T
import           Data.Time              (NominalDiffTime, UTCTime (UTCTime),
                                         defaultTimeLocale, formatTime,
                                         getCurrentTime)
import           Game.Chess
import           Keyboards
import           TgramAPITypes          (ChatId, UserId)

startFEN :: T.Text
startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

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

labels :: [T.Text]
labels = [
    "last_move",
    "last_position",
    "last_side_played",
    "last_time_moved",
    "created_on",
    "time_for_moves",
    "max_players",
    "min_players",
    "white_players",
    "black_players",
    "referees",
    "status",
    "players_votes",
    "game_chatid",
    "notified",
    "keyboard",
    "room_type"
    ]

initGameState :: RoomType -> UTCTime -> UserId -> ChatId -> GameState
initGameState Pub now uid cid = GameState Nothing Nothing Nothing Nothing now Nothing Nothing Nothing mempty mempty [uid] InPreparation Nothing cid mempty makePubStartKeyboard Pub
initGameState Priv now uid cid = GameState Nothing Nothing Nothing Nothing now Nothing Nothing Nothing mempty mempty [uid] InPreparation Nothing cid mempty makePrivStartKeyboard Priv

loadGameState :: GameState -> FEN -> Maybe GameState
loadGameState game fen@(FEN fen_str) = case fromFEN . T.unpack $ fen_str of
    Just pos -> case color pos of
        White -> Just $ game { lastPosition = Just fen, lastSidePlayed = Just B, lastMove = Nothing  }
        Black -> Just $ game { lastPosition = Just fen, lastSidePlayed = Just W, lastMove = Nothing  }
    Nothing -> Nothing

renderChessError :: ChessError -> T.Text
renderChessError (ErrorBadParse (BadParse txt)) = "Unable to parse this move request: " `T.append` txt
renderChessError (ErrorIllegalMove (CheckedKing (Move mov))) = "Illegal! You cannot play " `T.append` mov `T.append` " while your King is checked!"
renderChessError (ErrorIllegalMove (WouldCheckKing (Move mov))) = "Illegal! Playing " `T.append` mov `T.append` " would have your kind checked."
renderChessError (ErrorIllegalMove (LostCastlingRights (Move mov))) = "Illegal! You cannot play " `T.append` mov `T.append` " as you lost your castling rights earlier in the game."
renderChessError (ErrorIllegalMove (PieceDoesNotMove (Move mov))) = "Illegal! The piece cannot accomplish the move :" `T.append` mov `T.append` " . This is just not how chess works."
renderChessError (ErrorIllegalMove (PlainIllegal (Move mov))) = "This move is illegal: " `T.append` mov
renderChessError (NeedPromotion mv_str) = "To make this move legal, please append 'q', 'r', 'b' or 'n' to you move to promote in moving. Example: /move " `T.append` mv_str `T.append` "q to promote the pawn to a Queen."

renderResult :: Result -> T.Text
renderResult Aborted = "The game was aborted."
renderResult WhiteResigned = "White has just resigned! Congratulations to Black!"
renderResult BlackResigned = "Black has just resigned! Congratulations to White!"
renderResult WhiteIsMate = "White are mate! Congratulations to Black!"
renderResult BlackIsMate = "Black are mate! Congratulations to Black!"
renderResult _ = "This result has not been implemented yet."

tryMove :: Maybe FEN -> T.Text -> Either ChessError (Move, FEN)
tryMove Nothing mv = tryMove (Just . FEN $ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") mv
tryMove (Just (FEN fen_t)) mv = case fromFEN (T.unpack fen_t) of
    Just pos -> case fromUCI pos (T.unpack mv) of
        Just legal -> Right (Move mv, FEN . T.pack . toFEN . unsafeDoPly pos $ legal)
        Nothing ->
            let legal = map (T.pack . show) (legalPlies pos)
            in  if any (T.isPrefixOf mv) legal then Left . NeedPromotion $ fen_t else Left . ErrorIllegalMove . PlainIllegal . Move $ mv
    Nothing -> Left . ErrorBadParse . BadParse $ "Failed to parse this FEN string: " `T.append` fen_t

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

humanTime :: MonadIO m => m T.Text
humanTime = do
    now <- liftIO getCurrentTime
    pure . T.pack . formatTime defaultTimeLocale timeFormat $ now

toHumanTime :: UTCTime -> T.Text
toHumanTime = T.pack . formatTime defaultTimeLocale timeFormat
