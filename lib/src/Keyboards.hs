{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData #-}

module Keyboards where

import           Data.Foldable (Foldable (foldl'))
import           Data.Int      (Int64)
import qualified Data.Text     as T
import           TgramAPITypes (InlineKeyboardButton (InlineKeyboardButton, ikb_callback_data),
                                InlineKeyboardMarkup (InlineKeyboardMarkup, inline_keyboard),
                                inlineKeyboardButton)

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
    label_start     :: [InlineKeyboardButton],
    time_start_row  :: [InlineKeyboardButton]
} deriving (Show)

inlineKeyboards :: Keyboard -> InlineKeyboardMarkup
inlineKeyboards PrivK{..} = InlineKeyboardMarkup [side_row, label_time_move, time_move_row1, time_move_row2]
inlineKeyboards PubK{..} = InlineKeyboardMarkup [side_row, label_time_move, time_move_row1, time_move_row2, time_move_row3, label_start, time_start_row]

makePrivStartKeyboard :: Keyboard
makePrivStartKeyboard =
    let side = [(inlineKeyboardButton "Play as White") { ikb_callback_data = Just "w" }, (inlineKeyboardButton "Play as Black") { ikb_callback_data = Just "b" }]
        label = [(inlineKeyboardButton "~ Max. time per move ~") { ikb_callback_data = Just "_" }]
        time1 = [
            (inlineKeyboardButton "1 min") { ikb_callback_data = Just "1m"},
            (inlineKeyboardButton "5 min") { ikb_callback_data = Just "5m"},
            (inlineKeyboardButton "30 min") { ikb_callback_data = Just "30m"}
            ]
        time2 = [
            (inlineKeyboardButton "1 hour") { ikb_callback_data = Just "1h"},
            (inlineKeyboardButton "6 hours") { ikb_callback_data = Just "6h"},
            (inlineKeyboardButton "12 hours") { ikb_callback_data = Just "12h"}
            ]
    in  PrivK side label time1 time2

makePubStartKeyboard :: Keyboard
makePubStartKeyboard =
    let side = [
            (inlineKeyboardButton "Join Team White") { ikb_callback_data = Just "w" },
            (inlineKeyboardButton "Join Team Black") { ikb_callback_data = Just "b" }
            ]
        label_moves = [(inlineKeyboardButton "~ Max. time per move ~") { ikb_callback_data = Just "_" }]
        time_moves1 = [
            (inlineKeyboardButton "1 min") { ikb_callback_data = Just "1m"},
            (inlineKeyboardButton "5 min") { ikb_callback_data = Just "5m"},
            (inlineKeyboardButton "30 min") { ikb_callback_data = Just "30m"}
            ]
        time_moves2 = [
            (inlineKeyboardButton "1 hour") { ikb_callback_data = Just "1h"},
            (inlineKeyboardButton "6 hours") { ikb_callback_data = Just "6h"},
            (inlineKeyboardButton "12 hours") { ikb_callback_data = Just "12h"}
            ]
        time_moves3 = [
            (inlineKeyboardButton "1 day") { ikb_callback_data = Just "1d"},
            (inlineKeyboardButton "2 days") { ikb_callback_data = Just "2d"},
            (inlineKeyboardButton "3 days") { ikb_callback_data = Just "3d"}
            ]
        label_start = [(inlineKeyboardButton "~ Max. time for joining a team ~") { ikb_callback_data = Just "_"}]
        time_start = [
            (inlineKeyboardButton "5 min") { ikb_callback_data = Just "s5m"},
            (inlineKeyboardButton "30 min") { ikb_callback_data = Just "s30m"},
            (inlineKeyboardButton "1 hour") { ikb_callback_data = Just "s1h"}
            ]
    in  PubK side label_moves time_moves1 time_moves2 time_moves3 label_start time_start

data Side = W | B

data BalanceIssues = TooLow Side | TooHigh Side | Ok

renderBalanceIssues :: BalanceIssues -> T.Text
renderBalanceIssues (TooLow W) = "White does not clear the threshold for minimal nb. of players. "
renderBalanceIssues (TooLow B) = "Black does not clear the threshold for minimal nb. of players. "
renderBalanceIssues (TooHigh W) = "White overcomes the threshold for maximal nb. of players. "
renderBalanceIssues (TooHigh B) = "Black overcomes the threshold for maximal nb. of players. "
renderBalanceIssues Ok = mempty

foldRenderBalanceIssues :: [BalanceIssues] -> T.Text
foldRenderBalanceIssues = foldl' (\acc val -> acc `T.append` renderBalanceIssues val) mempty

findBalance :: ([Int], [Int]) -> Maybe Int -> Maybe Int -> T.Text
findBalance (white, black) (Just minthresh) (Just maxthresh) =
    let summed = length white + length black
        w_min_clear = if length white - minthresh < 0 then TooLow W else Ok
        b_min_clear = if length black - minthresh < 0 then TooLow B else Ok
        w_max_clear = if maxthresh - length white < 0 then TooHigh W else Ok
        b_max_clear = if maxthresh - length black < 0 then TooHigh B else Ok
        issues = [w_min_clear, b_min_clear, w_max_clear, b_max_clear]
    in  foldRenderBalanceIssues issues
findBalance (white, black) Nothing (Just maxthresh) =
    let summed = length white + length black
        w_max_clear = if maxthresh - length white < 0 then TooHigh W else Ok
        b_max_clear = if maxthresh - length black < 0 then TooHigh B else Ok
        issues = [w_max_clear, b_max_clear]
    in  foldRenderBalanceIssues issues
findBalance (white, black) (Just minthresh) Nothing =
    let summed = length white + length black
        w_min_clear = if length white - minthresh < 0 then TooLow W else Ok
        b_min_clear = if length black - minthresh < 0 then TooLow B else Ok
        issues = [w_min_clear, b_min_clear]
    in  foldRenderBalanceIssues issues
findBalance (white, black) Nothing Nothing =
    let w_txt = T.pack . show . length $ white
        b_txt = T.pack . show . length $ black
    in  "Ready to start whenever you want as " `T.append` w_txt `T.append` " vs " `T.append` b_txt

type RestoreGameButton = [InlineKeyboardButton]

makeRestoreGameButton :: Maybe (T.Text, T.Text) -> InlineKeyboardMarkup
makeRestoreGameButton Nothing = InlineKeyboardMarkup [[(inlineKeyboardButton "Restore the game!") { ikb_callback_data = Just mempty }]]
makeRestoreGameButton (Just (fen, mv)) =
    let txt = fen `T.append` ":" `T.append` mv
    in  InlineKeyboardMarkup [[(inlineKeyboardButton "Restore the game!") { ikb_callback_data = Just txt }]]