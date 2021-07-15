{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Keyboards where

import           AppTypes
import qualified Data.HashMap.Strict as HMS
import           Data.List
import           Data.Maybe          (fromMaybe)
import           Data.Ord
import qualified Data.Text           as T
import           TgramAPITypes       (InlineKeyboardButton (ikb_callback_data),
                                      InlineKeyboardMarkup (InlineKeyboardMarkup),
                                      inlineKeyboardButton)

inlineKeyboards :: Keyboard -> InlineKeyboardMarkup
inlineKeyboards PrivK{..} = InlineKeyboardMarkup [side_row, label_time_move, time_move_row1, time_move_row2]
inlineKeyboards PubK{..} = InlineKeyboardMarkup [side_row, label_time_move, time_move_row1, time_move_row2, time_move_row3, start_button]
inlineKeyboards Moves{..} = InlineKeyboardMarkup [moves]

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
        start_button = []
    in  PubK side label_moves time_moves1 time_moves2 time_moves3 start_button

makeStartButton :: [InlineKeyboardButton]
makeStartButton = [(inlineKeyboardButton "START") { ikb_callback_data = Just "start"}]

votingKeyboard :: GameState -> InlineKeyboardMarkup
votingKeyboard GameState{..} =
    let votes = fromMaybe HMS.empty playersVotes
        get_side uid = if uid `elem` whitePlayers then W else B
        voted_moves = HMS.toList $ HMS.foldl' (\acc (Move mv) -> case HMS.lookup mv acc of
            Nothing -> HMS.insert mv (1 :: Int) acc
            Just _  -> HMS.update (\count -> pure (count + 1)) mv acc) HMS.empty votes
        sorted = sortOn (Down . snd) voted_moves
        buttons = map (\(mv, count) -> (inlineKeyboardButton (mv `T.append` " (" `T.append` (T.pack . show $ count) `T.append` ")")) { ikb_callback_data = Just . T.append mv $ "v"  }) sorted
    in  inlineKeyboards $ Moves buttons
