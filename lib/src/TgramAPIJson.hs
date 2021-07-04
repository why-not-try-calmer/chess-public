{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

module TgramAPIJson where

import           Data.Aeson
import           Data.Int      (Int64)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)
import           TgramAPITypes (InlineKeyboardMarkup (InlineKeyboardMarkup),
                                toJsonDrop)

type ChatId = Int64

data OutMessage = SendMessage {
    s_chat_id      :: ChatId,
    s_text         :: T.Text,
    s_reply_markup :: Maybe InlineKeyboardMarkup,
    s_parse_mode   :: Maybe T.Text
    }
    | EditMessage
    {
    e_chat_id      :: ChatId,
    e_text         :: T.Text,
    e_message_id   :: Int,
    e_reply_markup :: Maybe InlineKeyboardMarkup,
    e_parse_mode   :: Maybe T.Text
    }
    deriving (Generic)

instance ToJSON OutMessage where
  toJSON = toJsonDrop 2

