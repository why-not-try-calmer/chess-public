{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

module TgramAPIJson where

import           Data.Aeson
import           Data.Int      (Int64)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)
import           TgramAPITypes (ChatId,
                                InlineKeyboardMarkup (InlineKeyboardMarkup),
                                toJsonDrop)

data OutMessage = SendMessage {
    s_chat_id      :: ChatId,
    s_text         :: T.Text,
    s_reply_markup :: Maybe InlineKeyboardMarkup
    }
    | EditMessage
    {
    e_chat_id      :: ChatId,
    e_text         :: T.Text,
    e_message_id   :: Int,
    e_reply_markup :: Maybe InlineKeyboardMarkup
    }
    deriving (Generic)

instance ToJSON OutMessage where
  toJSON = toJsonDrop 2

