{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module CloudFunctions where
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Network.HTTP.Req
import           TgramAPITypes          (ChatId, toJsonDrop)

data SVGToPNG = SVGToPNG {
    svg_chat_id :: ChatId,
    svg_move    :: Maybe T.Text,
    svg_fen     :: T.Text
} deriving (Show, Generic)

instance ToJSON SVGToPNG where
  toJSON = toJsonDrop 4

reqCallCFunc :: MonadIO m => T.Text -> T.Text -> SVGToPNG -> m IgnoreResponse
reqCallCFunc token endpoint svg = runReq defaultHttpConfig $ do
    let url = https endpoint /: "svg"
        reqUrl = url /: "sendPhoto"
    req Network.HTTP.Req.POST reqUrl (ReqBodyJson svg) ignoreResponse mempty
