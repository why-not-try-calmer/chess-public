{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppTypes where

import           Chess                (GameState (GameState))
import           Control.Concurrent   (MVar)
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict  as HMS
import           Data.Int             (Int64)
import qualified Data.Text            as T
import           Database             (MongoCreds (MongoCreds))
import           Database.MongoDB     (Pipe)
import           Servant.Server       (Handler, ServerError (ServerError))

data BotConfig = BotConfig {
    token      :: T.Text,
    memstore   :: Games,
    endpoint   :: T.Text,
    pipe       :: Pipe,
    mongoCreds :: MongoCreds
}

type Games = MVar (HMS.HashMap Int64 GameState)

newtype Bot a = Bot { runBot :: ReaderT BotConfig Handler a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadError ServerError)

