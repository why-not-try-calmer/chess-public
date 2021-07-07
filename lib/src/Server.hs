{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server (startApp) where


import           AppTypes
import           Chess
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async (async)
import           Control.Exception
import           Control.Monad.Reader
import qualified Data.HashMap.Strict      as HMS
import           Data.Maybe
import qualified Data.Text                as T
import           Database
import           Network.Wai.Handler.Warp
import           QueryParser
import           Servant
import           System.Environment
import           TgramAPITypes
import           TimeChecker

type BotAPI =
    "bot_webhook" :> ReqBody '[JSON] Update :> Post '[JSON] () :<|>
    "warmup" :> Get '[JSON] ()

data Userx

botApi :: Proxy BotAPI
botApi = Proxy

app :: BotConfig -> Application
app = serve botApi . runBotServer

runBotServer :: BotConfig -> Server BotAPI
runBotServer config = hoistServer botApi (flip runReaderT config . runBot) botServer

botServer :: ServerT BotAPI Bot
botServer = bot_webhook :<|> warmup where
    bot_webhook :: Update -> Bot ()
    bot_webhook update = ask >>= \env ->
        case callback_query update of
        Just cb -> handleCallBack cb
        Nothing -> case message update of
            Just msg -> handleMessage msg
            Nothing  -> throwError err400
    warmup :: Bot ()
    warmup = pure ()

handleMessage :: Message -> Bot ()
handleMessage msg =
    let cid = chat_id . chat $ msg
    in ask >>= \env -> case parseCmd msg of
        Left err -> sendMessage cid (renderParseErrors err) (token env)
        Right cmd -> evaluateCmd cmd msg >>= \case
            Left err -> sendMessage cid (renderEvaluateError err) (token env)
            Right _  -> pure ()

handleCallBack :: CallbackQuery -> Bot ()
handleCallBack cbquery = ask >>= \env ->
    let mb_query_data = cq_data cbquery
        tok = token env
        cid = maybe 0 (chat_id . chat) (cq_message cbquery)
    in  case mb_query_data of
        Nothing -> sendMessage cid "Error: got a callbackquery with no data." tok
        Just query_data -> do
            hmap <- liftIO $ readMVar (memstore env)
            case HMS.lookup cid hmap of
                Nothing ->
                    let reply = "Game mentioned in CallbackQueryData not found: " `T.append` (T.pack . show $ cid)
                    in  sendMessage cid reply tok
                Just game -> case status game of
                    Finished result ->
                        let reply = "Game is finished and cannot be changed. The result is: " `T.append` renderResult result
                        in  sendMessage cid reply tok
                    _ -> case parseQuery query_data of
                        Left err ->
                            let reply = renderParseQueryError err
                            in  sendMessage cid reply tok
                        Right query -> evaluateQuery query cbquery >>= \case
                            Left err -> sendMessage cid (renderCbEvaluateError err) tok
                            Right _  -> pure ()

startApp :: IO ()
startApp = do
    mvar <- newMVar HMS.empty
    env <- getEnvironment
    let mongoCreds = initMongCredsFrom . T.pack . fromJust . lookup "MONGO_CREDS" $ env
    openPipe mongoCreds >>= \case
        Left err -> throwIO . userError . T.unpack . renderDbError $ err
        Right pipe ->
            let telegramToken = fromJust . lookup "TELEGRAM_TOKEN" $ env
                gCloudFunc = fromJust . lookup "GCLOUD_ENDPOINT" $ env
                port = read . fromJust . lookup "PORT" $ env
                endpoint = fromJust . lookup "GCLOUD_ENDPOINT" $ env
                config = BotConfig {
                    token = T.pack $ "bot" <> telegramToken,
                    memstore = mvar,
                    endpoint = T.pack endpoint,
                    pipe = pipe,
                    mongoCreds = mongoCreds
                }
            in  do
                _ <- timeWorker config
                run port . app $ config
    where
        timeWorker config = async (forever $ checkAllTimes config) `catch` \e ->
            let caught_txt = "Exception caught"
                restart_txt = "\nRestarting check time worker now..."
            in  do
                print $ caught_txt ++ show (e :: SomeException) ++ restart_txt
                timeWorker config
