{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QueryParser where

import           AppTypes
import           Chess
import           CloudFunctions
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Reader
import qualified Data.HashMap.Strict      as HMS
import           Data.Int                 (Int64)
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Text.Read           (decimal)
import           Keyboards                hiding (B, Side, W)
import           TgramAPITypes
data ParseQueryError = NoTimer T.Text | NoUnit T.Text deriving (Show)

renderParseQueryError :: ParseQueryError -> T.Text
renderParseQueryError (NoTimer txt) = T.append "No timer found: " txt
renderParseQueryError (NoUnit txt)  = T.append "No unit found: " txt

data Query = Join Side | MoveTimer TimeUnit | StartTimer TimeUnit deriving (Show)

parseQuery :: T.Text -> Either ParseQueryError Query
parseQuery query
    | headq == "b" = Right $ Join B
    | headq == "w" = Right $ Join W
    | headq == "s" =
        let timer t v
                | t == "m" = Right $ Minutes v
                | t == "h" = Right $ Hours v
                | t == "d" = Right $ Days v
                | otherwise = Left . NoUnit $ "found s, but failed failed to parse on the rest" `T.append` query
            val = case decimal bodyq of
                Left _       -> 0
                Right (n, _) -> n :: Int
            eitherparsed = timer tailq val
        in  case eitherparsed of
            Left _  -> Left . NoUnit $ "found s, but no Right branch stemming from " `T.append` query
            Right tu -> Right . StartTimer $ tu
    | tailq == "m" = parseTimer Minutes query
    | tailq == "h" = parseTimer Hours query
    | tailq == "d" = parseTimer Days query
    | otherwise = Left . NoTimer $ "didn't find s, but failed to parse the rest: " `T.append` query
    where
        headq = T.take 1 query
        bodyq = T.dropEnd 1 . T.drop 1 $ query
        tailq = T.takeEnd 1 query
        parseTimer timer query = case decimal . T.dropEnd 1 $ query of
            Left _       -> Left . NoTimer $ "failed to parse on " `T.append` query
            Right (v, _) -> Right . MoveTimer . timer $ v

updateKeyboardFor :: Keyboard -> Query -> Keyboard
updateKeyboardFor keys@PrivK{..} query = case query of
    Join _ -> keys
    MoveTimer timer ->
        let text x = "Time between moves set to " `T.append` (T.pack . show $ x)
        in  case timer of
            Minutes ms -> keys {
                label_time_move = mempty, --[(inlineKeyboardButton (text ms `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty
            }
            Hours hs   -> keys {
                label_time_move = mempty, --[(inlineKeyboardButton (text hs `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty
            }
            Days ds    -> keys {
                label_time_move = mempty, -- [(inlineKeyboardButton (text ds `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty
            }
    _ -> keys
updateKeyboardFor keys@PubK{..} query = case query of
    Join _ -> keys
    MoveTimer timer ->
        let text x = "Time between moves set to " `T.append` (T.pack . show $ x)
        in  case timer of
            Minutes ms -> keys {
                label_time_move = [(inlineKeyboardButton (text ms `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty,
                time_move_row3 = mempty
            }
            Hours hs  -> keys {
                label_time_move = [(inlineKeyboardButton (text hs `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty,
                time_move_row3 = mempty
            }
            Days ds -> keys {
                label_time_move = [(inlineKeyboardButton (text ds `T.append` " minutes.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty,
                time_move_row3 = mempty
            }
    StartTimer timer ->
        let text x = "Time before start set to " `T.append` (T.pack . show $ x)
        in  case timer of
            Minutes ms -> keys {
                label_start = [(inlineKeyboardButton (text ms)) { ikb_callback_data = Just "_" } ],
                time_start_row = mempty
            }
            Hours hs   -> keys {
                label_start = [(inlineKeyboardButton (text hs)) { ikb_callback_data = Just "_" } ],
                time_start_row = mempty
            }
            Days ds    -> keys {
                label_start = [(inlineKeyboardButton (text ds)) { ikb_callback_data = Just "_" } ],
                time_start_row = mempty
            }

data EvalQueryError = NoGame T.Text | NotReferee T.Text | Occupied T.Text | AlreadyThere T.Text
newtype UserActionOk = Joined [Int]

tryJoining :: Int -> [Int] -> T.Text -> Either EvalQueryError UserActionOk
tryJoining player dest txt
    | player `elem` dest = Left . AlreadyThere $ describe
    | not (null dest) = Left . Occupied $ describe
    | otherwise =  Right $ Joined (player : dest)
    where describe = T.append (T.pack . show $ player) " tried to join " `T.append` txt

renderCbEvaluateError :: EvalQueryError -> T.Text
renderCbEvaluateError (NoGame txt) = T.append "Found this EvalQueryError: " txt
renderCbEvaluateError (NotReferee txt) = T.append "Cannot perform this operation if not game admin." txt
renderCbEvaluateError (Occupied txt) = T.append txt " but the player of the other colour switches team first."
renderCbEvaluateError (AlreadyThere txt) = T.append txt " but you cannot join the same team twice."

startOnCompleteSettings :: HMS.HashMap Int64 GameState -> Int64 -> Int -> Bot (Either a ())
startOnCompleteSettings hmap cid mid = do
    env <- ask
    let tok = token env
        svg = SVGToPNG cid Nothing startFEN
        game = hmap HMS.! cid
        roomtype = roomType game
        mvar = memstore env
    case roomtype of
        Priv ->
            let w = 1 == length (whitePlayers game)
                b = 1 == length (blackPlayers game)
                updated_map = HMS.update (\_ -> Just game { status = Started }) cid hmap
            in  if w && b && hasJust (timeforMoves game)
                then liftIO (concurrently_
                    (sendMessage cid "Game is starting in a few. If at any point you fail to resume your game, just use the /restore command. Gl & hf!" tok >> modifyMVar_ mvar  (\_ -> pure updated_map))
                    (reqCallCFunc (token env) (endpoint env) svg)
                    ) >> finish
                else finish
        Pub -> finish
    where
        finish = pure (Right ())
        hasJust (Just _) = True
        hasJust _        = False

evaluateQuery :: Query -> CallbackQuery -> Bot (Either EvalQueryError ())
evaluateQuery query cbquery = ask >>= \env -> do
    let tok = token env
        cid = maybe 0 (chat_id . chat) (cq_message cbquery)
        mid = maybe 0 message_id (cq_message cbquery)
        uid = user_id $ cq_from cbquery
        getText (Just m) = fromMaybe "Empty message" (text m)
        getText Nothing  = "No message carrier"
        message = getText . cq_message $ cbquery
        gamestate = memstore env
        end_point = endpoint env
        p = pipe env
    hmap <- liftIO $ readMVar gamestate
    case HMS.lookup cid hmap of
        Nothing -> pure . Left . NoGame . T.pack . show $ cid
        Just game -> let roomtype = roomType game in case query of
            Join W ->
                let new_keyboard = updateKeyboardFor (keyboard game) (Join W)
                in  case tryJoining uid (whitePlayers game) "white" of
                    Left err -> pure . Left $ err
                    Right (Joined white) ->
                        let updated_hmap = HMS.update (\_ -> Just game { whitePlayers = white, keyboard = new_keyboard }) cid hmap
                            msg = (message `T.append` "\n" `T.append` T.append "Just joined White: " (T.pack . show $ uid))
                        in  do
                            liftIO (modifyMVar_ gamestate (\_-> pure updated_hmap))
                            editMessageKeyboard cid mid msg tok (inlineKeyboards new_keyboard)
                            startOnCompleteSettings updated_hmap cid mid
            Join B ->
                let new_keyboard = updateKeyboardFor (keyboard game) (Join B)
                in  case tryJoining uid (blackPlayers game) "black" of
                    Left err -> pure . Left $ err
                    Right (Joined black) ->
                        let updated_hmap = HMS.update (\_ -> Just game { blackPlayers = black, keyboard = new_keyboard }) cid hmap
                            msg = (message `T.append` "\n" `T.append` T.append "Just joined Black: " (T.pack . show $ uid))
                        in  do
                            liftIO (modifyMVar_ gamestate (\_-> pure updated_hmap))
                            editMessageKeyboard cid mid msg tok $ inlineKeyboards new_keyboard
                            startOnCompleteSettings updated_hmap cid mid
            MoveTimer t ->
                let new_keyboard = updateKeyboardFor (keyboard game) (MoveTimer t)
                in  if uid `notElem` referees game then pure . Left . NotReferee $ T.pack . show $ uid else case t of
                Minutes ms    ->
                    let timecontrol = Just . convert $ ms * 60
                        msg = "Move timer set to " `T.append` (T.pack . show $ ms) `T.append` " minutes."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves =  timecontrol}) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
                Hours hs    ->
                    let timecontrol = Just . convert $ hs * 3600
                        msg = "Move timer set to " `T.append` (T.pack . show $ hs) `T.append` " hours."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
                Days ds     ->
                    let timecontrol = Just . convert $ ds * 3600 * 24
                        msg = "Move timer set to " `T.append` (T.pack . show $ ds) `T.append` " days."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
            StartTimer t ->
                let new_keyboard = updateKeyboardFor (keyboard game) (StartTimer t)
                in  if uid `notElem` referees game then pure . Left . NotReferee $ T.pack . show $ uid else case t of
                Minutes ms ->
                    let timecontrol = Just . convert $ ms * 60
                        msg = "Start timer set to " `T.append` (T.pack . show $ ms) `T.append` " minutes."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeBeforeStart = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
                Hours hs   ->
                    let timecontrol = Just . convert $ hs * 3600
                        msg = "Start timer set to " `T.append` (T.pack . show $ hs) `T.append` " minutes."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeBeforeStart = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
                Days ds    ->
                    let timecontrol = Just . convert $ ds * 3600 * 24
                        msg = "Start timer set to " `T.append` (T.pack . show $ ds) `T.append` " minutes."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeBeforeStart = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings updated_hmap cid mid
    where   convert = fromRational . toRational
