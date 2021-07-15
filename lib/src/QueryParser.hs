{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module QueryParser where

import           AppTypes
import           Chess
import           CloudFunctions
import           CmdParser
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Reader
import           Data.Foldable            (foldl')
import qualified Data.HashMap.Strict      as HMS
import           Data.Maybe
import           Data.Ord                 (Down (Down), comparing)
import qualified Data.Text                as T
import           Data.Text.Read           (decimal)
import           Database                 (saveGame)
import           Keyboards
import           TgramAPITypes

data Query = Join Side | MoveTimer TimeUnit | Vote Move | ConfirmStart deriving (Show)
data ParseQueryError = NoTimer T.Text | NoUnit T.Text deriving (Show)

renderParseQueryError :: ParseQueryError -> T.Text
renderParseQueryError (NoTimer txt) = T.append "No timer found: " txt
renderParseQueryError (NoUnit txt)  = T.append "No unit found: " txt

parseQuery :: T.Text -> Either ParseQueryError Query
parseQuery query
    | headq == "b" = Right $ Join B
    | headq == "w" = Right $ Join W
    | tailq == "v" = Right . Vote . Move $ T.dropEnd 1 query
    | tailq == "m" = parseTimer Minutes query
    | tailq == "h" = parseTimer Hours query
    | tailq == "d" = parseTimer Days query
    | query == "start" = Right ConfirmStart
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
                label_time_move = mempty,
                time_move_row1 = mempty,
                time_move_row2 = mempty
            }
            Hours hs   -> keys {
                label_time_move = mempty,
                time_move_row1 = mempty,
                time_move_row2 = mempty
            }
            Days ds    -> keys {
                label_time_move = mempty,
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
                label_time_move = [(inlineKeyboardButton (text hs `T.append` " hours.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty,
                time_move_row3 = mempty
            }
            Days ds -> keys {
                label_time_move = [(inlineKeyboardButton (text ds `T.append` " days.")) { ikb_callback_data = Just "_" } ],
                time_move_row1 = mempty,
                time_move_row2 = mempty,
                time_move_row3 = mempty
            }
    ConfirmStart -> keys { start_button = makeStartButton }
    _ -> keys
updateKeyboardFor keys _ = keys

data EvalQueryError = NoGame | NotReferee | Occupied T.Text | AlreadyThere T.Text | MissingMessage | MissingVotes
newtype UserActionOk = Joined [UserId]

tryJoining :: RoomType -> UserId -> [UserId] -> T.Text -> Either EvalQueryError UserActionOk
tryJoining Priv player dest txt
    | player `elem` dest = Left . AlreadyThere $ describe
    | not (null dest) = Left . Occupied $ describe
    | otherwise =  Right $ Joined (player : dest)
    where describe = T.append (T.pack . show $ player) " tried to join " `T.append` txt
tryJoining Pub player dest txt = Right $ Joined (player : dest)

renderCbEvaluateError :: EvalQueryError -> T.Text
renderCbEvaluateError NoGame = "No running game in this chat to register this command against."
renderCbEvaluateError NotReferee = "Only game admins can perform this action."
renderCbEvaluateError (Occupied txt) = T.append txt " but this colour already has a player. Have the player switch team first."
renderCbEvaluateError (AlreadyThere txt) = T.append txt " but you cannot join the same team twice."
renderCbEvaluateError MissingMessage = "Unable to identify the message references by this callback query."
renderCbEvaluateError MissingVotes = "Unable to access vote ledger. Please try again."

startOnCompleteSettings :: T.Text -> HMS.HashMap ChatId GameState -> ChatId -> Int -> Bot (Either a ())
startOnCompleteSettings msg hmap cid mid = do
    env <- ask
    let tok = token env
        svg = SVGToPNG cid Nothing startFEN
        game = hmap HMS.! cid
        ty = roomType game
        mvar = memstore env
        updated_map = HMS.update (\_ -> Just game { status = Ready }) cid hmap
        reply = if ty == Priv then "Game is starting in a few. Gl & hf!" else msg
        new_keyboard = inlineKeyboards . updateKeyboardFor (keyboard game) $ ConfirmStart
        start = case ty of
            Priv -> liftIO $ mapConcurrently_ id [
                modifyMVar_ mvar  (\_ -> pure updated_map),
                sendMessage cid reply tok,
                reqCallCFunc (token env) (endpoint env) svg >> pure ()
                ] >> finishRight
            Pub -> liftIO $ mapConcurrently_ id [
                modifyMVar_ mvar  (\_ -> pure updated_map),
                editMessageKeyboard cid mid reply tok new_keyboard
                ] >> finishRight
    case ty of
        Priv ->
            let w = 1 == length (whitePlayers game)
                b = 1 == length (blackPlayers game)
            in  if w && b && hasJust (timeforMoves game)
                then start
                else cancel
        Pub ->
            if null (whitePlayers game) || null (blackPlayers game) || not (hasJust . timeforMoves $ game)
            then cancel
            else start
    where
        cancel = pure (Right ())
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
        message_txt = getText . cq_message $ cbquery
        gamestate = memstore env
        end_point = endpoint env
        p = pipe env
    hmap <- liftIO $ readMVar gamestate
    case HMS.lookup cid hmap of
        Nothing -> pure . Left $ NoGame
        Just game -> let ty = roomType game in case query of
            Join W -> case tryJoining ty uid (whitePlayers game) "white" of
                Left err -> pure . Left $ err
                Right (Joined white) ->
                    let updated_hmap = HMS.update (\_ -> Just game { whitePlayers = white }) cid hmap
                        msg = (message_txt `T.append` "\n" `T.append` T.append "Just joined White: " (T.pack . show $ uid))
                    in  do
                        liftIO (modifyMVar_ gamestate (\_-> pure updated_hmap))
                        editMessageKeyboard cid mid msg tok (inlineKeyboards $ keyboard game)
                        startOnCompleteSettings message_txt updated_hmap cid mid
            Join B -> case tryJoining ty uid (blackPlayers game) "black" of
                Left err -> pure . Left $ err
                Right (Joined black) ->
                    let updated_hmap = HMS.update (\_ -> Just game { blackPlayers = black }) cid hmap
                        msg = (message_txt `T.append` "\n" `T.append` T.append "Just joined Black: " (T.pack . show $ uid))
                    in  do
                        liftIO (modifyMVar_ gamestate (\_-> pure updated_hmap))
                        editMessageKeyboard cid mid msg tok $ inlineKeyboards (keyboard game)
                        startOnCompleteSettings message_txt updated_hmap cid mid
            MoveTimer t ->
                let new_keyboard = updateKeyboardFor (keyboard game) (MoveTimer t)
                in  if uid `notElem` referees game then pure . Left $ NotReferee else case t of
                Minutes ms    ->
                    let timecontrol = Just . convert $ ms * 60
                        msg = "Move timer set to " `T.append` (T.pack . show $ ms) `T.append` " minutes."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves =  timecontrol}) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message_txt `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings message_txt updated_hmap cid mid
                Hours hs    ->
                    let timecontrol = Just . convert $ hs * 3600
                        msg = "Move timer set to " `T.append` (T.pack . show $ hs) `T.append` " hours."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message_txt `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings message_txt updated_hmap cid mid
                Days ds     ->
                    let timecontrol = Just . convert $ ds * 3600 * 24
                        msg = "Move timer set to " `T.append` (T.pack . show $ ds) `T.append` " days."
                        updated_hmap = HMS.update (\_ -> Just game { keyboard = new_keyboard, timeforMoves = timecontrol }) cid hmap
                    in  do
                        liftIO (modifyMVar_ gamestate (\_ -> pure updated_hmap))
                        editMessageKeyboard cid mid (message_txt `T.append` "\n" `T.append` msg) tok (inlineKeyboards new_keyboard)
                        startOnCompleteSettings message_txt updated_hmap cid mid
            Vote mv@(Move mv_txt) -> case cq_message cbquery of
                Nothing -> pure . Left $ MissingMessage
                Just message ->
                    let mb_hmap = playersVotes game
                    in  case mb_hmap of
                        Nothing -> pure . Left $ MissingVotes
                        Just votes ->
                            let inserted = HMS.insert uid mv votes
                                new_game_state = game { playersVotes = Just inserted }
                                new_games_state = HMS.update (\_ -> pure new_game_state) cid hmap
                            in  if inserted /= votes
                                then liftIO $ mapConcurrently_ id [
                                    modifyMVar_ gamestate (\_ -> pure new_games_state),
                                    saveGame p cid new_game_state,
                                    editMessageKeyboard cid mid message_txt tok $ votingKeyboard new_game_state
                                    ] >> finishRight
                                else sendMessage cid "You can change your vote, but not vote for the same item twice." tok >> finishRight
            ConfirmStart ->
                let svg = SVGToPNG cid Nothing startFEN
                in  if uid `notElem` referees game then pure . Left $ NotReferee else liftIO $ concurrently_
                        (reqCallCFunc (token env) (endpoint env) svg)
                        (sendMessage cid "Game is starting in a few. Gl & hf!" tok)
                    >> finishRight
    where   convert = fromRational . toRational
