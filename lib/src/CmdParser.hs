{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module CmdParser where


import           AppTypes
import           Chess
import           CloudFunctions
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Reader
import qualified Data.HashMap.Internal.Strict as HMS
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Data.Text.Read               (decimal)
import           Data.Time
import           Database
import           Game.Chess                   (fromFEN, legalPlies)
import           Keyboards                    hiding (B, Side, W)
import           Network.HTTP.Req
import           TgramAPIJson
import           TgramAPITypes

newtype NominateReferee = Referee T.Text
newtype AMove = AMove T.Text
data Assignment = ToBlack T.Text | ToWhite T.Text
newtype Remove = RemoveCmd T.Text
data Cmd = AutoStart | Start RoomType | Setup (TimeUnit, Side) | JoinTeam Side | NominateReferee T.Text | SubmitMove AMove | SubmitPreMoves [AMove] | Info | Resign | NominateSub T.Text | Assign Assignment | Remove T.Text | Abort | Restore
data ParseError = EmptyMessage | NoParse T.Text | ParserNotImplemented T.Text | UserNotFound T.Text | TimeElapsed T.Text | BadArgs T.Text | OnlyGroupSuper
data TimeUnit = Minutes Int | Hours Int | Days Int deriving (Show)

renderParseErrors :: ParseError -> T.Text
renderParseErrors (NoParse imp) = T.append "No available parse for this input: " imp
renderParseErrors EmptyMessage = "This command requires an argument, but the text sent was empty."
renderParseErrors (ParserNotImplemented imp) = T.append "Not implemented: " imp
renderParseErrors (UserNotFound imp) = T.append "Some user(s) could not be found:  " imp
renderParseErrors (TimeElapsed imp) = T.append "The time window has closed already, you cannot do this: " imp
renderParseErrors OnlyGroupSuper = "This bot can only be used in groups and supergroups. Sorry for the inconvenience."
renderParseErrors _ = T.append "Not implemented" mempty

parseCmd :: Message -> Either ParseError Cmd
parseCmd msg = parseCmd . split . contents $ msg
    where
        contents m = fromMaybe
            ("No message! Here is the full update received: " `T.append` (T.pack . show $ m))
            (text m)
        split = T.splitOn " "
        ctype m = chat_type . chat $ m
        parseReply txt =
            let comma_vals = map T.strip . T.splitOn "," $ txt
                lbreak_vals = map T.strip . T.lines $ txt
                relevant = maximum [comma_vals, lbreak_vals]
                getRelevant [] = Left . NoParse $ "Empty split"
                getRelevant [time_interval, colour] = Right (time_interval, colour)
                getRelevant _ = Left . NoParse $ "Too large split"
                Right (time_interval, colour) = getRelevant relevant
                val = decimal $ T.dropEnd 1 time_interval
                unit = T.takeEnd 1 time_interval
                getColour colour
                    |   colour == "b" = Right B
                    |   colour == "w" = Right W
                    |   otherwise = Left . NoParse $ colour
                parseUnit unit v
                    |   unit == "m" = Right $ Minutes v
                    |   unit == "h" = Right $ Hours v
                    |   unit == "d" = Right $ Days v
                    |   otherwise  = Left . NoParse $ "No parse for colour"
            in  when (T.null txt) (Left . NoParse $ T.pack "Empty message. Service message? Here is the full message: " `T.append` (T.pack . show $ msg))
                >>
                if length relevant /= 2 then Left $ NoParse (T.pack $ "Input is too long" ++ show (length relevant))
                else case val of
                Left str    -> Left . NoParse $ T.pack str
                Right (n,_) -> case parseUnit unit n of
                    Left err -> Left err
                    Right duration -> case getColour colour of
                        Left err -> Left err
                        Right c  -> Right $ Setup (duration, c)
        parseCmd [] = Left . NoParse $ mempty
        parseCmd (cw:rest)
            |   cw == "/start" = Right AutoStart
            |   cw == "/new" = case ctype msg of
                    Group      -> Right $ Start Priv
                    Supergroup -> Right $ Start Pub
                    _          -> Left OnlyGroupSuper
            |   cw == "/join" =
                    let [colour] = rest
                        go arg
                            |   arg == "b" = Right . JoinTeam $ B
                            |   arg == "w" = Right . JoinTeam $ W
                            |   otherwise = Left. BadArgs $ "Missing or too many arguments for '/join' "
                    in  go colour
            |   cw == "/referee" =
                    let [appointed] = rest
                    in  Right . NominateReferee $ appointed
            |   cw == "/info" = Right Info
            |   cw == "/castle" = Left . ParserNotImplemented $ "/castle"
            |   cw == "/promote" = Left . ParserNotImplemented $ "/promote"
            |   cw == "/abort" = Right Abort
            |   cw == "/resign" = Right Resign
            |   cw == "/sub" =
                    let [substitute] = rest
                    in  Right . NominateSub $ substitute
            |   cw == "/assign" =
                    let [assigned, colour] = rest
                    in  if colour == "b" then Right . Assign . ToBlack $ assigned else Right . Assign . ToWhite $ assigned
            |   cw == "/remove" =
                    let [toRemove] = rest
                    in  Right . Remove $ toRemove
            |   cw == "/move" = if null rest then Left EmptyMessage else Right . SubmitMove . AMove . head $ rest
            |   cw == "/premove" = Right . SubmitPreMoves $ AMove <$> rest
            |   cw == "/restore" = Right Restore
            |   otherwise  = Left . NoParse . contents $ msg

data EvaluateError = AlreadyGameInChat | GameDoesNotExist | NotPlayer Int | IllegalMove T.Text | NotYourTurn | EvaluateNotImplemented T.Text | GameStatusDoesNotFit | GameNotStarted | GameFinished Result | UnableToRestore T.Text

renderEvaluateError :: EvaluateError -> T.Text
renderEvaluateError AlreadyGameInChat = "There is already a game in this chat, you chess-hungry opportunist."
renderEvaluateError GameDoesNotExist = "There is no game in this chat. Use /new to start a new one."
renderEvaluateError (IllegalMove move) = T.append "That's an illegal move: " move
renderEvaluateError (NotPlayer uid) = T.append "This user is not registered as a player in this game: " . T.pack . show $ uid
renderEvaluateError (EvaluateNotImplemented txt) = txt
renderEvaluateError GameStatusDoesNotFit = "What you're trying to do cannot be done given the current state of the game."
renderEvaluateError GameNotStarted = "The game has not started yet."
renderEvaluateError (GameFinished result) = renderResult result
renderEvaluateError (UnableToRestore txt) = T.append "Unable to restore for this reason: " txt
renderEvaluateError NotYourTurn = "It is not your turn to play."

nowTheGames :: Bot (UTCTime, Games)
nowTheGames = ask >>= \env -> liftIO $ getCurrentTime >>= \now -> pure (now, memstore env)

finishRight :: MonadIO m => m (Either a ())
finishRight = pure (Right ())

evaluateCmd :: Cmd -> Message -> Bot (Either EvaluateError ())
evaluateCmd Abort msg = do
    let reply = "Game aborted successfully."
        cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    (_, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Just game -> case status game of
            Started ->
                let reply = "Cannot abort an ongoing game. Have one player '/resign' instead."
                in  sendMessage cid reply (token env) >> finishRight
            Finished res -> sendMessage cid (renderResult res) (token env) >> finishRight
            _ ->
                let deleted = HMS.delete cid hmap
                in  if uid `notElem` (whitePlayers game ++ blackPlayers game) || uid `notElem` referees game then pure . Left . NotPlayer $ uid else do
                    liftIO $ concurrently_
                        (modifyMVar_ mvar (\_ -> pure deleted))
                        (sendMessage cid reply (token env))
                    >> finishRight
        Nothing -> sendMessage cid (renderEvaluateError GameDoesNotExist) (token env) >> finishRight
evaluateCmd AutoStart msg = do
    let cid = chat_id . chat $ msg
        reply = "This bot can only be used from within a private chat. Create such a chat first, and then invite the bot along with a friend."
    env <- ask
    sendMessage cid reply (token env)
    finishRight
evaluateCmd Info msg = do
    let cid = chat_id . chat $ msg
    env <- ask
    games <- liftIO . readMVar . memstore $ env
    case HMS.lookup cid games of
        Nothing ->
            let reply = "No ongoing game in this chat " `T.append` "(" `T.append` (T.pack . show $ cid) `T.append` ")"
            in  sendMessage cid reply (token env) >> finishRight
        Just game ->
            let fields = [
                    T.pack . show $ createdOn game,
                    case roomType game of Priv -> "Private chat"; Pub -> "Public chat",
                    T.pack . show $ status game,
                    maybe "No last move" (\(Move mv) -> mv) (lastMove game),
                    maybe mempty (\(FEN fen) -> fen) (lastPosition game),
                    maybe mempty (T.pack . show) (timeforMoves game)
                    ]
                reply = T.concat $ zipWith (\val lab -> lab `T.append` ": " `T.append` val `T.append` "\n") fields ["Created on", "Room type", "Game has status", "Last move", "Last position", "Time between moves (in seconds)"]
            in  sendMessage cid reply (token env) >> finishRight
evaluateCmd (Start Pub) msg = pure . Left . EvaluateNotImplemented $ "Games in supergroups not implemented yet. Please use this bot in (private) groups." {- do
    let reply = "Okay, game set to start in this very group chat. There are 5 settings to set: the time window for joining this game, the max. duration between each move, the min. and max. number of players in each team (colour), finally the colour you will play with (if any). Please use the buttons below to set the game up."
        cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    (now, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Just game -> sendMessage cid (renderEvaluateError AlreadyGameInChat) (token env) >> finishRight
        Nothing -> let state = initGameState Pub now uid cid; updated_map = HMS.insert cid state hmap in do
            sendMessageKeyboard cid reply (token env) (inlineKeyboards $ keyboard state)
            liftIO $ modifyMVar_ mvar (\_ -> pure updated_map) >> finishRight -}
evaluateCmd (Start Priv) msg = do
    let reply = "Both players should now pick a colour and the creator of the game should set a max. duration between each move. Please use the buttons below.\n"
        cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    (now, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    let exists = maybe False (\g -> case status g of Finished _ -> False; _ -> True) (HMS.lookup cid hmap)
    if exists then sendMessage cid (renderEvaluateError AlreadyGameInChat) (token env) >> finishRight
    else let state = initGameState Priv now uid cid; updated_map = HMS.insert cid state hmap in do
        sendMessageKeyboard cid reply (token env) (inlineKeyboards $ keyboard state)
        liftIO $ modifyMVar_ mvar (\_ -> pure updated_map)
        finishRight
evaluateCmd Resign msg = do
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    let tok = token env
    (_, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Just game -> case status game of
            Started ->
                let is_player = uid `elem` (whitePlayers game ++ blackPlayers game)
                    has_colour = if uid `elem` whitePlayers game then W else B
                    has_turn = maybe False (has_colour /=) $ lastSidePlayed game
                    new_status = if has_colour == W then Finished WhiteResigned else Finished BlackResigned
                    updated_game = game { status = new_status }
                    updated_map = HMS.update (\_ -> Just updated_game) cid hmap
                    black_or_white = if has_colour == W then "White" else "Black"
                    rev_black_or_white = if has_colour == B then "White" else "Black"
                    reply = black_or_white `T.append` " has resigned! Congratulation to " `T.append` rev_black_or_white
                in  if not (is_player && has_turn) then sendMessage cid "To resign, you must a player in an ongoing game and it must be your turn" tok >> finishRight
                    else liftIO $ mapConcurrently_ id [
                        modifyMVar_ mvar (\_ -> pure updated_map),
                        sendMessage cid reply (token env),
                        saveGame (pipe env) cid updated_game
                        ] >> finishRight
            _ -> sendMessage cid (renderEvaluateError GameStatusDoesNotFit) (token env) >> finishRight
        Nothing -> do
            sendMessage cid (renderEvaluateError GameDoesNotExist) (token env)
            finishRight
evaluateCmd Restore msg = do
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    let mvar = memstore env
    hmap <- liftIO . readMVar $ mvar
    case HMS.lookup cid hmap of
        Just _ -> pure . Left . UnableToRestore $ "Game is fresh in memory already. Nothing to restore."
        Nothing -> tryRestoreGame (pipe env) cid >>= \case
            Left err -> pure . Left . UnableToRestore . renderDbError $ err
            Right game_doc -> case bsonToGame game_doc of
                Nothing -> pure . Left . UnableToRestore $ "Game found but decoding failed."
                Just restored ->
                    let (Just (Move mv)) = lastMove restored
                        (Just (FEN fen)) = lastPosition restored
                        inserted_hmap = HMS.insert cid restored hmap
                        svg = SVGToPNG cid (Just mv) fen
                    in  if uid `notElem` (whitePlayers restored ++ blackPlayers restored) then pure . Left . NotPlayer $ uid else do
                        liftIO $ concurrently_
                            (modifyMVar_ mvar $ \_ -> pure inserted_hmap)
                            (reqCallCFunc (token env) (endpoint env) svg)
                        finishRight
evaluateCmd (SubmitMove (AMove move)) msg =
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    in do
    env <- ask
    let tok = token env
    (now, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Nothing -> pure . Left $ GameDoesNotExist
        Just game -> case status game of
            InPreparation -> pure . Left $ GameNotStarted
            Started ->
                let is_player = uid `elem` (whitePlayers game ++ blackPlayers game)
                    has_colour = if uid `elem` whitePlayers game then W else B
                    doMove = case tryMove (lastPosition game) move of
                        Left err -> sendMessage cid (renderChessError err) tok >> finishRight
                        Right (move@(Move mv), fen@(FEN new_fen)) ->
                            let new_alerts =
                                    let (white_notified, black_notified) = notified game
                                    in  if has_colour == W then (mempty, black_notified) else (white_notified, mempty)
                                new_state = game { notified = new_alerts, lastTimeMoved = Just now, lastMove = Just move, lastPosition = Just fen, lastSidePlayed = Just has_colour }
                                updated_map = HMS.update (\_ -> Just new_state) cid hmap
                                detect_checkmate = case fromFEN (T.unpack new_fen) of
                                    Nothing  -> False
                                    Just pos -> null . legalPlies $ pos
                                (winner_announcement, its_over) = if lastSidePlayed game == Just W then ("White won by checkmate! Good game.", Finished BlackIsMate) else ("Black won by checkmate! Good game.", Finished WhiteIsMate)
                                game_over = new_state { status = its_over}
                            in  liftIO $ mapConcurrently_ id [
                                    modifyMVar_ mvar (\_ -> pure updated_map) >> sendMessage cid ("Thanks for this move: " `T.append` mv) tok,
                                    void $ reqCallCFunc tok (endpoint env) $ SVGToPNG cid (Just mv) new_fen
                                ] >> when detect_checkmate ( do
                                    modifyMVar_ mvar (\_ -> pure $ HMS.update (\_ -> Just game_over) cid hmap)
                                    sendMessage cid winner_announcement tok
                                ) >> saveGame (pipe env) cid (if detect_checkmate then game_over else new_state) >> finishRight
                in  if not is_player then pure . Left . NotPlayer $ uid else case lastSidePlayed game of
                        Nothing -> if has_colour == B then sendMessage cid (renderEvaluateError NotYourTurn) tok >> finishRight else doMove
                        Just W -> if has_colour == W then sendMessage cid (renderEvaluateError NotYourTurn) tok >> finishRight else doMove
                        Just B -> if has_colour == B then sendMessage cid (renderEvaluateError NotYourTurn) tok >> finishRight else doMove
            Finished res -> pure . Left $ GameFinished res
            _ -> pure . Left . EvaluateNotImplemented $ "Ready status not implemented yet."
evaluateCmd _ _ = pure . Left . EvaluateNotImplemented $ "Failed to evaluated this command (not implemented)."

sendMessage :: MonadIO m => ChatId -> T.Text -> T.Text ->  m ()
sendMessage cid msg tok = void . reqSend tok "sendMessage" $ SendMessage cid msg Nothing Nothing

sendMessageKeyboard :: MonadIO m => ChatId -> T.Text -> T.Text -> InlineKeyboardMarkup -> m ()
sendMessageKeyboard cid msg tok keys = void . reqSend tok "sendMessage" $ SendMessage cid msg (Just keys) (Just "Markdown")

editMessage :: MonadIO m => ChatId -> Int -> T.Text -> T.Text -> m ()
editMessage cid mid msg tok = void . reqSend tok "editMessageText" $ EditMessage cid msg mid Nothing (Just "Markdown")

editMessageKeyboard :: MonadIO m => ChatId -> Int -> T.Text -> T.Text -> InlineKeyboardMarkup -> m ()
editMessageKeyboard cid mid msg tok keys = void . reqSend tok "editMessageText" $ EditMessage cid msg mid (Just keys) (Just "Markdown")

reqSend :: MonadIO m => T.Text -> T.Text -> OutMessage -> m IgnoreResponse
reqSend token postMethodName encodedMsg = runReq defaultHttpConfig $ do
    let url = https "api.telegram.org" /: token
        reqUrl = url /: postMethodName
    req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) ignoreResponse mempty
