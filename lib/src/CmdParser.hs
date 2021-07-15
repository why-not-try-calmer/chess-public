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
import           Data.List
import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (Down (Down))
import qualified Data.Text                    as T
import           Data.Time
import           Database
import           Game.Chess                   (fromFEN, legalPlies)
import           Keyboards
import           Network.HTTP.Req
import           TgramAPIJson
import           TgramAPITypes

newtype NominateReferee = Referee T.Text
newtype AMove = AMove T.Text
data Assignment = ToBlack T.Text | ToWhite T.Text
newtype Remove = RemoveCmd T.Text
data Cmd = AutoStart | Start RoomType | Load FEN | NominateReferee T.Text | SubmitMove AMove | SubmitPreMoves [AMove] | Info | Resign | NominateSub T.Text | Assign Assignment | Remove T.Text | Abort | Restore
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
parseCmd msg =
    let txt = contents msg
    in  if T.null txt then Right AutoStart else go . split $ txt
    where
        contents m = fromMaybe mempty $ text m
        split = T.splitOn " "
        ctype m = chat_type . chat $ m
        go [] = Left . NoParse $ mempty
        go (cw:rest)
            |   cw == "/abort" = Right Abort
            |   cw == "/assign" =
                    let [assigned, colour] = rest
                    in  if colour == "b" then Right . Assign . ToBlack $ assigned else Right . Assign . ToWhite $ assigned
            |   cw == "/help" = Right AutoStart
            |   cw == "/info" = Right Info
            |   cw == "/load" = if null rest then Left EmptyMessage else Right . Load . FEN . head $ rest
            |   cw == "/move" = if null rest then Left EmptyMessage else Right . SubmitMove . AMove . head $ rest
            |   cw == "/new" = case ctype msg of
                    Group      -> Right $ Start Priv
                    Supergroup -> Right $ Start Pub
                    _          -> Left OnlyGroupSuper
            |   cw == "/premove" = Right . SubmitPreMoves $ AMove <$> rest
            |   cw == "/referee" =
                    let [appointed] = rest
                    in  Right . NominateReferee $ appointed
            |   cw == "/remove" =
                    let [toRemove] = rest
                    in  Right . Remove $ toRemove
            |   cw == "/resign" = Right Resign
            |   cw == "/restore" = Right Restore
            |   cw == "/start" = Right AutoStart
            |   cw == "/sub" =
                    let [substitute] = rest
                    in  Right . NominateSub $ substitute
            |   otherwise  = Left . NoParse . contents $ msg

data EvaluateError = AlreadyGameInChat | GameDoesNotExist | NotPlayerOrTurn | IllegalMove T.Text | EvaluateNotImplemented T.Text | GameStatusDoesNotFit | GameNotStarted | GameFinished Result | UnableToRestore T.Text

renderEvaluateError :: EvaluateError -> T.Text
renderEvaluateError AlreadyGameInChat = "There is already a game in this chat, please resign or abort before starting or loading a new game."
renderEvaluateError GameDoesNotExist = "There is no game in this chat. Use /new to start a new one."
renderEvaluateError (IllegalMove move) = T.append "That's an illegal move: " move
renderEvaluateError (EvaluateNotImplemented txt) = txt
renderEvaluateError GameStatusDoesNotFit = "What you're trying to do cannot be done given the current state of the game."
renderEvaluateError GameNotStarted = "The game has not started yet."
renderEvaluateError (GameFinished result) = renderResult result
renderEvaluateError (UnableToRestore txt) = T.append "Unable to restore for this reason: " txt
renderEvaluateError NotPlayerOrTurn = "You need to be part of the team whose turn is now to submit a move."

nowTheGames :: Bot (UTCTime, Games)
nowTheGames = ask >>= \env -> liftIO $ getCurrentTime >>= \now -> pure (now, memstore env)

finishRight :: MonadIO m => m (Either a ())
finishRight = pure (Right ())

retryOrFail :: Cmd -> Message -> EvaluateError -> Bot (Either EvaluateError ())
retryOrFail cmd msg error = ask >>= \env -> do
    let cid = chat_id . chat $ msg
        tok = token env
        mvar = memstore env
        p = pipe env
    hmap <- liftIO $ readMVar mvar
    tryRestoreGame p cid >>= \case
        Left _ -> pure . Left $ error
        Right game_doc -> case bsonToGame game_doc of
            Nothing -> pure . Left . UnableToRestore $ "Game found but decoding failed."
            Just restored ->
                let updated = HMS.insert cid restored hmap
                in  do
                    liftIO $ concurrently_
                        (sendMessage cid "Restoring from database, hang on..." tok)
                        (modifyMVar_ mvar $ \_ -> pure updated)
                    evaluateCmd cmd msg

quickStart :: Message -> RoomType -> Bot (Either EvaluateError ())
quickStart msg ty = do
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
        reply = case ty of
            Pub -> "All players should now pick a colour and the creator of the game should set a max. duration between each move. Please use the buttons below. The game will abort if it has not been set up within the next 15 minutes."
            Priv -> "Both players should now pick a colour and the creator of the game should set a max. duration between each move. Please use the buttons below. The game will abort if it has not been set up for the next 15 minutes."
    env <- ask
    (now, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    let exists = maybe False (\g -> case status g of Finished _ -> False; _ -> True) (HMS.lookup cid hmap)
    if exists then sendMessage cid (renderEvaluateError AlreadyGameInChat) (token env) >> finishRight
    else let state = initGameState ty now uid cid; updated_map = HMS.insert cid state hmap in do
        sendMessageKeyboard cid reply (token env) (inlineKeyboards $ keyboard state)
        liftIO $ modifyMVar_ mvar (\_ -> pure updated_map)
        finishRight

evaluateCmd :: Cmd -> Message -> Bot (Either EvaluateError ())
evaluateCmd Abort msg = do
    let reply = "Game aborted successfully."
        cid = chat_id . chat $ msg
        uid = maybe (0::UserId) user_id (from msg)
    env <- ask
    (_, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Just game -> case status game of
            Finished res -> sendMessage cid (renderResult res) (token env) >> finishRight
            _ ->
                let deleted = HMS.delete cid hmap
                    abort = liftIO $ mapConcurrently_ id [
                        modifyMVar_ mvar (\_ -> pure deleted),
                        sendMessage cid reply (token env),
                        removeThisGame (pipe env) cid
                        ] >> finishRight
                in  case lastMove game of
                    Nothing -> abort
                    Just _ -> if uid `notElem` referees game then pure . Left $ NotPlayerOrTurn else abort
        Nothing -> sendMessage cid (renderEvaluateError GameDoesNotExist) (token env) >> finishRight
evaluateCmd AutoStart msg = do
    let cid = chat_id . chat $ msg
        reply = "/abort - to abort the game (can only do when the game has not started yet)\n/load <fen_string> - copy-paste an FEN-encoded string as argument and you'll be able to resume from the position directly\n/move <your_move> - express your moves using the UCI format, i.e. /move e2e4\n/new - to start a new game\n/resign - to resign from the game (can only do when it's your turn)\n/start - (or /help) to display the list of commands"
    env <- ask
    sendMessage cid reply (token env)
    finishRight
evaluateCmd Info msg = do
    let cid = chat_id . chat $ msg
    env <- ask
    games <- liftIO . readMVar . memstore $ env
    case HMS.lookup cid games of
        Nothing -> retryOrFail AutoStart msg GameDoesNotExist
        Just game ->
            let fields = [
                    T.pack . show $ createdOn game,
                    T.pack . show $ game_chatid game,
                    case roomType game of Priv -> "Private chat"; Pub -> "Public chat",
                    T.pack . show $ status game,
                    maybe "No last move" (\(Move mv) -> mv) (lastMove game),
                    maybe mempty (\(FEN fen) -> fen) (lastPosition game),
                    maybe mempty (T.pack . show) (timeforMoves game)
                    ]
                reply = T.concat $ zipWith (\val lab -> lab `T.append` ": " `T.append` val `T.append` "\n") fields ["Created on", "Chat id", "Room type", "Game has status", "Last move", "Last position", "Time between moves (in seconds)"]
            in  sendMessage cid reply (token env) >> finishRight
evaluateCmd (Load fen@(FEN fen_str)) msg = do
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    (now, mvar) <- nowTheGames
    games <- liftIO $ readMVar mvar
    case HMS.lookup cid games of
        Nothing ->
            let reply = "No ongoing game in this chat " `T.append` "(" `T.append` (T.pack . show $ cid) `T.append` ")"
                mb_new_state = loadGameState (initGameState Priv now uid cid) fen
            in  case mb_new_state of
                Just new_state ->
                    let inserted = HMS.insert cid new_state games
                        reply = "Both players should now pick a colour and the creator of the game should set a max. duration between each move. Please use the buttons below.\n"
                    in  liftIO $ mapConcurrently_ id [
                            modifyMVar_ mvar $ \_ -> pure inserted,
                            saveGame (pipe env) cid new_state,
                            sendMessageKeyboard cid reply (token env) (inlineKeyboards $ keyboard new_state)
                        ] >> finishRight
                Nothing -> pure . Left . UnableToRestore $ fen_str
        Just game -> pure . Left $ AlreadyGameInChat
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
                    in  if uid `notElem` (whitePlayers restored ++ blackPlayers restored) then pure . Left $ NotPlayerOrTurn else do
                        liftIO $ concurrently_
                            (modifyMVar_ mvar $ \_ -> pure inserted_hmap)
                            (reqCallCFunc (token env) (endpoint env) svg)
                        finishRight
evaluateCmd (Start Pub) msg = quickStart msg Pub
evaluateCmd cmd@(Start Priv) msg = quickStart msg Priv
evaluateCmd cmd@(SubmitMove (AMove move)) msg = do
    let cid = chat_id . chat $ msg
        uid = maybe 0 user_id (from msg)
    env <- ask
    let tok = token env
        p = pipe env
    (now, mvar) <- nowTheGames
    hmap <- liftIO $ readMVar mvar
    case HMS.lookup cid hmap of
        Nothing -> retryOrFail cmd msg GameDoesNotExist
        Just game' -> case status game' of
            InPreparation -> pure . Left $ GameNotStarted
            Finished res -> pure . Left $ GameFinished res
            _ ->
                let playing_team = maybe (whitePlayers game) (\case W -> blackPlayers game; B -> whitePlayers game) (lastSidePlayed game)
                    has_colour = if uid `elem` whitePlayers game then W else B
                    game = case status game' of Ready -> game' { status = Started }; _ -> game'
                in  case roomType game of
                    Pub ->
                        if uid `notElem` playing_team then pure . Left $ NotPlayerOrTurn
                        else case tryMove (lastPosition game) move of
                            Left err -> sendMessage cid (renderChessError err) tok >> finishRight
                            Right (move@(Move mv), fen@(FEN new_fen)) -> case playersVotes game of
                                Nothing ->
                                    let votes_hmap = HMS.insert uid move HMS.empty
                                        updated_with_votes = game { playersVotes = Just votes_hmap }
                                        reply = "New move submitted. Please use the button below to vote for it. Send another move (using '/move') if you'd like instead"
                                    in  liftIO $ mapConcurrently_ id [
                                            sendMessageKeyboard cid reply tok $ votingKeyboard updated_with_votes,
                                            modifyMVar_ mvar (\_ -> pure (HMS.update (\_ -> pure updated_with_votes) cid hmap)),
                                            saveGame p cid updated_with_votes
                                        ] >> finishRight
                                Just votes_hmap ->
                                    let new_alerts = (mempty, mempty)
                                        updated_votes = HMS.update (\_ -> pure move) uid votes_hmap
                                        updated_with_votes = game { playersVotes = Just updated_votes }
                                        updated_with_votes_and_accepted = updated_with_votes { notified = new_alerts, lastMove = Just move, lastPosition = Just fen, lastTimeMoved = Just now, lastSidePlayed = Just has_colour }
                                        clears_majority = winner > fromIntegral (length playing_team `div` 2)
                                            where
                                                winner = snd . head . sortOn (Down . snd) . HMS.toList .
                                                    HMS.foldl' (\acc (Move mv) -> case HMS.lookup mv acc of
                                                        Nothing -> HMS.insert mv (1::Int) acc
                                                        Just _  -> HMS.update (\v -> Just $ v+1) mv acc) HMS.empty $ updated_votes
                                        detect_checkmate = case fromFEN (T.unpack new_fen) of
                                            Nothing  -> False
                                            Just pos -> null . legalPlies $ pos
                                        (winner_announcement, its_over) = if lastSidePlayed game == Just W then ("White won by checkmate! Good game.", Finished BlackIsMate) else ("Black won by checkmate! Good game.", Finished WhiteIsMate)
                                        game_over = updated_with_votes_and_accepted { status = its_over}
                                        (payload, reply)
                                            |   not clears_majority = (updated_with_votes, "Thanks for this vote, " `T.append` mv `T.append` ". The majority is yet to accept it.")
                                            |   not detect_checkmate = (updated_with_votes_and_accepted, "Thanks for this vote " `T.append` mv `T.append` ". Registering now.")
                                            |   otherwise = (game_over,  "Thanks for this vote" `T.append` mv `T.append` ". This was a winning move: " `T.append` winner_announcement)
                                    in  liftIO $ mapConcurrently_ id [
                                            editMessageKeyboard cid (message_id msg) reply tok $ votingKeyboard payload,
                                            modifyMVar_ mvar (\_ -> pure (HMS.update (\_ -> pure payload) cid hmap)),
                                            saveGame p cid game,
                                            void . reqCallCFunc tok (endpoint env) $ SVGToPNG cid (Just mv) new_fen
                                        ] >> finishRight
                    Priv ->
                        let doMove = case tryMove (lastPosition game) move of
                                Left err -> sendMessage cid (renderChessError err) tok >> finishRight
                                Right (move@(Move mv), fen@(FEN new_fen)) ->
                                    let new_alerts = (mempty, mempty)
                                        new_state = game { notified = new_alerts, lastTimeMoved = Just now, lastMove = Just move, lastPosition = Just fen, lastSidePlayed = Just has_colour }
                                        detect_checkmate = case fromFEN (T.unpack new_fen) of
                                            Nothing  -> False
                                            Just pos -> null . legalPlies $ pos
                                        (winner_announcement, its_over) = if lastSidePlayed game == Just W then ("White won by checkmate! Good game.", Finished BlackIsMate) else ("Black won by checkmate! Good game.", Finished WhiteIsMate)
                                        game_over = new_state { status = its_over}
                                        updateWith payload = HMS.update (\_ -> Just payload) cid hmap
                                        (game, games) = if detect_checkmate then (new_state, updateWith new_state) else (game_over, updateWith game_over)
                                    in  do
                                        liftIO $ mapConcurrently_ id [
                                            sendMessage cid ("Thanks for this move: " `T.append` mv) tok,
                                            modifyMVar_ mvar (\_ -> pure games),
                                            saveGame (pipe env) cid game,
                                            void . reqCallCFunc tok (endpoint env) $ SVGToPNG cid (Just mv) new_fen
                                            ]
                                        when detect_checkmate $ sendMessage cid winner_announcement tok
                                        finishRight
                        in  if uid `notElem` playing_team then pure . Left $ NotPlayerOrTurn else doMove
evaluateCmd _ _ = pure . Left . EvaluateNotImplemented $ "Failed to evaluated this command (not implemented)."

sendMessage :: MonadIO m => ChatId -> T.Text -> T.Text ->  m ()
sendMessage cid msg tok = void . reqSend tok "sendMessage" $ SendMessage cid msg Nothing

sendMessageKeyboard :: MonadIO m => ChatId -> T.Text -> T.Text -> InlineKeyboardMarkup -> m ()
sendMessageKeyboard cid msg tok keys = void . reqSend tok "sendMessage" $ SendMessage cid msg (Just keys)

editMessage :: MonadIO m => ChatId -> Int -> T.Text -> T.Text -> m ()
editMessage cid mid msg tok = void . reqSend tok "editMessageText" $ EditMessage cid msg mid Nothing

editMessageKeyboard :: MonadIO m => ChatId -> Int -> T.Text -> T.Text -> InlineKeyboardMarkup -> m ()
editMessageKeyboard cid mid msg tok keys = void . reqSend tok "editMessageText" $ EditMessage cid msg mid (Just keys)

reqSend :: MonadIO m => T.Text -> T.Text -> OutMessage -> m ()
reqSend token postMethodName encodedMsg = case encodedMsg of
    SendMessage _ t _   -> unless (T.null t) go
    EditMessage _ t _ _ -> unless (T.null t) go
    where
        go = runReq defaultHttpConfig $ do
            let url = https "api.telegram.org" /: token
                reqUrl = url /: postMethodName
            req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) ignoreResponse mempty
            pure ()

