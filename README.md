## Private games (1 vs. 1)
1. Create a chat on Telegram.
2. Invite `@pubchess_bot` along with a friend of yours. If you have no friend you can invite me, `@ad_himself`. If your friend does not allow invitations, simply send him a link to the chat privately.
3. Use `/new` to setup a new game. Use the inline buttons to set up the game as you please. When all settings are provided and both players have picked a colour, the game will start automatically.
4. Use `/move <yourmove>` to make a move. The game uses the UCI notation, here are some examples
  * White pawn to e4: `/move e2e4`
  * castling king side with White: `/move e1g1`
  * promoting h-file Black pawn to a Queen: `/move h2h1q` (use `r`, `b`, `n` for Rooks, Bishops and Knights respectively)
5. Beware of time controls! If you fail to input a move before a timeout occurs, you will lose the game. The game will notify twice before that happens, however.

## Public games (x vs. y)
1. Create a chat on Telegram, and in any order: invite your friends and make the chat public. If your friends do not allow invitations, simply send them a link to the chat privately.
2. Only then: invite `@pubchess_bot`. If you invite the bot before making the chat public it will not work. 
3. Use `/new` to setup a new game. Use the inline buttons to set up the game as you please. When all time settings are provided and each side have at least one played, the game creator will be prompted to start the game using the providing inline button.
4. Use `/move <yourmove>` to submit a move. To submit a move, it must be your Colour to play. The game uses the UCI notation, here are some examples
  * White pawn to e4: `/move e2e4`
  * castling king side with White: `/move e1g1`
  * promoting h-file Black pawn to a Queen: `/move h2h1q` (use `r`, `b`, `n` for Rooks, Bishops and Knights respectively)
5. After a move is submitted, everyone in the team for which the move has been submitted can either vote for an existing move (using the provided inline buttons) or can submit an alternative move, that is then added to the buttons for others to vote for. At any moment, a player can change their vote. As soon as a move clears the majority threshold, it is played.
6. Beware of time controls! If your team fails to input a move before a timeout occurs, you will lose the game. However, the game will notify twice before that happens. Moreover if the game finds that a move was submitted for voting as the time runs out, it will consider it accepted and play it.

# Commands
* `/abort` -- abort the current game is not move was played
*  `/help` -- show this list of commands
* `/info` -- shows infos about the current running game in the chat if any
* `/move <your_move>` -- make (private chats) or submit (public chats) a move
* `/new` -- start a new game if no game is running already in the chat
* `/resign` -- resigns for the Colour to play
* `/start` -- same as `/help`

# To do list
* [x] implemented time control notifications
* [x] implemented public chess games with:
  * [x] forced moves on timeout when a best move can be determined
  * [ ] add votes for resigning
