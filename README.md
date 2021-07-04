# Demo
1. Create a chat on Telegram.
2. Invite `@pubchess_bot` along with a friend of yours. If you have no friend you can invite me, `@ad_himself`. If your friend does not allow invitations, simply send him a a link to the chat privately.
3. Use `/new` to setup a new game. Use the inline buttons to set up the game as you please. When all settings are provided and both players have picked a colour, the game will start automatically.
4. Should you receive an error telling you that the game does not exist, this might be because the server was restarted. Then simply have one of the two players use the `/restore` command to bring the game back up. 

__NB: The bot does not (yet) work in Telegram _supergroup_ chat. A chat is considered a supergroup chat if any of the following conditions is met:__

* the `Allow new users to see past history` setting is enabled
* the chat is set from `private` to `public`

# Commands
* `/new`
* `/restore`
* `/move <your_move>`

The format for registering moves is UCL. To move the piece currently on e2 to e4, do `/move e2e4`.

# To do list
* implement time control notifications
* implement public chess games
