The README for our Implementation of Pac-man
-Isabel Kurvers 9698434
-Feiko Augustijn 2413531

Controls:
(WASD/Arrows) Controlling the player, in four directions.
(Space) Pause the game
(R) Reset the game. This will reset everything to the beginning state
(T) Save your current progress. When restarting the game, the game will open on the last save.
(Y) Open the most recent save.

Custom Levels:
We have two files in the game, both taking in the same format. 
By making a change to “veld_pac-man.txt” you can edit the starting save file. 
When resetting (pressing ‘R’) the map will reset to this file. Since this is a safe file, 
you can change much more, like the starting position of the ghosts and player, and the lives of the player.
By changing “SaveState.txt” you can change data for the current save. When starting the game up again, 
it will automatically load up any changes made to this file. 
When changing these files it is very important not to change the order of the lines, 
or change the size of the map. When keeping this format, any change should work as described.

"w" = Wall
"o" = Small dot
"0" = Big dot
"l" = The left of the cage, works as a wall
"r" = The right of the cage, works as a wall
"_" = The bottom of the cage, works as a wall
"-" = The top of the cage, works as a wall
"n" = Nothing, air
"d" = Door. Wall for player, considered air for the ghosts

Reminder: only swap out individual letters, do not add any rows or make them shorter/longer. 
This will cause the game to crash.
 
