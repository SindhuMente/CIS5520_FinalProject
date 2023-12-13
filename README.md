# project-cis5520: Wizard Maze

Names: Sindhura Mente, Amelia Sharpe
Pennkeys: smente, amsharpe

## Game description
The Wizard Maze is a game inspired by the TriWizard Tournament from Harry Potter and the
Goblet of Fire. The core objective is to collect a chalice from a predetermined goal state 
within the maze, collecting tokens to help get closer to the goal state as well as avoiding or
using portals which either aid or deter the player’s progress.

## File overview (in order they should be read):

### Maze.hs:
This module defines data types for representing the maze itself, cells, and portals. It contains
parsers for reading an ASCII maze from a text file as well as functions for adding cells, 
coins, compasses and setting the start and goal positions. It also parses a separate file containing
coordinates for where to place portal entrances and exits and adds them to the maze data structure.
Lastly, generates random coin and compasses positions and adds them to the maze and provides functions
to visualize the maze as a string with ASCII art.

### GameState.hs:
This module contains the main game logic and IO functionality. It wraps a GameState around the Maze
provided by the Maze parsing module to keep track of player information and makes use of the state monad to
handle the changes to the game after each player moves. This module handles moving logic including the 
collection of coins & compasses & moving through portals, and displays the maze and player information to the 
users while allowing them to interact with the game by first choosing their difficulty level and then playing the game.


## Additional libraries depended on
DList.hs
ParserCombinators.hs
State.hs

## Other considerations
We initially thought of using the Dfs module but ended up not needing it because we did not end up
randomly generated an entire mazes, but rather portions of it such as tokens.

## Compilation & running instructions
- This project compiles with `stack build`
- You can run the game with `stack run`
- You can run the tests with `stack test`

Finally, you can start a REPL with stack ghci.