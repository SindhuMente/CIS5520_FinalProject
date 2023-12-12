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

## Compilation & running instructions
To start the game and play the game:
- `stack ghci src/GameState.hs`
- type `startGame` and press enter

To run tests:










This is an "Empty project" for Haskell. It is configured in the same way as
the lecture demo and homework assignments for CIS 5520, but contains no
code. Feel free to use this repository for experimentation!

If you want to change the name of this project, look for all occurrences of
`project-cis5520` in the `project-cis5520.cabal` file and in the `hie.yaml` 
file. (And change the name of the cabal file to match your new name!)

## Module organization

Haskell packages typically divide their source code into three separate places:

  - The bulk of your code should be developed as a reusable library in 
    modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) 
    for you to get started. You can add additional modules here.
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-21.6 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

