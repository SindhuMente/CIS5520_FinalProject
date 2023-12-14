module GameState where

import Control.Applicative
import Control.Arrow (Arrow (first))
import Control.Monad (when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Maze
import Maze qualified as P
import State (State)
import State qualified as S
import System.IO
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)
import Prelude

------------------------------------------------------------------------------------------

-- | Part 0: GameState Data Types & Arbitrary Instances

------------------------------------------------------------------------------------------

data Action = MUp | MDown | MLeft | MRight deriving (Eq, Show)

instance Arbitrary Action where
  arbitrary :: Gen Action
  arbitrary = QC.elements [MUp, MDown, MLeft, MRight]
  shrink :: Action -> [Action]
  shrink _ = [MUp]

data Player = One | Two deriving (Eq, Ord)

instance Arbitrary Player where
  arbitrary :: Gen Player
  arbitrary = QC.elements [One, Two]
  shrink :: Player -> [Player]
  shrink _ = [One]

instance Show Player where
  show :: Player -> String
  show One = "One (S)"
  show Two = "Two (T)"

instance Arbitrary Game where
  arbitrary :: Gen Game
  arbitrary = genMaze >>= \x -> return (initializeGameState initialGame x)
  shrink :: Game -> [Game]
  shrink g = [g]

genMaze :: Gen Maze
genMaze =
  let updatedCoins = QC.chooseInt (0, 4) >>= \l -> return (addCoinsToMazeRandom l qcMaze)
   in QC.chooseInt (0, 4) >>= \x -> updatedCoins >>= \y -> return (addCompassesToMazeRandom x y)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo} deriving (Eq, Show)

data Attributes = Attributes {numPoints :: Int, position :: Cell} deriving (Eq, Show)

data PlayerInfo = PlayerInfo {player1 :: Attributes, player2 :: Attributes} deriving (Eq, Show)

qcMaze :: Maze
qcMaze =
  Maze
    { cells =
        [ Cell {x = 0, y = 0, isWall = False},
          Cell {x = 0, y = 1, isWall = True},
          Cell {x = 0, y = 2, isWall = False},
          Cell {x = 0, y = 3, isWall = True},
          Cell {x = 0, y = 4, isWall = False},
          Cell {x = 0, y = 5, isWall = True},
          Cell {x = 0, y = 6, isWall = True},
          Cell {x = 0, y = 7, isWall = False},
          Cell {x = 0, y = 8, isWall = False},
          Cell {x = 0, y = 9, isWall = False},
          Cell {x = 0, y = 10, isWall = True},
          Cell {x = 1, y = 0, isWall = False},
          Cell {x = 1, y = 1, isWall = False},
          Cell {x = 1, y = 2, isWall = False},
          Cell {x = 1, y = 3, isWall = True},
          Cell {x = 1, y = 4, isWall = False},
          Cell {x = 1, y = 5, isWall = True},
          Cell {x = 1, y = 6, isWall = True},
          Cell {x = 1, y = 7, isWall = True},
          Cell {x = 1, y = 8, isWall = True},
          Cell {x = 1, y = 9, isWall = False},
          Cell {x = 1, y = 10, isWall = True},
          Cell {x = 2, y = 0, isWall = False},
          Cell {x = 2, y = 1, isWall = False},
          Cell {x = 2, y = 2, isWall = False},
          Cell {x = 2, y = 3, isWall = False},
          Cell {x = 2, y = 4, isWall = False},
          Cell {x = 2, y = 5, isWall = False},
          Cell {x = 2, y = 6, isWall = False},
          Cell {x = 2, y = 7, isWall = False},
          Cell {x = 2, y = 8, isWall = False},
          Cell {x = 2, y = 9, isWall = True},
          Cell {x = 2, y = 10, isWall = True},
          Cell {x = 3, y = 0, isWall = True},
          Cell {x = 3, y = 1, isWall = False},
          Cell {x = 3, y = 2, isWall = False},
          Cell {x = 3, y = 3, isWall = True},
          Cell {x = 3, y = 4, isWall = False},
          Cell {x = 3, y = 5, isWall = False},
          Cell {x = 3, y = 6, isWall = False},
          Cell {x = 3, y = 7, isWall = False},
          Cell {x = 3, y = 8, isWall = False},
          Cell {x = 3, y = 9, isWall = True},
          Cell {x = 3, y = 10, isWall = True},
          Cell {x = 4, y = 0, isWall = True},
          Cell {x = 4, y = 1, isWall = True},
          Cell {x = 4, y = 2, isWall = False},
          Cell {x = 4, y = 3, isWall = True},
          Cell {x = 4, y = 4, isWall = True},
          Cell {x = 4, y = 5, isWall = True},
          Cell {x = 4, y = 6, isWall = False},
          Cell {x = 4, y = 7, isWall = True},
          Cell {x = 4, y = 8, isWall = True},
          Cell {x = 4, y = 9, isWall = True},
          Cell {x = 4, y = 10, isWall = True},
          Cell {x = 5, y = 0, isWall = True},
          Cell {x = 5, y = 1, isWall = False},
          Cell {x = 5, y = 2, isWall = True},
          Cell {x = 5, y = 3, isWall = True},
          Cell {x = 5, y = 4, isWall = True},
          Cell {x = 5, y = 5, isWall = True},
          Cell {x = 5, y = 6, isWall = False},
          Cell {x = 5, y = 7, isWall = False},
          Cell {x = 5, y = 8, isWall = False},
          Cell {x = 5, y = 9, isWall = True},
          Cell {x = 5, y = 10, isWall = True},
          Cell {x = 6, y = 0, isWall = False},
          Cell {x = 6, y = 1, isWall = False},
          Cell {x = 6, y = 2, isWall = False},
          Cell {x = 6, y = 3, isWall = True},
          Cell {x = 6, y = 4, isWall = True},
          Cell {x = 6, y = 5, isWall = False},
          Cell {x = 6, y = 6, isWall = False},
          Cell {x = 6, y = 7, isWall = False},
          Cell {x = 6, y = 8, isWall = True},
          Cell {x = 6, y = 9, isWall = True},
          Cell {x = 6, y = 10, isWall = True},
          Cell {x = 7, y = 0, isWall = True},
          Cell {x = 7, y = 1, isWall = True},
          Cell {x = 7, y = 2, isWall = False},
          Cell {x = 7, y = 3, isWall = False},
          Cell {x = 7, y = 4, isWall = False},
          Cell {x = 7, y = 5, isWall = False},
          Cell {x = 7, y = 6, isWall = True},
          Cell {x = 7, y = 7, isWall = False},
          Cell {x = 7, y = 8, isWall = True},
          Cell {x = 7, y = 9, isWall = True},
          Cell {x = 7, y = 10, isWall = True},
          Cell {x = 8, y = 0, isWall = True},
          Cell {x = 8, y = 1, isWall = True},
          Cell {x = 8, y = 2, isWall = True},
          Cell {x = 8, y = 3, isWall = False},
          Cell {x = 8, y = 4, isWall = True},
          Cell {x = 8, y = 5, isWall = False},
          Cell {x = 8, y = 6, isWall = True},
          Cell {x = 8, y = 7, isWall = False},
          Cell {x = 8, y = 8, isWall = True},
          Cell {x = 8, y = 9, isWall = True},
          Cell {x = 8, y = 10, isWall = False}
        ],
      startPlayerOne = Cell {x = 0, y = 4, isWall = False},
      startPlayerTwo = Cell {x = 8, y = 5, isWall = False},
      goal = Cell {x = 2, y = 8, isWall = False},
      coins = [],
      compasses = [],
      portals = [Portal {entrance = Cell {x = 6, y = 0, isWall = False}, exit = Cell {x = 4, y = 7, isWall = False}}, Portal {entrance = Cell {x = 2, y = 5, isWall = False}, exit = Cell {x = 1, y = 0, isWall = False}}],
      rows = 9,
      cols = 11
    }

------------------------------------------------------------------------------------------

-- | Part 1: Moving to an Empty Cell with no special Tokens

------------------------------------------------------------------------------------------

-- move player 1 step in specified direction
moveOne :: Cell -> Player -> State Game ()
moveOne c p = do
  g <- S.get
  let newPlayerInfo = updateInfo p (playerInfo g) c False
   in updateGameState p (g {playerInfo = newPlayerInfo, current = switchPlayer p})

------------------------------------------------------------------------------------------

-- | Part 2: Moving to a Cell with a Portal & Entering it

------------------------------------------------------------------------------------------

-- enter a portal & transport player to predetermined location
enterPortal :: Cell -> Player -> State Game ()
enterPortal c p = do
  g <- S.get
  let exitPortal = fetchPortal g c
   in let newPlayerInfo = updateInfo p (playerInfo g) exitPortal False
       in updateGameState p (g {playerInfo = newPlayerInfo, current = switchPlayer p})

-- helper to get exit of portall associated with given entrance cell
fetchPortal :: Game -> Cell -> Cell
fetchPortal g c =
  let myPortal = Prelude.filter (\p -> entrance p == c) (portals (board g))
   in case myPortal of
        [x] -> exit x
        _ -> Cell {x = -1, y = -1, isWall = True} -- case where != 1 portals found

------------------------------------------------------------------------------------------

-- | Part 3: Moving to a Cell with a Coin on it

------------------------------------------------------------------------------------------

-- collect a coin and add 1 to score of player p
collectCoin :: Cell -> Player -> State Game ()
collectCoin c p =
  do
    g <- S.get
    let newPlayerInfo = updateInfo p (playerInfo g) c True
     in let updatedMaze = removeCoin (board g) c
         in updateGameState p (Game {board = updatedMaze, playerInfo = newPlayerInfo, current = switchPlayer p})

-- remove coin from maze once it has been collected
removeCoin :: Maze -> Cell -> Maze
removeCoin maze cell =
  let updatedCoins = Prelude.filter (/= cell) (coins maze)
   in maze {coins = updatedCoins}

------------------------------------------------------------------------------------------

-- | Part 4: Moving to a Cell with a Compass on it

------------------------------------------------------------------------------------------

-- collect compass, add 1 to score of player p, & move player to compass cell, and then 1 step closer to goal,
-- or stay put (if current position is closest to goal)
collectCompass :: Cell -> Player -> State Game ()
collectCompass c p = do
  g <- S.get
  let newPosition = pointMe (board g) c p
   in let updatedMaze = removeCompass (board g) c
       in let newPlayerInfo = updateInfo p (playerInfo g) c True
           in -- we must move again according to the PointMe spell :)
              S.put (S.execState (whichMove newPosition p) (g {playerInfo = newPlayerInfo, board = updatedMaze, current = p}))

-- helper to extract closest neighbor to goal
pointMe :: Maze -> Cell -> Player -> Cell
pointMe maze c p =
  let (cX, cY) = (x c, y c)
   in let possibleNeighbors = [(cX - 1, cY), (cX + 1, cY), (cX, cY - 1), (cX, cY + 1)]
       in let validNeighbors = filterNeighbors maze possibleNeighbors ++ [(cX, cY)]
           in extractBest maze validNeighbors

-- helper to get valid neighbors
filterNeighbors :: Maze -> [(Int, Int)] -> [(Int, Int)]
filterNeighbors maze pos =
  let firstPass = filter (`elem` map (\c -> (x c, y c)) (cells maze)) pos
   in filter (\c -> not (getCellWallVal c maze)) firstPass

-- helper to extract neighbor with min distance to goal
extractBest :: Maze -> [(Int, Int)] -> Cell
extractBest m pos =
  let cup = (x (goal m), y (goal m))
   in let distances = map (\c -> (c, getManhattanDistance cup c)) pos
       in getMin distances

-- helper to compute manhattan distance heuristic
getManhattanDistance :: (Int, Int) -> (Int, Int) -> Int
getManhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- helper to get min of list of tuples of (x,y) coordinates & dist from goal
getMin :: [((Int, Int), Int)] -> Cell
getMin [] = Cell {x = -1, y = -1, isWall = True}
getMin lst = case List.sortBy (comparing snd) lst of
  (l@((a, b), c) : ls) -> Cell {x = a, y = b, isWall = False}
  _ -> Cell {x = -1, y = -1, isWall = True}

-- helper to remove compass from maze once it has been collected
removeCompass :: Maze -> Cell -> Maze
removeCompass maze cell =
  let updatedCompasses = Prelude.filter (/= cell) (compasses maze)
   in maze {compasses = updatedCompasses}

-- helper to determine which case chosen cell from the compass is
whichMove :: Cell -> Player -> State Game ()
whichMove c p = do
  g <- S.get
  if c `elem` coins (board g)
    then collectCoin c p
    else
      if c `elem` getPortalEntrances (board g)
        then enterPortal c p
        else
          if c `elem` compasses (board g)
            then collectCompass c p
            else moveOne c p

-- helper to extract list of portal entrances
getPortalEntrances :: Maze -> [Cell]
getPortalEntrances maze = map entrance (portals maze)

------------------------------------------------------------------------------------------

-- | Part 5: Helper Functions for Moving

------------------------------------------------------------------------------------------

-- helper to update player info attributes
updateInfo :: Player -> PlayerInfo -> Cell -> Bool -> PlayerInfo
updateInfo One pI c b =
  -- b is True if collected coin/compass
  let p1 = player1 pI
   in pI {player1 = p1 {position = c, numPoints = numPoints p1 + (if b then 1 else 0)}}
updateInfo Two pI c b =
  let p2 = player2 pI
   in pI {player2 = p2 {position = c, numPoints = numPoints p2 + (if b then 1 else 0)}}

-- helper to switch current player after each turn
switchPlayer :: Player -> Player
switchPlayer One = Two
switchPlayer Two = One

-- helper to extract if a cell is a wall or not (nonexistent cells are automatically walls)
getCellWallVal :: (Int, Int) -> Maze -> Bool
getCellWallVal tup maze =
  let fCells = filter (\c -> (x c, y c) == tup) (cells maze)
   in case fCells of
        [] -> True
        (x : xs) -> isWall x

-- helper to update the player positions in the Game State for printing purposes
updatePlayerState :: Player -> State Game ()
updatePlayerState p = do
  g <- S.get
  if p == One
    then S.put g {board = (board g) {startPlayerOne = position (player1 (playerInfo g))}}
    else S.put g {board = (board g) {startPlayerTwo = position (player2 (playerInfo g))}}

-- helper to call state monad functions when updating state
updateGameState :: Player -> Game -> State Game ()
updateGameState p g = S.put (S.execState (updatePlayerState p) g)

------------------------------------------------------------------------------------------

-- | Part 6: Game Logic

------------------------------------------------------------------------------------------

-- maps action on a cell to the cell a user will move to if that action is taken
actionsToCell :: Maze -> Cell -> Action -> Cell
actionsToCell maze pos MLeft =
  let newX = x pos
   in let newY = y pos - 1
       in let wallVal = getCellWallVal (newX, newY) maze
           in Cell {x = newX, y = newY, isWall = wallVal}
actionsToCell maze pos MRight =
  let newX = x pos
   in let newY = y pos + 1
       in let wallVal = getCellWallVal (newX, newY) maze
           in Cell {x = newX, y = newY, isWall = wallVal}
actionsToCell maze pos MUp =
  let newX = x pos - 1
   in let newY = y pos
       in let wallVal = getCellWallVal (newX, newY) maze
           in Cell {x = newX, y = newY, isWall = wallVal}
actionsToCell maze pos MDown =
  let newX = x pos + 1
   in let newY = y pos
       in let wallVal = getCellWallVal (newX, newY) maze
           in Cell {x = newX, y = newY, isWall = wallVal}

-- helper to extract current position of player p
extractPosition :: Player -> Game -> Cell
extractPosition p g =
  if p == One
    then position (player1 (playerInfo g))
    else position (player2 (playerInfo g))

-- helper to print player info to console
printPlayerAttributes :: Attributes -> String
printPlayerAttributes attrs =
  "Points: "
    ++ show (numPoints attrs)
    ++ "\n\
       \Position in Maze: x="
    ++ show (x (position attrs))
    ++ ", y="
    ++ show (y (position attrs))

-- helper to initialize game attributes with provided maze
initializeGameState :: Game -> Maze -> Game
initializeGameState g m =
  let p1 = startPlayerOne m
   in let p2 = startPlayerTwo m
       in let pI =
                PlayerInfo
                  { player1 = Attributes {numPoints = 0, position = p1},
                    player2 = Attributes {numPoints = 0, position = p2}
                  }
           in g {board = m, playerInfo = pI}

-- the representation of an empty game
initialGame :: Game
initialGame =
  Game
    { board =
        Maze
          { cells = [],
            startPlayerOne = Cell {x = -1, y = -1, isWall = True},
            startPlayerTwo = Cell {x = -1, y = -1, isWall = True},
            goal = Cell {x = -1, y = -1, isWall = True},
            coins = [],
            compasses = [],
            portals = [],
            rows = 1,
            cols = 1
          },
      current = One,
      playerInfo =
        PlayerInfo
          { player1 = Attributes {numPoints = 0, position = Cell {x = -1, y = -1, isWall = True}},
            player2 = Attributes {numPoints = 0, position = Cell {x = -1, y = -1, isWall = True}}
          }
    }

startGame :: IO ()
startGame = do
  putStrLn "Welcome to The Wizard Maze!"
  putStrLn "Your Goal is to get to the Tri-wizard cup before your opponent taking turns to move"
  putStrLn
    "There are some components of the Maze you should be aware of: \n \
    \ - Player One's location (indicated by S) \n \
    \ - Player Two's location (indicated by T) \n \
    \ - Coins (indicated by Q): collecting a coin adds 1 to your score \n \
    \ - Portals (indicated by P): beware: portals can transport you anywhere in the Maze \n \
    \ - Compasses (indicated by C): collecting a compass adds 1 to your score & takes you a step closer to the goal \n \
    \    - if the location of the compass is closer to the goal than a move in any valid direction, you will remain in that position \n \
    \    - please also remember that the compass is a guide and may not put you on the optimal path to the Cup"
  playGame initialGame

-- helper for start game that handles initial user input (difficulty level) & parsing
playGame :: Game -> IO ()
playGame emptyGame = do
  putStrLn "please enter your desired difficulty level (easy, medium, or hard)"
  putStr ">>> "
  hFlush stdout
  difficulty <- getLine
  case difficulty of -- extract mapping from difficulty to num coins/compasses
    "easy" -> do
      initMaze <- P.parseFromFile (many mazeFileParser) "data/easy.txt"
      getMaze initMaze "data/easy_portals.txt" 3 1 -- coins, compases
    "medium" -> do
      initMaze <- P.parseFromFile (many mazeFileParser) "data/medium.txt"
      getMaze initMaze "data/medium_portals.txt" 5 2 -- coins, compasses
    "hard" -> do
      initMaze <- P.parseFromFile (many mazeFileParser) "data/hard.txt"
      getMaze initMaze "data/hard_portals.txt" 7 3 -- coins, compasses
    "quit" -> return ()
    _ -> do
      putStrLn "invalid difficulty level, please try again!"
      playGame initialGame
  where
    -- handles parsing & generation of portals, coins, & compasses onto the maze
    getMaze initMaze fName numCoins numCompasses =
      case initMaze of
        Just (rawMaze, s) -> do
          portalsRes <- P.parseFromFile P.portalFileParser fName
          case portalsRes of
            Just (portalsList, _) -> do
              let maze = parseMaze rawMaze
              let updatedMaze = maze {portals = portalsList}
              let updatedCoins = P.addCoinsToMazeRandom numCoins updatedMaze
              let updatedCompasses = P.addCompassesToMazeRandom numCompasses updatedCoins
              let game = initializeGameState emptyGame updatedCompasses
               in do
                    go game
            _ -> do
              putStrLn "error loading maze, please try again later"
        _ -> do
          putStrLn "error loading maze, please try again later"
    -- recursive function that handle IO -> terminates when user quits or if a user wins
    go game =
      do
        let curPlayer = current game
         in do
              putStrLn (show (board game) ++ "\n")
              putStrLn "------ Player One (S) Info: -------"
              putStrLn (printPlayerAttributes (player1 (playerInfo game)) ++ "\n")
              putStrLn "------ Player Two (T) Info: -------"
              putStrLn (printPlayerAttributes (player2 (playerInfo game)))
              putStrLn "-------------------------------\n"
              putStr ("Player " ++ show curPlayer ++ "'s Turn >>> ")
              hFlush stdout
              dir <- getLine
              case dir of
                "l" -> makeMove MLeft curPlayer game
                "r" -> makeMove MRight curPlayer game
                "u" -> makeMove MUp curPlayer game
                "d" -> makeMove MDown curPlayer game
                "quit" -> return ()
                _ -> do
                  putStrLn "invalid move, please enter a valid move (l, r, u, d, quit)\n"
                  go game
    -- helper for choosing which  of move functions to call
    makeMove action p g = do
      let pos = extractPosition p g
       in let c = actionsToCell (board g) pos action
           in if c `elem` cells (board g)
                then
                  if isWall c
                    then do
                      putStrLn "can't move to a Wall! Please try another move\n" -- do something if trying to move to wall
                      go g
                    else
                      if p == current g
                        then
                          if c == goal (board g)
                            then do
                              putStrLn ("Congrats Player " ++ show p ++ "! You Won the TriWizard Cup!") -- cur player wins!
                            else do
                              let g' = S.execState (whichMove c p) g
                               in go g'
                        else do
                          putStrLn "something went wrong, please try again later" -- do something if player isn't current
                else do
                  putStrLn "you are trying to move outside of the maze, please choose a valid direction" -- error trying to move outside of board
                  go g
