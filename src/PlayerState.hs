module PlayerState where

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
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

data Action = MUp | MDown | MLeft | MRight deriving (Eq, Show)

data Player = One | Two deriving (Eq, Ord, Show)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo} deriving (Show)

data Attributes = Attributes {numPoints :: Int, position :: Cell} deriving (Eq, Show)

data PlayerInfo = PlayerInfo {player1 :: Attributes, player2 :: Attributes} deriving (Eq, Show)

data End = Win Player | Lose Player deriving (Eq, Show)

getPortalEntrances :: Maze -> [Cell]
getPortalEntrances maze = map entrance (portals maze)

moveOne :: Cell -> Player -> State Game ()
moveOne c p = do
  g <- S.get
  let newPlayerInfo = updatePosition p (playerInfo g) c
   in S.put (g {playerInfo = newPlayerInfo, current = switchPlayer p})

updatePosition :: Player -> PlayerInfo -> Cell -> PlayerInfo
updatePosition One pI c =
  let p1 = player1 pI
   in pI {player1 = p1 {position = c}}
updatePosition Two pI c =
  let p2 = player2 pI
   in pI {player2 = p2 {position = c}}

updatePoints :: Player -> PlayerInfo -> Cell -> PlayerInfo
updatePoints One pI c =
  let p1 = player1 pI
   in pI {player1 = p1 {position = c, numPoints = numPoints p1 + 1}}
updatePoints Two pI c =
  let p2 = player2 pI
   in pI {player2 = p2 {position = c, numPoints = numPoints p2 + 1}}

extractPosition :: Player -> Game -> Cell
extractPosition p g =
  if p == One
    then position (player1 (playerInfo g))
    else position (player2 (playerInfo g))

-- assuming coin = 1 point
collectCoin :: Cell -> Player -> State Game ()
collectCoin c p =
  do
    g <- S.get
    let newPlayerInfo = updatePoints p (playerInfo g) c
     in let updatedMaze = removeCoin (board g) c
         in S.put Game {board = updatedMaze, playerInfo = newPlayerInfo, current = switchPlayer p}

removeCoin :: Maze -> Cell -> Maze
removeCoin maze cell =
  let updatedCoins = Prelude.filter (/= cell) (coins maze)
   in maze {coins = updatedCoins}

fetchPortal :: Game -> Cell -> Cell
fetchPortal g c =
  let myPortal = Prelude.filter (\p -> entrance p == c) (portals (board g))
   in case myPortal of
        [x] -> exit x
        _ -> Cell {x = -1, y = -1, isWall = True} -- error? how to handle case where != 1 portals found?

enterPortal :: Cell -> Player -> State Game ()
enterPortal c p = do
  g <- S.get
  let exitPortal = fetchPortal g c
   in let newPlayerInfo = updatePosition p (playerInfo g) exitPortal
       in S.put (g {playerInfo = newPlayerInfo, current = switchPlayer p})

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

collectCompass :: Cell -> Player -> State Game ()
collectCompass c p = do
  g <- S.get
  let newPosition = pointMe (board g) c p
   in let updatedMaze = removeCompass (board g) c
       in let newPlayerInfo = updatePoints p (playerInfo g) c
           in do
                S.put (g {playerInfo = newPlayerInfo, board = updatedMaze})
                whichMove newPosition p

-- in let newGame = S.put (g {playerInfo = newPlayerInfo, board = updatedMaze})
-- in whichMove newPosition p -- we must move again according to the PointMe spell :)

removeCompass :: Maze -> Cell -> Maze
removeCompass maze cell =
  let updatedCompasses = Prelude.filter (/= cell) (compasses maze)
   in maze {compasses = updatedCompasses}

pointMe :: Maze -> Cell -> Player -> Cell
pointMe maze c p =
  let (cX, cY) = (x c, y c)
   in let possibleNeighbors = [(cX - 1, cY), (cX + 1, cY), (cX, cY - 1), (cX, cY + 1)]
       in let validNeighbors = filterNeighbors maze possibleNeighbors ++ [(cX, cY)]
           in extractBest maze validNeighbors

filterNeighbors :: Maze -> [(Int, Int)] -> [(Int, Int)]
filterNeighbors maze pos =
  let firstPass = filter (`elem` map (\c -> (x c, y c)) (cells maze)) pos
   in filter (\c -> not (getCellWallVal c maze)) firstPass

extractBest :: Maze -> [(Int, Int)] -> Cell
extractBest m pos =
  let cup = (x (goal m), y (goal m))
   in let distances = map (\c -> (c, getManhattanDistance cup c)) pos
       in getMin distances

getMin :: [((Int, Int), Int)] -> Cell
getMin [] = Cell {x = -1, y = -1, isWall = True}
getMin lst = case List.sortBy (comparing snd) lst of
  (l@((a, b), c) : ls) -> Cell {x = a, y = b, isWall = False}
  _ -> Cell {x = -1, y = -1, isWall = True}

getManhattanDistance :: (Int, Int) -> (Int, Int) -> Int
getManhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- helper to switch player
switchPlayer :: Player -> Player
switchPlayer One = Two
switchPlayer Two = One

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

getCellWallVal :: (Int, Int) -> Maze -> Bool
getCellWallVal tup maze =
  let fCells = filter (\c -> (x c, y c) == tup) (cells maze)
   in case fCells of
        [] -> True
        (x : xs) -> isWall x

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

playMaze :: IO ()
playMaze = do
  putStrLn "Welcome to The Wizard Maze!"
  putStrLn "Your Goal is to get to the Tri-wizard cup before your opponent taking turns to move"
  putStrLn
    "There are some components of the Maze you should be aware of: \n \
    \ - Coins (indicated by Q): collecting a coin adds 1 to your score \n \
    \ - Portals (indicated by P): beware: portals can transport you anywhere in the Maze \n \
    \ - Compasses (indicated by C): collecting a compass adds 1 to your score & takes you a step closer to the goal"
  playGame initialGame

playGame :: Game -> IO ()
playGame emptyGame = do
  putStrLn "please enter your desired difficulty level(easy, medium, or hard)"
  putStr ">>> "
  difficulty <- getLine
  case difficulty of
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
    getMaze initMaze fName numCoins numCompasses =
      case initMaze of
        Just (rawMaze, s) -> do
          portalsRes <- P.parseFromFile P.portalFileParser fName
          case portalsRes of
            Just (portalsList, _) -> do
              -- putStrLn (show (portalsList)) -- here for debugging purposes
              let maze = parseMaze rawMaze
               in let updatedMaze = maze {portals = portalsList}
                   in let updatedCoins = P.addCoinsToMazeRandom numCoins updatedMaze
                       in let updatedCompasses = P.addCompassesToMazeRandom numCompasses updatedCoins
                           in let game = initializeGameState emptyGame updatedCompasses
                               in do
                                    -- print (board game) -- here for debugging purposes
                                    go game
            _ -> do
              putStrLn "error loading maze, please try again later"
              return ()
        _ -> do
          putStrLn "error loading maze, please try again later"
          return ()
    go game =
      do
        let curPlayer = current game
         in do
              putStrLn "------ Player One Info: -------"
              putStrLn (printPlayerAttributes (player1 (playerInfo game)))
              putStrLn "------ Player Two Info: -------"
              putStrLn (printPlayerAttributes (player2 (playerInfo game)))
              putStrLn "-------------------------------\n"
              putStr ("Player " ++ getPlayerStr curPlayer ++ "'s Turn >>> ")
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
                              putStrLn ("Congrats Player " ++ getPlayerStr p ++ "! You Won the TriWizard Cup!") -- cur player wins!
                              return ()
                            else
                              if c `elem` coins (board g)
                                then do
                                  let g' = S.execState (collectCoin c p) g
                                   in go g'
                                else
                                  if c `elem` getPortalEntrances (board g)
                                    then do
                                      let g' = S.execState (enterPortal c p) g
                                       in go g'
                                    else
                                      if c `elem` compasses (board g)
                                        then do
                                          let g' = S.execState (collectCompass c p) g
                                           in go g'
                                        else do
                                          let g' = S.execState (moveOne c p) g
                                           in go g'
                        else do
                          putStrLn "something went wrong, please try again later" -- do something if player isn't current
                          return ()
                else do
                  putStrLn "you are trying to move outside of the maze, please choose a valid direction" -- error trying to move outside of board?
                  go g

getPlayerStr :: Player -> String
getPlayerStr One = "One"
getPlayerStr Two = "Two"

printPlayerAttributes :: Attributes -> String
printPlayerAttributes attrs =
  "Points: "
    ++ show (numPoints attrs)
    ++ "\n \
       \ Position in Maze: x="
    ++ show (x (position attrs))
    ++ ", y="
    ++ show (y (position attrs))

--- >>> getMin [((1, 2), 3), ((0, 1), 4), ((23, 1), -1)]
-- Cell {x = 23, y = 1, isWall = False}
