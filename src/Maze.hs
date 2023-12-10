module Maze where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, foldM, liftM, replicateM, replicateM_, unless, when)
-- import Dfs

import DList qualified as D
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as Set
import ParserCombinators (Parser, alpha, between, char, choice, digit, doParse, filterP, int, parse, satisfy, sepBy, space, string)
import State (State)
import State qualified as S
import System.IO
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, randomIO, randomR, uniform, uniformR)
import Test.HUnit (Assertion, Counts, Test (..), Testable (test), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Prelude

------------------------------------------------------------------------------------------

-- | Part 0: Maze data types

------------------------------------------------------------------------------------------

data Cell = Cell
  { x :: Int,
    y :: Int,
    isWall :: Bool
  }
  deriving (Eq, Show)

data Maze = Maze
  { cells :: [Cell],
    startPlayerOne :: Cell,
    startPlayerTwo :: Cell,
    goal :: Cell,
    coins :: [Cell],
    compasses :: [Cell],
    portals :: [Portal],
    rows :: Int,
    cols :: Int
  }
  deriving (Eq, Show)

data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Eq, Show)

------------------------------------------------------------------------------------------

-- | Part 1: Parsing the maze text file

------------------------------------------------------------------------------------------

-- | Only the following characters are allowed: 1, 0, G, S, T
-- the coins and compasses will be randomly generated while the portals will be read from a separate file
allowed :: Char -> Bool
allowed c = c `elem` ['0', '1', 'G', 'S', 'T']

-- | Create a parser that accepts allowed characters
allowedChar :: Parser Char
allowedChar = satisfy allowed

-- | Parse a maximal nonempty sequence of allowed characters:
text :: Parser String
text = some allowedChar

-- | Parsers for new line character
newLine :: Parser Char
newLine = char '\n'

-- | Parser combinator takes a parser, runs it, then skips over any newline characters occurring afterwards
newLineP :: Parser a -> Parser a
newLineP p = p <* many newLine

-- | Parse an entire ascii maze, remember to use `many`
mazeFileParser :: Parser String
mazeFileParser = newLineP text

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a, String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  return $ doParse parser str

------------------------------------------------------------------------------------------

-- | Part 2:  Building the Maze data structure

------------------------------------------------------------------------------------------

-- Add a cell to the Maze's list of cells
addCell :: Int -> Int -> Bool -> Maze -> Maze
addCell row col isWall maze = maze {cells = Cell row col isWall : cells maze}

-- Add a coin to the Maze
addCoin :: Int -> Int -> Maze -> Maze
addCoin row col maze = maze {coins = Cell row col False : coins maze}

-- Add a compass to the Maze
addCompass :: Int -> Int -> Maze -> Maze
addCompass row col maze = maze {compasses = Cell row col False : compasses maze}

-- Set the start position for player one in the Maze
setStartPlayerOne :: Int -> Int -> Maze -> Maze
setStartPlayerOne row col maze = maze {startPlayerOne = Cell row col False}

-- Set the start position for player one in the Maze
setStartPlayerTwo :: Int -> Int -> Maze -> Maze
setStartPlayerTwo row col maze = maze {startPlayerTwo = Cell row col False}

-- Set the goal position in the Maze
setGoal :: Int -> Int -> Maze -> Maze
setGoal row col maze = maze {goal = Cell row col False}

-- Parse a single character
parseChar :: (Char, Int) -> Int -> Int -> Maze -> Maze
parseChar (c, col) row maze =
  case c of
    '0' -> addCell row col True
    '1' -> addCell row col False
    'S' -> addCell row col False . setStartPlayerOne row col
    'T' -> addCell row col False . setStartPlayerTwo row col
    'G' -> addCell row col False . setGoal row col
    _ -> error "invalid maze"

-- Parse a row of the maze add keep track of its column index starting from 0
parseRow :: String -> Int -> Int -> Maze -> Maze
parseRow rows row col maze =
  foldr (\c m -> parseChar c row col m) maze (zip rows [0 ..])

initialMaze :: Maze
initialMaze = Maze [] (Cell 0 0 False) (Cell 0 0 False) (Cell 0 0 False) [] [] [] 0 0

-- Parse the entire maze and construct the Maze data structure
parseMaze :: [String] -> Maze
parseMaze rows =
  foldr (\(row, rowIdx) m -> parseRow row rowIdx 0 (addRowsCols m)) initialMaze (zip rows [0 ..])
  where
    addRowsCols maze = maze {rows = length rows, cols = length (head rows)}

------------------------------------------------------------------------------------------

-- | Part 3:  Parsing and adding portals

------------------------------------------------------------------------------------------

wsP :: Parser a -> Parser a
wsP p = p <* many (space <|> char ',')

portalP :: Parser [Portal]
portalP = do
  _ <- wsP (many alpha)
  numPortals <- wsP int
  replicateM numPortals (wsP parsePortal)

parsePortal :: Parser Portal
parsePortal = do
  entrance <- wsP parseCell
  exit <- wsP parseCell
  return $ Portal entrance exit

parseCell :: Parser Cell
parseCell = do
  char '('
  row <- int
  char ','
  col <- int
  char ')'
  return $ Cell row col False

portalFileParser :: Parser [Portal]
portalFileParser = portalP

------------------------------------------------------------------------------------------

-- | Part 4:  Random generation of coins

------------------------------------------------------------------------------------------

-- | check if a cell has a portal, compass or is a start/goal cell
isPortalOrCompassOrStartOrGoal :: Cell -> Maze -> Bool
isPortalOrCompassOrStartOrGoal cell maze =
  cell `elem` portalCells || cell `elem` compasses maze || cell == startPlayerOne maze || cell == startPlayerTwo maze || cell == goal maze
  where
    portalCells = concatMap (\portal -> [entrance portal, exit portal]) (portals maze)

-- | get all potential coin coordinates
getPotentialCoinCoordinates :: Maze -> [(Int, Int)]
getPotentialCoinCoordinates maze = go (cells maze) D.empty
  where
    go [] visited = D.toList visited
    go (cell : rest) visited =
      if isWall cell || isPortalOrCompassOrStartOrGoal cell maze
        then go rest visited
        else go rest (visited `D.append` D.singleton (x cell, y cell))

-- | given (x,y), return Cell data type at (x,y)
getCell :: Int -> Int -> Maze -> Maybe Cell
getCell x y maze = List.find (\(Cell x' y' _) -> x == x' && y == y') (cells maze)

mkStdGen' :: Int -> StdGen
mkStdGen' = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

-- | pick n coordinates for coins at random
randmonPickN :: StdGen -> Int -> [a] -> ([a], StdGen)
randmonPickN gen n coords = go gen n coords []
  where
    go g 0 _ acc = (reverse acc, g)
    go g k [] acc = (reverse acc, g)
    go g k lst acc =
      let (idx, g') = Random.randomR (0, length lst - 1) g
          (left, target : right) = splitAt idx lst
       in go g' (k - 1) (left ++ right) (target : acc)

addCoinsToMazeRandom :: Int -> Maze -> Maze
addCoinsToMazeRandom n maze =
  let (randomCoords, _) = randmonPickN (mkStdGen' 40) n (getPotentialCoinCoordinates maze)
   in foldr (\(x, y) m -> addCoin x y m) maze randomCoords

-- >>> getPotentialCoinCoordinates test_maze
-- [(0,0),(0,2),(0,7),(0,8),(0,9),(1,0),(1,1),(1,2),(1,4),(1,9),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(3,1),(3,2),(3,4),(3,5),(3,6),(3,7),(3,8),(4,2),(4,6),(5,1),(5,6),(5,7),(5,8),(6,0),(6,1),(6,2),(6,5),(6,6),(6,7),(7,2),(7,3),(7,4),(7,5),(8,3),(8,7),(8,10)]

-- >>>  randmonPickN (mkStdGen' 40) 5 (getPotentialCoinCoordinates test_maze)
-- ([(6,0),(7,4),(5,6),(3,7),(4,2)],StdGen {unStdGen = SMGen 17376949877490861412 5307070500861010077})

------------------------------------------------------------------------------------------

-- | Part 5:  Random generation of compasses

------------------------------------------------------------------------------------------

-- | check if a cell has a portal, compass or is a start/goal cell
isPortalOrCoinOrStartOrGoal :: Cell -> Maze -> Bool
isPortalOrCoinOrStartOrGoal cell maze =
  cell `elem` portalCells || cell `elem` coins maze || cell == startPlayerOne maze || cell == startPlayerTwo maze || cell == goal maze
  where
    portalCells = concatMap (\portal -> [entrance portal, exit portal]) (portals maze)

-- | get all potential coin coordinates
getPotentialCompassCoordinates :: Maze -> [(Int, Int)]
getPotentialCompassCoordinates maze = go (cells maze) D.empty
  where
    go [] visited = D.toList visited
    go (cell : rest) visited =
      if isWall cell || isPortalOrCoinOrStartOrGoal cell maze
        then go rest visited
        else go rest (visited `D.append` D.singleton (x cell, y cell))

-- addCompassesToMazeRandom :: Int -> Maze -> Maze
-- addCompassesToMazeRandom n maze =
--   let (randomCoords, _) = randmonPickN (mkStdGen' 40) n (getPotentialCoinCoordinates maze)
--    in foldr (\(x, y) m -> addCompass x y m) maze randomCoords

addCompassesToMazeRandom :: Int -> Maze -> Maze
addCompassesToMazeRandom n maze =
  let (randomCoords, _) = randmonPickN (mkStdGen' 40) n (getPotentialCompassCoordinates maze)
   in foldr (\(x, y) m -> addCompass x y m) maze randomCoords

------------------------------------------------------------------------------------------

-- | Part 6:  Testing

------------------------------------------------------------------------------------------

-- | a maze file should only contain valid characters
test_file_has_valid_characters :: Test
test_file_has_valid_characters =
  "valid characters" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> assert $ all (all (`elem` ['0', '1', 'Q', 'C', 'P', 'G', 'S', 'T'])) mazeString
      Nothing -> assert False

-- | a maze should have exactly 1 goal
test_exactly_one_goal :: Test
test_exactly_one_goal =
  "one goal" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ countGoals maze == 1
      Nothing -> assert False

countGoals :: Maze -> Int
countGoals maze = length $ filter (\cell -> cell == goal maze) (cells maze)

-- | a maze should have 2 starting points, 1 for each player
test_two_starting_points :: Test
test_two_starting_points =
  "two starting points" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ countStartingPoints maze == 2
      Nothing -> assert False

-- | an easy maze should be of size 99
test_easy_size :: Test
test_easy_size =
  "easy level size" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        assert $ length (cells maze) == 99
      Nothing -> assert False

-- | testing that goal pos was parsed correctly
test_easy_goal_pos :: Test
test_easy_goal_pos =
  "easy level goal pos" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        assert $ goal maze == Cell 2 8 False
      Nothing -> assert False

-- Helper unction to count the number of starting points in a maze
countStartingPoints :: Maze -> Int
countStartingPoints maze =
  length $ filter (\cell -> cell == startPlayerOne maze || cell == startPlayerTwo maze) (cells maze)

-- | a maze should have the correct dimensions
test_dimensions :: Test
test_dimensions =
  "maze dimensions" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        assert $ rows maze == 9 && cols maze == 11
      Nothing -> assert False

test_add_portals_easy :: Test
test_add_portals_easy =
  "adding portals easy" ~: do
    let portalsFile = "data/easy_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        -- print m
        assert (length m == 2)
      Nothing -> error "Failed to parse portals file"

test_add_portals_medium :: Test
test_add_portals_medium =
  "adding portals medium" ~: do
    let portalsFile = "data/medium_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        -- print m
        assert (length m == 3)
      Nothing -> error "Failed to parse portals file"

test_add_portals_hard :: Test
test_add_portals_hard =
  "adding portals hard" ~: do
    let portalsFile = "data/hard_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        -- print m
        assert (length m == 4)
      Nothing -> error "Failed to parse portals file"

test_add_portals_to_maze :: Test
test_add_portals_to_maze =
  "adding portals to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    portalsRes <- parseFromFile portalFileParser "data/easy_portals.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        -- print "************ adding portals ************"
        case portalsRes of
          Just (portalsList, _) -> do
            let updatedMaze = maze {portals = portalsList}
            -- print updatedMaze
            assert $ length (portals updatedMaze) == 2 && null (portals maze)
          Nothing -> error "Failed to parse portals file"
      Nothing -> assert False

test_get_eligible_coin_coordinates :: Test
test_get_eligible_coin_coordinates =
  "get eligible coin coordinates" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        print maze
        let coords = getPotentialCoinCoordinates maze
        print "\n"
        print coords
        assert $
          all
            ( \(x, y) ->
                let cell = getCell x y maze
                 in case cell of
                      Nothing -> False
                      Just c -> not $ isPortalOrCompassOrStartOrGoal c maze
            )
            coords
      Nothing -> assert False

test_random_add_coins_to_maze :: Test
test_random_add_coins_to_maze =
  "randomly add coins to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        let updatedMaze = addCoinsToMazeRandom 5 maze
        -- print "****************************"
        print updatedMaze
        assert $ length (coins updatedMaze) == 5
      Nothing -> assert False

test_get_eligible_compass_coordinates :: Test
test_get_eligible_compass_coordinates =
  "get eligible compass coordinates" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        print maze
        let coords = getPotentialCompassCoordinates maze
        print "\n"
        print coords
        assert $
          all
            ( \(x, y) ->
                let cell = getCell x y maze
                 in case cell of
                      Nothing -> False
                      Just c -> not $ isPortalOrCoinOrStartOrGoal c maze
            )
            coords
      Nothing -> assert False

test_random_add_compasses_to_maze :: Test
test_random_add_compasses_to_maze =
  "randomly add compasses to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        -- print maze
        let updatedMaze = addCompassesToMazeRandom 2 maze
        -- print "****************************"
        print updatedMaze
        assert $ length (compasses updatedMaze) == 2
      Nothing -> assert False

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_file_has_valid_characters,
        test_exactly_one_goal,
        test_two_starting_points,
        test_easy_size,
        test_easy_goal_pos,
        test_dimensions,
        test_add_portals_easy,
        test_add_portals_medium,
        test_add_portals_hard,
        test_add_portals_to_maze,
        test_get_eligible_coin_coordinates,
        test_random_add_coins_to_maze,
        test_get_eligible_compass_coordinates,
        test_random_add_compasses_to_maze
      ]

-- >>> test_all
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}

test_maze :: Maze
test_maze =
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
      compasses = [Cell {x = 7, y = 7, isWall = False}],
      portals = [],
      rows = 9,
      cols = 11
    }
