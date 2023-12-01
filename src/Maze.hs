module Maze where

import Control.Applicative (Alternative (..))
import Data.Maybe
-- import Dfs
import ParserCombinators (Parser, char, doParse, filterP, parse, satisfy, string)
import System.IO
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
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
    portals :: [Portal]
  }
  deriving (Eq, Show)

data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Eq, Show)

------------------------------------------------------------------------------------------

-- | Part 1: Parsing the text file

------------------------------------------------------------------------------------------

-- | Only the following characters are allowed: 1, 0, Q, C, P, G, S, T
allowed :: Char -> Bool
allowed c = c `elem` ['0', '1', 'Q', 'C', 'P', 'G', 'S', 'T']

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

-- Add a portal to the Maze
addPortal :: Int -> Int -> Maze -> Maze
addPortal row col maze = undefined

-- Parse a single character
parseChar :: (Char, Int) -> Int -> Int -> Maze -> Maze -- still need to add portals
parseChar (c, col) row maze =
  case c of
    '0' -> addCell row col True
    '1' -> addCell row col False
    'Q' -> addCell row col False . addCoin row col
    'C' -> addCell row col False . addCompass row col
    'S' -> addCell row col False . setStartPlayerOne row col
    'T' -> addCell row col False . setStartPlayerTwo row col
    'G' -> addCell row col False . setGoal row col
    _ -> error "invalid maze" -- still need to add portal logic

-- Parse a row of the maze add keep track of its column index starting from 0
parseRow :: String -> Int -> Int -> Maze -> Maze
parseRow rows row col maze =
  foldr (\c m -> parseChar c row col m) maze (zip rows [0 ..])

initialMaze :: Maze
initialMaze = Maze [] (Cell 0 0 False) (Cell 0 0 False) (Cell 0 0 False) [] [] []

-- Parse the entire maze and construct the Maze data structure
parseMaze :: [String] -> Maze
parseMaze rows =
  foldr (\(row, rowIdx) m -> parseRow row rowIdx 0 m) initialMaze (zip rows [0 ..])

------------------------------------------------------------------------------------------

-- | Part 3:  Testing

------------------------------------------------------------------------------------------

-- | a maze file should only contain valid characters
test_file_has_valid_characters :: Test
test_file_has_valid_characters =
  "valid characters" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> assert $ all (all (`elem` ['0', '1', 'Q', 'C', 'P', 'G', 'S', 'T'])) mazeString
      Nothing -> assert False

-- | a game should have exactly 1 goal
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

-- | a game should have 2 starting points, 1 for each player
test_two_starting_points :: Test
test_two_starting_points =
  "two starting points" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ countStartingPoints maze == 2
      Nothing -> assert False

test_easy_size :: Test
test_easy_size =
  "easy level size" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        print maze
        assert $ length (cells maze) == 99
      Nothing -> assert False

test_easy_goal_pos :: Test
test_easy_goal_pos =
  "easy level goal pos" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        print maze
        assert $ goal maze == Cell 2 8 False
      Nothing -> assert False

-- Function to count the number of starting points in a maze
countStartingPoints :: Maze -> Int
countStartingPoints maze =
  length $ filter (\cell -> cell == startPlayerOne maze || cell == startPlayerTwo maze) (cells maze)

test_goal_reachability :: Test
test_goal_reachability = undefined

test_all :: IO Counts
test_all =
  runTestTT $
    TestList
      [ test_file_has_valid_characters,
        test_exactly_one_goal,
        test_two_starting_points,
        test_easy_size,
        test_easy_goal_pos
      ]

-- >>> test_all
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}
