module Maze where

import Control.Applicative (Alternative (..))
import ParserCombinators (Parser, char, doParse, filter, parse, satisfy, string)
import System.IO
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Prelude hiding (filter)

------------------------------------------------------------------------------------------

-- | Part 0: Maze data types

------------------------------------------------------------------------------------------

data Cell = Cell
  { x :: Int,
    y :: Int,
    isWall :: Bool
  }
  deriving (Show)

data Maze = Maze
  { cells :: [Cell],
    startPlayerOne :: Cell,
    startPlayerTwo :: Cell,
    goal :: Cell,
    coins :: [Cell],
    compasses :: [Cell],
    portals :: [Portal]
  }
  deriving (Show)

-- data Portal = Portal Cell Cell deriving (Show)
data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Show)


data Player = Player {
  position :: Cell,
  tools :: [Attributes]
}

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

-- | Parse an entire ascii maze, rememeber to use `many`
mazeFileParser :: Parser String
mazeFileParser = newLineP text

-- | Sample string for testing
sampleString :: String
sampleString = "00Q0S00P000\n00001000000\nPQ1010101"

-- | Run a parser on a particular input file
parseFromFile :: Parser a -> String -> IO (Maybe (a, String))
parseFromFile parser filename = do
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  return $ doParse parser str

-- >>> parseFromFile (many mazeFileParser) "data/easy.txt"
-- Just (["00Q0S00P000","00001000000","00001111G00","0Q000011100","00000P10000","00000011Q00","00P00111000","0000010C000","00010T00000"],"")

-- >>> parseFromFile (many mazeFileParser) "data/medium.txt"
-- Just ([],"")

-- | Get maze in list form (list of strings where each string is a row of the maze)
extractMazeString :: IO (Maybe (a, String)) -> IO (Maybe a)
extractMazeString ioResult = do
  maybeResult <- ioResult
  case maybeResult of
    Just (a, _) -> return (Just a)
    Nothing -> return Nothing

------------------------------------------------------------------------------------------

-- | Part 2:  Building the Maze data structure

------------------------------------------------------------------------------------------

{-
data Maze = Maze
  { cells :: [Cell],
    startPlayerOne :: Cell,
    startPlayerTwo :: Cell,
    goal :: Cell,
    coins :: [Cell],
    compasses :: [Cell],
    portals :: [Portal]
  }
  deriving (Show)
-}

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
    'Q' -> addCoin row col
    'C' -> addCell row col False . addCompass row col
    'S' -> addCell row col False . setStartPlayerOne row col
    'T' -> addCell row col False . setStartPlayerTwo row col
    'G' -> addCell row col False . setGoal row col
    _ -> undefined

-- Parse a row of the maze add keep track of its column index starting from 0
parseRow :: String -> Int -> Int -> Maze -> Maze
parseRow line row col maze =
  foldr f maze (zip line [0 ..])

initialMaze :: Maze
initialMaze = Maze [] (Cell 0 0 False) (Cell 0 0 False) (Cell 0 0 False) [] [] []

-- Parse the entire maze and construct the Maze data structure
parseMaze :: [String] -> Maze
parseMaze rows =
  foldr

------------------------------------------------------------------------------------------

-- | Part 3:  Testing

------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------

-- | Updates

{-

instead of list of strings, have list of tuples where
- an element would be (row, rowIdx)
- then take this and do the column stuff

Portals:
- maze would have no Portals
- randomGen should pick some set of 1s as entrances, and
pick others to be destinations:
- need to consider before which are correct destinations to avoid infinite loops

-}
------------------------------------------------------------------------------------------



