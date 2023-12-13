module Maze
  ( Cell (..),
    Maze (..),
    Portal (..),
    addCell,
    addCoin,
    addCompass,
    addCoinsToMazeRandom,
    addCompassesToMazeRandom,
    allowed,
    allowedChar,
    cellToChar,
    chunksOf,
    getCell,
    getPotentialCoinCoordinates,
    getPotentialCompassCoordinates,
    initialMaze,
    isPortalOrCompassOrStartOrGoal,
    isPortalOrCoinOrStartOrGoal,
    mazeFileParser,
    parseFromFile,
    parseMaze,
    portalFileParser,
    setStartPlayerOne,
    setStartPlayerTwo,
    setGoal,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, foldM, liftM, replicateM, replicateM_, unless, when)
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
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Prelude

------------------------------------------------------------------------------------------

-- | Part 0: Maze data types & instances

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
  deriving (Eq)

data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Eq, Show)

instance Arbitrary Cell where
  arbitrary :: Gen Cell
  arbitrary = genCell

  shrink :: Cell -> [Cell]
  shrink (Cell x y isWall) = [Cell x' y' isWall | (x', y') <- shrink (x, y)]

instance Show Maze where
  show :: Maze -> String
  show = showMaze

-- 14 is max XCoord size (i.e, hard maze)
genXCoordinate :: Gen Int
genXCoordinate = QC.choose (0, 14)

-- 17 is max YCoord size (i.e, hard maze)
genYCoordinate :: Gen Int
genYCoordinate = QC.choose (0, 17)

genIsWall :: Gen Bool
genIsWall = QC.arbitrary

genCell :: Gen Cell
genCell = do
  x <- genXCoordinate
  y <- genYCoordinate
  Cell x y <$> genIsWall

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

addCompassesToMazeRandom :: Int -> Maze -> Maze
addCompassesToMazeRandom n maze =
  let (randomCoords, _) = randmonPickN (mkStdGen' 40) n (getPotentialCompassCoordinates maze)
   in foldr (\(x, y) m -> addCompass x y m) maze randomCoords

------------------------------------------------------------------------------------------

-- | Part 6:  Drawing the board

------------------------------------------------------------------------------------------

-- map a cell to its character representation
cellToChar :: Maze -> Cell -> Char
cellToChar maze cell
  | isWall cell = 'X'
  | cell == startPlayerOne maze = 'S'
  | cell == startPlayerTwo maze = 'T'
  | cell == goal maze = 'G'
  | cell `elem` coins maze = 'Q'
  | cell `elem` compasses maze = 'C'
  | cell `elem` portalCells = 'P'
  | otherwise = ' '
  where
    portalCells = concatMap (\portal -> [entrance portal]) (portals maze)

-- | convert a maze to a string representation
showMaze :: Maze -> String
showMaze maze =
  let border = "+" ++ concat (replicate (cols maze) "---+") ++ "\n"
      rowStrings = [concat ["| ", [cellToChar maze (fromMaybe (Cell 0 0 False) (getCell y x maze))], " "] | y <- [0 .. rows maze - 1], x <- [0 .. cols maze - 1]]
      formattedRows = List.intercalate ("\n" ++ border) (map concat (chunksOf (cols maze) rowStrings))
   in border ++ formattedRows ++ "\n" ++ border

-- | split a list into chunks of size n
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n
    build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
    build g = g (:) []
