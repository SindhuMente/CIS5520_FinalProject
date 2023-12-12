module Maze where

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

------------------------------------------------------------------------------------------

-- | Part 7:  HUnit Testing

------------------------------------------------------------------------------------------

-- | a maze file should only contain valid characters
test_file_has_valid_characters :: Test
test_file_has_valid_characters =
  "valid characters" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> assert $ not $ null mazeString && all (all (`elem` ['0', '1', 'Q', 'C', 'P', 'G', 'S', 'T'])) mazeString
      Nothing -> assert False

-- | a maze should have exactly 1 goal
test_exactly_one_goal :: Test
test_exactly_one_goal =
  "one goal" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ not $ null mazeString && countGoals maze == 1
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
        assert $ not $ null mazeString && countStartingPoints maze == 2
      Nothing -> assert False

-- | an easy maze should be of size 99
test_easy_size :: Test
test_easy_size =
  "easy level size" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ not $ null mazeString && length (cells maze) == 99
      Nothing -> assert False

-- | testing that goal pos was parsed correctly
test_easy_goal_pos :: Test
test_easy_goal_pos =
  "easy level goal pos" ~: do
    res <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case res of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ not $ null mazeString && goal maze == Cell 2 8 False
      Nothing -> assert False

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
        assert $ not $ null mazeString && rows maze == 9 && cols maze == 11
      Nothing -> assert False

test_add_portals_easy :: Test
test_add_portals_easy =
  "adding portals easy" ~: do
    let portalsFile = "data/easy_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        assert $ not $ null m && (length m == 2)
      Nothing -> error "Failed to parse portals file"

test_add_portals_medium :: Test
test_add_portals_medium =
  "adding portals medium" ~: do
    let portalsFile = "data/medium_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        assert $ not $ null m && (length m == 3)
      Nothing -> error "Failed to parse portals file"

test_add_portals_hard :: Test
test_add_portals_hard =
  "adding portals hard" ~: do
    let portalsFile = "data/hard_portals.txt"
    result <- parseFromFile portalFileParser portalsFile
    case result of
      Just (m, _) -> do
        assert $ not $ null m && (length m == 4)
      Nothing -> error "Failed to parse portals file"

test_add_portals_to_maze_easy :: Test
test_add_portals_to_maze_easy =
  "adding portals to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    portalsRes <- parseFromFile portalFileParser "data/easy_portals.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        case portalsRes of
          Just (portalsList, _) -> do
            let updatedMaze = maze {portals = portalsList}
            assert $ not $ null portalsList && length (portals updatedMaze) == 2 && null (portals maze)
          Nothing -> error "Failed to parse portals file"
      Nothing -> assert False

test_get_eligible_coin_coordinates :: Test
test_get_eligible_coin_coordinates =
  "get eligible coin coordinates" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        let coords = getPotentialCoinCoordinates maze
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
        let updatedMaze = addCoinsToMazeRandom 5 maze
        assert $ not $ null mazeString && length (coins updatedMaze) == 5
      Nothing -> assert False

test_get_eligible_compass_coordinates :: Test
test_get_eligible_compass_coordinates =
  "get eligible compass coordinates" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        let coords = getPotentialCompassCoordinates maze
        assert $
          not $
            null mazeString
              && all
                ( \(x, y) ->
                    let cell = getCell x y maze
                     in case cell of
                          Nothing -> False
                          Just c -> not $ isPortalOrCoinOrStartOrGoal c maze
                )
                coords
      Nothing -> assert False

test_random_add_compasses_to_maze_easy :: Test
test_random_add_compasses_to_maze_easy =
  "randomly add compasses to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/easy.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        let updatedMaze = addCompassesToMazeRandom 2 maze
        assert $ length (compasses updatedMaze) == 2
      Nothing -> assert False

test_random_add_compasses_to_maze_medium :: Test
test_random_add_compasses_to_maze_medium =
  "randomly add compasses to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/medium.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        let updatedMaze = addCompassesToMazeRandom 0 maze
        assert $ null (compasses updatedMaze)
      Nothing -> assert False

test_random_add_compasses_to_maze_hard :: Test
test_random_add_compasses_to_maze_hard =
  "randomly add compasses to maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/hard.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        let updatedMaze = addCompassesToMazeRandom 11 maze
        assert $ length (compasses updatedMaze) == 11
      Nothing -> assert False

test_medium_maze_size :: Test
test_medium_maze_size =
  "add medium maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/medium.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ length (cells maze) == 168
      Nothing -> assert False

test_hard_maze_size :: Test
test_hard_maze_size =
  "add hard maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/hard.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        assert $ length (cells maze) == 270
      Nothing -> assert False

test_add_portals_to_maze_hard :: Test
test_add_portals_to_maze_hard =
  "adding portals to hard maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/hard.txt"
    portalsRes <- parseFromFile portalFileParser "data/hard_portals.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        case portalsRes of
          Just (portalsList, _) -> do
            let updatedMaze = maze {portals = portalsList}
            assert $ length (portals updatedMaze) == 4 && null (portals maze)
          Nothing -> error "Failed to parse portals file"
      Nothing -> assert False

test_add_portals_to_maze_medium :: Test
test_add_portals_to_maze_medium =
  "adding portals to medium maze" ~: do
    mazeRes <- parseFromFile (many mazeFileParser) "data/medium.txt"
    portalsRes <- parseFromFile portalFileParser "data/medium_portals.txt"
    case mazeRes of
      Just (mazeString, _) -> do
        let maze = parseMaze mazeString
        case portalsRes of
          Just (portalsList, _) -> do
            let updatedMaze = maze {portals = portalsList}
            assert $ length (portals updatedMaze) == 3 && null (portals maze)
          Nothing -> error "Failed to parse portals file"
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
        test_add_portals_to_maze_easy,
        test_get_eligible_coin_coordinates,
        test_random_add_coins_to_maze,
        test_get_eligible_compass_coordinates,
        test_random_add_compasses_to_maze_easy,
        test_random_add_compasses_to_maze_medium,
        test_random_add_compasses_to_maze_hard,
        test_hard_maze_size,
        test_medium_maze_size,
        test_add_portals_to_maze_hard,
        test_add_portals_to_maze_medium
      ]

-- >>> test_all
-- Counts {cases = 20, tried = 20, errors = 0, failures = 0}

------------------------------------------------------------------------------------------

-- | Part 8:  Prop Testing

------------------------------------------------------------------------------------------

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

prop_AddCellShouldIncreaseMazeSizeByOne :: Cell -> Bool
prop_AddCellShouldIncreaseMazeSizeByOne cell =
  let maze = addCell (x cell) (y cell) (isWall cell) test_maze1
   in length (cells maze) == length (cells test_maze1) + 1

prop_AddCoinShouldIncreaseCoinsSizeByOne :: Cell -> Bool
prop_AddCoinShouldIncreaseCoinsSizeByOne cell =
  let maze = addCoin (x cell) (y cell) test_maze1
   in length (coins maze) == length (coins test_maze1) + 1

prop_AddCompassShouldIncreaseCompassesSizeByOne :: Cell -> Bool
prop_AddCompassShouldIncreaseCompassesSizeByOne cell =
  let maze = addCompass (x cell) (y cell) test_maze1
   in length (compasses maze) == length (compasses test_maze1) + 1

prop_SetStartPlayerOneShouldChangeStartPlayerOne :: Cell -> Bool
prop_SetStartPlayerOneShouldChangeStartPlayerOne cell =
  let maze = setStartPlayerOne (x cell) (y cell) test_maze1
   in startPlayerOne maze == Cell (x cell) (y cell) False

prop_SetStartPlayerTwoShouldChangeStartPlayerTwo :: Cell -> Bool
prop_SetStartPlayerTwoShouldChangeStartPlayerTwo cell =
  let maze = setStartPlayerTwo (x cell) (y cell) test_maze1
   in startPlayerTwo maze == Cell (x cell) (y cell) False

prop_SetGoalShouldChangeGoal :: Cell -> Bool
prop_SetGoalShouldChangeGoal cell =
  let maze = setGoal (x cell) (y cell) test_maze1
   in goal maze == Cell (x cell) (y cell) False

qc :: IO ()
qc = do
  putStrLn "QuickCheck tests"
  putStrLn "Add cell increase maze size by one"
  QC.quickCheck prop_AddCellShouldIncreaseMazeSizeByOne
  putStrLn "Add coin increase coins size by one"
  QC.quickCheck prop_AddCoinShouldIncreaseCoinsSizeByOne
  putStrLn "Add compass increase compasses size by one"
  QC.quickCheck prop_AddCompassShouldIncreaseCompassesSizeByOne
  putStrLn "Set start player one should change start player one"
  QC.quickCheck prop_SetStartPlayerOneShouldChangeStartPlayerOne
  putStrLn "Set start player two should change start player two"
  QC.quickCheck prop_SetStartPlayerTwoShouldChangeStartPlayerTwo
  putStrLn "Set goal should change goal"
  QC.quickCheck prop_SetGoalShouldChangeGoal

test_maze1 :: Maze
test_maze1 =
  Maze
    { cells =
        [ Cell {x = 0, y = 0, isWall = False},
          Cell {x = 0, y = 1, isWall = False},
          Cell {x = 0, y = 2, isWall = True},
          Cell {x = 0, y = 3, isWall = False},
          Cell {x = 1, y = 0, isWall = True},
          Cell {x = 1, y = 1, isWall = False},
          Cell {x = 1, y = 2, isWall = False},
          Cell {x = 1, y = 3, isWall = False},
          Cell {x = 2, y = 0, isWall = False},
          Cell {x = 2, y = 1, isWall = False},
          Cell {x = 2, y = 2, isWall = False},
          Cell {x = 2, y = 3, isWall = False},
          Cell {x = 3, y = 0, isWall = True},
          Cell {x = 3, y = 1, isWall = False},
          Cell {x = 3, y = 2, isWall = False},
          Cell {x = 3, y = 3, isWall = False}
        ],
      startPlayerOne = Cell {x = 3, y = 1, isWall = False},
      startPlayerTwo = Cell {x = 0, y = 3, isWall = False},
      goal = Cell {x = 0, y = 0, isWall = False},
      coins = [Cell {x = 2, y = 2, isWall = False}],
      compasses = [Cell {x = 1, y = 1, isWall = False}],
      portals =
        [ Portal
            { entrance = Cell {x = 2, y = 0, isWall = False},
              exit = Cell {x = 0, y = 1, isWall = False}
            }
        ],
      rows = 4,
      cols = 4
    }

smallTestMaze :: Maze
smallTestMaze =
  Maze
    { cells =
        [ Cell {x = 0, y = 0, isWall = False},
          Cell {x = 0, y = 1, isWall = False},
          Cell {x = 1, y = 0, isWall = False},
          Cell {x = 1, y = 1, isWall = False}
        ],
      startPlayerOne = Cell {x = 0, y = 0, isWall = False},
      startPlayerTwo = Cell {x = 1, y = 1, isWall = False},
      goal = Cell {x = 0, y = 1, isWall = False},
      coins = [],
      compasses = [],
      portals = [],
      rows = 2,
      cols = 2
    }
