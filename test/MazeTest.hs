module MazeTest where

import Data.Maybe
import GHC.Base (many)
import Lib
import Maze
  ( Cell (..),
    Maze (..),
    Portal (..),
    addCell,
    addCoin,
    addCoinsToMazeRandom,
    addCompass,
    addCompassesToMazeRandom,
    getCell,
    getPotentialCoinCoordinates,
    getPotentialCompassCoordinates,
    isPortalOrCoinOrStartOrGoal,
    isPortalOrCompassOrStartOrGoal,
    mazeFileParser,
    parseFromFile,
    parseMaze,
    portalFileParser,
    setGoal,
    setStartPlayerOne,
    setStartPlayerTwo,
  )
import ParserCombinators (Parser (..), parse)
import Test.HUnit
import Test.QuickCheck qualified as QC
import Prelude

-- instance Applicative Parser where
--   pure :: a -> Parser a
--   pure x = P $ \s -> Just (x, s)
--   (<*>) :: Parser (a -> b) -> Parser a -> Parser b
--   p1 <*> p2 = P $ \s -> do
--     (f, s') <- doParse p1 s
--     (x, s'') <- doParse p2 s'
--     return (f x, s'')

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
  -- putStrLn "QuickCheck tests"
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
