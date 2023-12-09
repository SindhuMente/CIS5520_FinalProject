module Dfs where

import Data.List
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Prelude

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

-- | Check if there is a path between two cells
-- takes in a start cell, goal cell, maze, list of visited cells
doesPathExist :: Cell -> Cell -> Maze -> Bool
doesPathExist start goal maze =
  dfs start goal maze []

-- | Get adjacent cells
getAdjacentCells :: Cell -> Maze -> [Cell]
getAdjacentCells (Cell x y _) maze =
  filter
    (isCoordinateWithinBounds maze)
    [ Cell (x - 1) y (isCoordinateWall (x - 1) y maze),
      Cell (x + 1) y (isCoordinateWall (x + 1) y maze),
      Cell x (y - 1) (isCoordinateWall x (y - 1) maze),
      Cell x (y + 1) (isCoordinateWall x (y + 1) maze)
    ]

-- | Check if a coordinate is within bounds of the maze
isCoordinateWithinBounds :: Maze -> Cell -> Bool
isCoordinateWithinBounds maze (Cell x y _) =
  x >= 0 && x < cols maze && y >= 0 && y < rows maze

-- |  Given (x,y), check if Cell at (x,y) is a wall
isCoordinateWall :: Int -> Int -> Maze -> Bool
isCoordinateWall x y maze =
  case find (\(Cell x' y' _) -> x == x' && y == y') (cells maze) of
    Just (Cell _ _ isWall') -> isWall'
    Nothing -> False

-- | Neighbor = a cell you can go to next
-- | Adjacent Cell = a cell beside current cell (regardless of type)
-- | Portal logic itself still TBD
-- | If one of the adjacent cells is a portal, neighbor would be the associated exit
-- | Therefore, once portal logic is set, need to make sure an exit is never a wall
getNeighbors :: Cell -> Maze -> [Cell]
getNeighbors cell maze =
  filter (not . isWall) $ getAdjacentCells cell maze -- incomplete

-- | dfs function takes in a start cell, goal cell, maze, list of visited cells
dfs :: Cell -> Cell -> Maze -> [Cell] -> Bool
dfs curr goal maze visited
  | curr == goal = True
  | curr `elem` visited = False
  | isWall curr = False
  | otherwise = any (\neighbor -> dfs neighbor goal maze (curr : visited)) (getNeighbors curr maze)

-- | Get exit cell of this portal
getPortalExit :: Cell -> Maze -> Maybe Cell
getPortalExit maybeEntrance maze =
  case find (\portal -> maybeEntrance == entrance portal) (portals maze) of
    Just portal -> Just (exit portal)
    Nothing -> Nothing

-- | Get exit cells of all portals in the maze
getPortalExits :: Maze -> [Cell]
getPortalExits maze = concatMap (\(Portal _ exitCell) -> [exitCell]) (portals maze)
