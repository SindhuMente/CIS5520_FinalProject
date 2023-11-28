module MazeTypes where

import Control.Applicative (Alternative (..))
import System.IO
import Prelude hiding (filter)

data Cell = Cell
  { x :: Int,
    y :: Int,
    isWall :: Bool
  }
  deriving (Show)

data Portal = Portal Cell Cell deriving (Show)

data Maze = Maze
  { maze :: [Cell],
    startPlayerOne :: Cell,
    startPlayerTwo :: Cell,
    goal :: Cell,
    coins :: [Cell],
    compasses :: [Cell],
    portals :: [Portal]
  }
  deriving (Show)

-- >>> doParse maze "0Hello World"
-- Variable not in scope:
--   doParse :: (Maze -> [Cell]) -> String -> t_abhe[sk:1]
