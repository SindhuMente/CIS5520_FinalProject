module Player where

-- import State (State)
-- import State qualified as S

import Control.Applicative
import Control.Monad (when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Maze
import System.IO
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

-- data Cell = Cell
--   { x :: Int,
--     y :: Int,
--     isWall :: Bool -- true if wall is at cell
--   }
--   deriving (Show, Eq)

-- data Maze = Maze
--   { cells :: [Cell],
--     startPlayerOne :: Cell,
--     startPlayerTwo :: Cell,
--     goal :: Cell,
--     coins :: [Cell],
--     compasses :: [Cell],
--     portals :: [Portal]
--   }
--   deriving (Eq, Show)

data Action = MUp | MDown | MLeft | MRight deriving (Eq, Show)

-- data Portal = Portal {entrance :: Cell, exit :: Cell} deriving (Eq, Show)

data Player = One | Two deriving (Eq, Ord, Show)

data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo} deriving (Show)

data Attributes = Attributes {numPoints :: Int, position :: Cell} deriving (Eq, Show)

type PlayerInfo = M.Map Player Attributes

data End = Win Player | Lose Player deriving (Eq, Show)

---------------- Functions ---------------

{- Note: All functions defined under the assumption that
there cannot be multiple elements on the same cell
Ex. a portal exit & coin cannot be on same cell
this is subject to change -}

mapActionToCell :: Game -> Player -> Action -> Cell
mapActionToCell g p a = case M.lookup p (playerInfo g) of
  Just attrs ->
    case a of
      MUp -> undefined
      MDown -> undefined
      MLeft -> undefined
      MRight -> undefined
  Nothing -> undefined -- error player not found?

makeMove :: Game -> Cell -> Player -> Game
makeMove g c p =
  if c `elem` cells (board g)
    then
      if isWall c
        then error "can't move there" -- do something if trying to move to wall
        else
          if p == current g
            then
              if c == goal (board g)
                then undefined -- cur player wins, what to return? - should we use Either Monad?
                else
                  if c `elem` coins (board g)
                    then collectCoin g c p
                    else
                      if c `elem` getPortalEntrances (board g)
                        then enterPortal g c p
                        else
                          if c `elem` compasses (board g)
                            then collectCompass g c p
                            else moveOne g c p
            else undefined -- do something if player isn't current
    else undefined -- error trying to move outside of board?

getPortalEntrances :: Maze -> [Cell]
getPortalEntrances maze = map entrance (portals maze)

exampleMazeP :: Maze
exampleMazeP =
  Maze
    { cells =
        [ Cell {x = 0, y = 0, isWall = False},
          Cell {x = 0, y = 1, isWall = False},
          Cell {x = 0, y = 2, isWall = False},
          Cell {x = 1, y = 0, isWall = False},
          Cell {x = 1, y = 1, isWall = False},
          Cell {x = 1, y = 2, isWall = False},
          Cell {x = 2, y = 0, isWall = False},
          Cell {x = 2, y = 1, isWall = False},
          Cell {x = 2, y = 2, isWall = False}
        ],
      startPlayerOne = Cell {x = 2, y = 1, isWall = False},
      startPlayerTwo = Cell {x = 1, y = 2, isWall = False},
      goal = Cell {x = 0, y = 0, isWall = False},
      coins = [],
      compasses = [],
      portals =
        [ Portal
            { entrance = Cell {x = 1, y = 1, isWall = False},
              exit = Cell {x = 0, y = 1, isWall = False}
            }
        ]
    }

test_getPortalEntrances :: Test
test_getPortalEntrances =
  "Portal Entrances tests" ~:
    TestList
      [ getPortalEntrances exampleMazeP ~?= [Cell {x = 1, y = 1, isWall = False}]
      ]

-- >>> runTestTT test_getPortalEntrances
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

exampleMazeM :: Maze
exampleMazeM =
  Maze
    { cells =
        [ Cell {x = 0, y = 0, isWall = True},
          Cell {x = 0, y = 1, isWall = False},
          Cell {x = 0, y = 2, isWall = False},
          Cell {x = 0, y = 3, isWall = False},
          Cell {x = 0, y = 4, isWall = False},
          Cell {x = 0, y = 5, isWall = False},
          Cell {x = 1, y = 0, isWall = False},
          Cell {x = 1, y = 1, isWall = False},
          Cell {x = 1, y = 2, isWall = True},
          Cell {x = 1, y = 3, isWall = True},
          Cell {x = 1, y = 4, isWall = False},
          Cell {x = 1, y = 5, isWall = True},
          Cell {x = 2, y = 0, isWall = False},
          Cell {x = 2, y = 1, isWall = True},
          Cell {x = 2, y = 2, isWall = True},
          Cell {x = 2, y = 3, isWall = False},
          Cell {x = 2, y = 4, isWall = False},
          Cell {x = 2, y = 5, isWall = True},
          Cell {x = 3, y = 0, isWall = False},
          Cell {x = 3, y = 1, isWall = True},
          Cell {x = 3, y = 2, isWall = False},
          Cell {x = 3, y = 3, isWall = False},
          Cell {x = 3, y = 4, isWall = True},
          Cell {x = 3, y = 5, isWall = True},
          Cell {x = 4, y = 0, isWall = False},
          Cell {x = 4, y = 1, isWall = False},
          Cell {x = 4, y = 2, isWall = False},
          Cell {x = 4, y = 3, isWall = False},
          Cell {x = 4, y = 4, isWall = True},
          Cell {x = 4, y = 5, isWall = True},
          Cell {x = 5, y = 0, isWall = False},
          Cell {x = 5, y = 1, isWall = False},
          Cell {x = 5, y = 2, isWall = False},
          Cell {x = 5, y = 3, isWall = False},
          Cell {x = 5, y = 4, isWall = True},
          Cell {x = 5, y = 5, isWall = True}
        ],
      startPlayerOne = Cell {x = 5, y = 2, isWall = False},
      startPlayerTwo = Cell {x = 0, y = 5, isWall = False},
      goal = Cell {x = 2, y = 0, isWall = False},
      coins =
        [ Cell {x = 0, y = 2, isWall = False},
          Cell {x = 2, y = 4, isWall = False},
          Cell {x = 4, y = 1, isWall = False}
        ],
      compasses = [],
      portals =
        [ Portal
            { entrance = Cell {x = 5, y = 0, isWall = False},
              exit = Cell {x = 3, y = 3, isWall = False}
            }
        ]
    }

initGame :: Game
initGame =
  Game
    { board = exampleMazeM,
      current = One,
      playerInfo =
        M.fromList
          [ (One, Attributes {numPoints = 0, position = Cell {x = 5, y = 2, isWall = False}}),
            (Two, Attributes {numPoints = 0, position = Cell {x = 0, y = 5, isWall = False}})
          ]
    }

-- Cell -> Player -> State Game ()
moveOne :: Game -> Cell -> Player -> Game
moveOne g c p =
  case M.lookup p (playerInfo g) of
    Just attrs ->
      let newPlayerInfo = M.insert p (attrs {position = c}) (playerInfo g)
       in g {playerInfo = newPlayerInfo, current = switchPlayer p}
    Nothing -> undefined -- error player not found?

test_moveOne :: Test
test_moveOne =
  "Move One tests" ~:
    TestList
      [ extractPosition One (moveOne initGame Cell {x = 5, y = 1, isWall = False} One) ~?= Cell {x = 5, y = 1, isWall = False}
      ]

-- >>> runTestTT test_moveOne
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

extractPosition :: Player -> Game -> Cell
extractPosition p pI =
  maybe
    Cell {x = -1, y = -1, isWall = True}
    position
    (M.lookup p (playerInfo pI))

-- assuming coin = 1 point
collectCoin :: Game -> Cell -> Player -> Game
collectCoin g c p =
  case M.lookup p (playerInfo g) of
    Just attrs ->
      let newPlayerInfo = M.insert p (Attributes {position = c, numPoints = numPoints attrs + 1}) (playerInfo g)
       in let updatedMaze = removeCoin (board g) c
           in Game {board = updatedMaze, playerInfo = newPlayerInfo, current = switchPlayer p}
    Nothing -> undefined -- error player not found?

test_collectCoin :: Test
test_collectCoin =
  "Collect Coin tests" ~:
    TestList
      [ coins (board (collectCoin (moveOne initGame Cell {x = 5, y = 1, isWall = False} One) Cell {x = 4, y = 1, isWall = False} One))
          ~?= [ Cell {x = 0, y = 2, isWall = False},
                Cell {x = 2, y = 4, isWall = False}
              ]
      ]

-- >>> runTestTT test_collectCoin
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

removeCoin :: Maze -> Cell -> Maze
removeCoin maze cell =
  let updatedCoins = Prelude.filter (/= cell) (coins maze)
   in maze {coins = updatedCoins}

test_removeCoin :: Test
test_removeCoin =
  "Collect Coin tests" ~:
    TestList
      [ coins (removeCoin (board initGame) Cell {x = 4, y = 1, isWall = False})
          ~?= [ Cell {x = 0, y = 2, isWall = False},
                Cell {x = 2, y = 4, isWall = False}
              ]
      ]

-- >>> runTestTT test_removeCoin
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

fetchPortal :: Game -> Cell -> Cell
fetchPortal g c =
  let myPortal = Prelude.filter (\p -> entrance p == c) (portals (board g))
   in case myPortal of
        [x] -> exit x
        _ -> undefined -- error? how to handle case where != 1 portals found?

test_fetchPortal :: Test
test_fetchPortal =
  "Fetch Portal tests" ~:
    TestList
      [ fetchPortal initGame Cell {x = 5, y = 0, isWall = False}
          ~?= Cell {x = 3, y = 3, isWall = False}
      ]

-- >>> runTestTT test_fetchPortal
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

enterPortal :: Game -> Cell -> Player -> Game
enterPortal g c p = case M.lookup p (playerInfo g) of
  Just attrs ->
    let exitPortal = fetchPortal g c
     in let newPlayerInfo = M.insert p (attrs {position = exitPortal}) (playerInfo g)
         in g {playerInfo = newPlayerInfo, current = switchPlayer p}
  Nothing -> undefined -- error player not found?

test_enterPortal :: Test
test_enterPortal =
  "Enter Portal tests" ~:
    TestList
      [ extractPosition One (enterPortal (moveOne initGame Cell {x = 5, y = 0, isWall = False} One) Cell {x = 5, y = 0, isWall = False} One)
          ~?= Cell {x = 3, y = 3, isWall = False}
      ]

-- >>> runTestTT test_enterPortal
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

collectCompass :: Game -> Cell -> Player -> Game
collectCompass g c p = case M.lookup p (playerInfo g) of
  Just attrs ->
    let newPosition = pointMe (board g) c p
     in let updatedMaze = removeCompass (board g) c
         in let newPlayerInfo = M.insert p (Attributes {position = c, numPoints = numPoints attrs + 1}) (playerInfo g)
             in let newGame = g {playerInfo = newPlayerInfo, board = updatedMaze}
                 in makeMove newGame c p -- we must move again according to the PointMe spell :)
  Nothing -> undefined -- error player not found?

test_collectCompass :: Test
test_collectCompass =
  "Collect Compass tests" ~:
    TestList
      [ compasses (board (collectCompass (moveOne initGame Cell {x = 4, y = 2, isWall = False} One) Cell {x = 4, y = 3, isWall = False} One))
          ~?= []
      ]

removeCompass :: Maze -> Cell -> Maze
removeCompass maze cell =
  let updatedCompasses = Prelude.filter (/= cell) (compasses maze)
   in maze {compasses = updatedCompasses}

test_removeCompass :: Test
test_removeCompass =
  "Collect Coin tests" ~:
    TestList
      [ compasses (removeCompass (board initGame) Cell {x = 4, y = 3, isWall = False})
          ~?= []
      ]

pointMe :: Maze -> Cell -> Player -> Cell -- maybe return type could be Action instead of Cell, not sure
pointMe = undefined -- some logic to calculate the Cell that is 1 step towards goal

-- helper to switch player
switchPlayer :: Player -> Player
switchPlayer One = Two
switchPlayer Two = One

test_switchPlayer :: Test
test_switchPlayer =
  "Switch Player tests" ~:
    TestList
      [ compasses (removeCompass (board initGame) Cell {x = 4, y = 3, isWall = False})
          ~?= []
      ]

--------------- Tests ---------------

prop_compassTokenValidity :: Game -> Game -> Bool
prop_compassTokenValidity g1 g2 = undefined

prop_turnValidity :: Game -> Player -> Action -> Bool
prop_turnValidity g p a = p == current g

prop_ActionValidity :: Game -> Player -> Action -> Bool
prop_ActionValidity g p a = p == current g && undefined
