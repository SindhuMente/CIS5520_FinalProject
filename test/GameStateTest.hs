module GameStateTest where

import GameState
import Maze
import State qualified as S
import Test.HUnit (Counts, Test)
import Test.HUnit.Base
import Test.HUnit.Text (runTestTT)
import Test.QuickCheck

------------------------------------------------------------------------------------------

-- | Part 1: Unit Testing

------------------------------------------------------------------------------------------

-- example game
testGame :: Game
testGame =
  Game
    { board =
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
          },
      current = One,
      playerInfo =
        PlayerInfo
          { player1 =
              Attributes
                { numPoints = 0,
                  position = Cell {x = 3, y = 1, isWall = False}
                },
            player2 =
              Attributes
                { numPoints = 0,
                  position = Cell {x = 0, y = 3, isWall = False}
                }
          }
    }

test_moveOne :: Test
test_moveOne =
  "moveOne tests"
    ~: TestList
      [ player1 (playerInfo (S.execState (moveOne Cell {x = 2, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 2, y = 1, isWall = False}},
        player2 (playerInfo (S.execState (moveOne Cell {x = 1, y = 3, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}},
        current (S.execState (moveOne Cell {x = 2, y = 1, isWall = False} One) testGame) ~?= Two,
        current (S.execState (moveOne Cell {x = 1, y = 3, isWall = False} Two) testGame) ~?= One
      ]

test_enterPortal :: Test
test_enterPortal =
  "enterPortal tests"
    ~: TestList
      [ player1 (playerInfo (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
        player2 (playerInfo (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
        current (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} One) testGame) ~?= Two,
        current (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} Two) testGame) ~?= One
      ]

test_fetchPortal :: Test
test_fetchPortal =
  "fetchPortal tests"
    ~: TestList
      [ fetchPortal testGame Cell {x = 2, y = 0, isWall = False} ~?= Cell {x = 0, y = 1, isWall = False},
        fetchPortal testGame Cell {x = 2, y = 3, isWall = False} ~?= Cell {x = -1, y = -1, isWall = True}
      ]

test_collectCoin :: Test
test_collectCoin =
  "collectCoin tests"
    ~: TestList
      [ player1 (playerInfo (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
        player2 (playerInfo (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
        current (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} One) testGame) ~?= Two,
        current (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame) ~?= One,
        coins (board (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= []
      ]

test_removeCoin :: Test
test_removeCoin =
  "removeCoin tests"
    ~: TestList
      [ coins (removeCoin (board testGame) Cell {x = 2, y = 2, isWall = False}) ~?= [],
        coins (removeCoin (board testGame) Cell {x = 2, y = 1, isWall = False}) ~?= [Cell {x = 2, y = 2, isWall = False}]
      ]

test_collectCompass :: Test
test_collectCompass =
  "collectCompass tests"
    ~: TestList
      [ player1 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
        player2 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
        current (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame) ~?= Two,
        current (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame) ~?= One,
        compasses (board (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame)) ~?= []
      ]

test_pointMe :: Test
test_pointMe =
  "pointMe tests"
    ~: TestList
      [ pointMe (board testGame) Cell {x = 1, y = 1, isWall = False} One ~?= Cell {x = 0, y = 1, isWall = False},
        pointMe (board testGame) Cell {x = 2, y = 2, isWall = False} Two ~?= Cell {x = 1, y = 2, isWall = False},
        pointMe (board testGame) Cell {x = 2, y = 0, isWall = False} One ~?= Cell {x = 2, y = 0, isWall = False}
      ]

test_filterNeighbors :: Test
test_filterNeighbors =
  "filterNeighbors tests"
    ~: TestList
      [ filterNeighbors (board testGame) [(0, 1), (2, 1), (1, 0), (1, 2)] ~?= [(0, 1), (2, 1), (1, 2)],
        filterNeighbors (board testGame) [] ~?= []
      ]

test_extractBest :: Test
test_extractBest =
  "extractBest tests"
    ~: TestList
      [ extractBest (board testGame) [(0, 1), (2, 1), (1, 0), (1, 2)] ~?= Cell {x = 0, y = 1, isWall = False},
        extractBest (board testGame) [] ~?= Cell {x = -1, y = -1, isWall = True},
        extractBest (board testGame) [(2, 0)] ~?= Cell {x = 2, y = 0, isWall = False}
      ]

test_getManhattanDistance :: Test
test_getManhattanDistance =
  "getManhattanDistance tests"
    ~: TestList
      [ getManhattanDistance (1, 1) (0, 0) ~?= 2,
        getManhattanDistance (2, 2) (2, 2) ~?= 0,
        getManhattanDistance (2, 0) (0, 1) ~?= 3
      ]

test_getMin :: Test
test_getMin =
  "getMin tests"
    ~: TestList
      [ getMin [((0, 0), 100), ((2, 1), 10), ((3, 2), 1), ((1, 3), 4), ((0, 2), 8)] ~?= Cell {x = 3, y = 2, isWall = False},
        getMin [((2, 1), 10)] ~?= Cell {x = 2, y = 1, isWall = False},
        getMin [] ~?= Cell {x = -1, y = -1, isWall = True},
        getMin [((2, 1), 1), ((3, 2), 1)] ~?= Cell {x = 2, y = 1, isWall = False}
      ]

test_removeCompass :: Test
test_removeCompass =
  "removeCompass tests"
    ~: TestList
      [ compasses (removeCompass (board testGame) Cell {x = 1, y = 1, isWall = False}) ~?= [],
        compasses (removeCompass (board testGame) Cell {x = 2, y = 1, isWall = False}) ~?= [Cell {x = 1, y = 1, isWall = False}]
      ]

test_whichMove :: Test
test_whichMove =
  "whichMove tests"
    ~: TestList
      [ player1 (playerInfo (S.execState (whichMove Cell {x = 1, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
        player2 (playerInfo (S.execState (whichMove Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
        player1 (playerInfo (S.execState (whichMove Cell {x = 2, y = 0, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
        player2 (playerInfo (S.execState (whichMove Cell {x = 1, y = 3, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}}
      ]

test_getPortalEntrances :: Test
test_getPortalEntrances =
  "getPortalEntrances tests"
    ~: TestList
      [ getPortalEntrances (board testGame) ~?= [Cell {x = 2, y = 0, isWall = False}]
      ]

test_updateInfo :: Test
test_updateInfo =
  "updatePosition tests"
    ~: TestList
      [ updateInfo One (playerInfo testGame) Cell {x = 2, y = 1, isWall = False} False ~?= (playerInfo testGame) {player1 = Attributes {numPoints = 0, position = Cell {x = 2, y = 1, isWall = False}}},
        updateInfo Two (playerInfo testGame) Cell {x = 1, y = 3, isWall = False} False ~?= (playerInfo testGame) {player2 = Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}}},
        updateInfo One (playerInfo testGame) Cell {x = 1, y = 1, isWall = False} True ~?= (playerInfo testGame) {player1 = Attributes {numPoints = 1, position = Cell {x = 1, y = 1, isWall = False}}},
        updateInfo Two (playerInfo testGame) Cell {x = 2, y = 2, isWall = False} True ~?= (playerInfo testGame) {player2 = Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}}}
      ]

test_getCellWallVal :: Test
test_getCellWallVal =
  "getCellWallVal tests"
    ~: TestList
      [ getCellWallVal (1, 1) (board testGame) ~?= False,
        getCellWallVal (3, 0) (board testGame) ~?= True
      ]

test_updatePlayerState :: Test
test_updatePlayerState =
  "updatePlayerState tests"
    ~: TestList
      [ startPlayerOne (board (S.execState (updatePlayerState One) testGame {playerInfo = (playerInfo testGame) {player1 = Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}}}})) ~?= Cell {x = 0, y = 1, isWall = False},
        startPlayerTwo (board (S.execState (updatePlayerState Two) testGame {playerInfo = (playerInfo testGame) {player2 = Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}}}})) ~?= Cell {x = 0, y = 1, isWall = False},
        startPlayerOne (board (S.execState (updatePlayerState Two) testGame {playerInfo = (playerInfo testGame) {player2 = Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}}}})) ~?= Cell {x = 3, y = 1, isWall = False},
        startPlayerTwo (board (S.execState (updatePlayerState One) testGame {playerInfo = (playerInfo testGame) {player1 = Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}}}})) ~?= Cell {x = 0, y = 3, isWall = False}
      ]

-- >>> runTestTT test_updatePlayerState
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

test_actionsToCell :: Test
test_actionsToCell =
  "actionsToCell tests"
    ~: TestList
      [ actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MUp ~?= Cell {x = 1, y = 2, isWall = False},
        actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MDown ~?= Cell {x = 3, y = 2, isWall = False},
        actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MLeft ~?= Cell {x = 2, y = 1, isWall = False},
        actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MRight ~?= Cell {x = 2, y = 3, isWall = False}
      ]

test_extractPosition :: Test
test_extractPosition =
  "extractPosition tests"
    ~: TestList
      [ extractPosition One testGame ~?= Cell {x = 3, y = 1, isWall = False},
        extractPosition Two testGame ~?= Cell {x = 0, y = 3, isWall = False}
      ]

test_initializeGameState :: Test
test_initializeGameState =
  "initializeGameState tests"
    ~: TestList
      [ initializeGameState initialGame (board testGame) ~?= testGame
      ]

test_allMethods :: IO Counts
test_allMethods =
  runTestTT $
    TestList
      [ test_moveOne,
        test_enterPortal,
        test_fetchPortal,
        test_collectCoin,
        test_removeCoin,
        test_collectCompass,
        test_pointMe,
        test_filterNeighbors,
        test_extractBest,
        test_getManhattanDistance,
        test_getMin,
        test_removeCompass,
        test_whichMove,
        test_getPortalEntrances,
        test_updateInfo,
        test_getCellWallVal,
        test_updatePlayerState,
        test_actionsToCell,
        test_extractPosition,
        test_initializeGameState
      ]

-- >>> addCoinsToMazeRandom 4 qcMaze
-- +---+---+---+---+---+---+---+---+---+---+---+

-- |   | X |   | X | S | X | X |   |   |   | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- |   |   |   | X |   | X | X | X | X |   | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- |   |   |   |   |   | P |   |   | G | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | X |   |   | X |   |   |   |   |   | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | X | X | Q | X | X | X |   | X | X | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | X |   | X | X | X | X |   |   | Q | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | P |   |   | X | X | Q |   |   | X | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | X | X |   |   |   |   | X |   | X | X | X
-- +---+---+---+---+---+---+---+---+---+---+---+
-- | X | X | X | Q | X | T | X |   | X | X |
-- +---+---+---+---+---+---+---+---+---+---+---+

-- >>> test_allMethods
-- Counts {cases = 61, tried = 61, errors = 0, failures = 0}

-- example maze
easyMaze :: Maze
easyMaze =
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
      coins = [Cell {x = 6, y = 5, isWall = False}, Cell {x = 8, y = 3, isWall = False}, Cell {x = 5, y = 8, isWall = False}],
      compasses = [Cell {x = 6, y = 5, isWall = False}],
      portals = [Portal {entrance = Cell {x = 6, y = 0, isWall = False}, exit = Cell {x = 4, y = 7, isWall = False}}, Portal {entrance = Cell {x = 2, y = 5, isWall = False}, exit = Cell {x = 1, y = 0, isWall = False}}],
      rows = 9,
      cols = 11
    }

-- example Game 2
easyGame :: Game
easyGame = initializeGameState initialGame easyMaze

-- >>> player1 (playerInfo (S.execState (collectCompass Cell {x = 6, y = 5, isWall = False} One) easyGame))
-- Attributes {numPoints = 1, position = Cell {x = 6, y = 6, isWall = False}}

-- >>> player1 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame))
-- Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}}

--- >>> getManhattanDistance (2, 8) (6, 6)
-- 6

-- >>> pointMe easyMaze Cell {x=6,y=5,isWall=False} One
-- Cell {x = 6, y = 6, isWall = False}

------------------------------------------------------------------------------------------

-- | Part 2: Property Based Testing

------------------------------------------------------------------------------------------

-- checks if the action we take ensures that we end up in the intended state after the game state is updated
prop_actionExpected :: Game -> Action -> Cell -> Bool
prop_actionExpected g MUp c = x (actionsToCell (board g) c MUp) == x c - 1 && y (actionsToCell (board g) c MUp) == y c
prop_actionExpected g MDown c = x (actionsToCell (board g) c MDown) == x c + 1 && y (actionsToCell (board g) c MDown) == y c
prop_actionExpected g MLeft c = x (actionsToCell (board g) c MLeft) == x c && y (actionsToCell (board g) c MLeft) == y c - 1
prop_actionExpected g MRight c = x (actionsToCell (board g) c MRight) == x c && y (actionsToCell (board g) c MRight) == y c + 1

-- checks to make sure that after a move, the number of coins&compass does not increase
prop_movePreservesCompassesAndCoins :: Game -> Action -> Cell -> Player -> Bool
prop_movePreservesCompassesAndCoins g a c p =
  let c' = S.execState (whichMove (actionsToCell (board g) c a) p) g
   in length (compasses (board c')) <= length (compasses (board g)) && length (coins (board c')) <= length (coins (board g))

-- next 2 props check that taking an action and then taking the reverse action ensures we end up in the inital cell
-- only for moveOne & collectCoin cases, since compasses & portals are nondeterministic & unidirectional
prop_moveOneBackExpected :: Game -> Action -> Cell -> Player -> Bool
prop_moveOneBackExpected g a c p =
  let g' = S.execState (moveOne (actionsToCell (board g) c a) p) g
   in let c' = extractPosition p g'
       in let a' = oppositeAction a
           in let g'' = S.execState (moveOne (actionsToCell (board g') c' a') p) g'
               in x (extractPosition p g'') == x c && y (extractPosition p g'') == y c

prop_moveCoinBackExpected :: Game -> Action -> Cell -> Player -> Bool
prop_moveCoinBackExpected g a c p =
  let g' = S.execState (collectCoin (actionsToCell (board g) c a) p) g
   in let c' = extractPosition p g'
       in let a' = oppositeAction a
           in let g'' = S.execState (collectCoin (actionsToCell (board g') c' a') p) g'
               in x (extractPosition p g'') == x c && y (extractPosition p g'') == y c

-- retrieves action in opposite direction
oppositeAction :: Action -> Action
oppositeAction MUp = MDown
oppositeAction MDown = MUp
oppositeAction MLeft = MRight
oppositeAction MRight = MLeft

-- checks to make sure that collecting a coin adds to the player's score
prop_coinAddsToScore :: Game -> Cell -> Player -> Bool
prop_coinAddsToScore g c One =
  let g' = S.execState (collectCoin c One) g
   in numPoints (player1 (playerInfo g')) == 1 + numPoints (player1 (playerInfo g))
prop_coinAddsToScore g c Two =
  let g' = S.execState (collectCoin c Two) g
   in numPoints (player2 (playerInfo g')) == 1 + numPoints (player2 (playerInfo g))

qcGameState :: IO ()
qcGameState = do
  putStrLn "prop_actionExpected"
  -- quickCheck (prop_actionExpected easyGame)
  quickCheck prop_actionExpected
  putStrLn "prop_movePreservesCompassesAndCoins"
  -- quickCheck (prop_movePreservesCompassesAndCoins easyGame)
  quickCheck prop_movePreservesCompassesAndCoins
  putStrLn "prop_moveOneBackExpected"
  -- quickCheck (prop_moveOneBackExpected easyGame)
  quickCheck prop_moveOneBackExpected
  putStrLn "prop_moveCoinBackExpected"
  -- quickCheck (prop_moveCoinBackExpected easyGame)
  quickCheck prop_moveCoinBackExpected
  putStrLn "prop_coinAddsToScore"
  -- quickCheck (prop_coinAddsToScore easyGame)
  quickCheck prop_coinAddsToScore
