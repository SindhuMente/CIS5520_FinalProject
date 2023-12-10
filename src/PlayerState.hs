-- module PlayerState where

-- import Control.Applicative
-- import Control.Arrow (Arrow (first))
-- import Control.Monad (when)
-- import Data.List qualified as List
-- import Data.Map (Map, (!?))
-- import Data.Map qualified as M
-- import Data.Maybe (fromMaybe)
-- import Data.Ord (comparing)
-- import Maze
-- import Maze qualified as P
-- import State (State)
-- import State qualified as S
-- import System.IO
-- import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
-- import Test.QuickCheck qualified as QC
-- import Text.Read (readMaybe)

-- ------------------------------------------------------------------------------------------

-- -- | Part 0: PlayerState Data Types

-- ------------------------------------------------------------------------------------------

-- data Action = MUp | MDown | MLeft | MRight deriving (Eq, Show)

-- data Player = One | Two deriving (Eq, Ord, Show)

-- data Game = Game {board :: Maze, current :: Player, playerInfo :: PlayerInfo} deriving (Eq, Show)

-- data Attributes = Attributes {numPoints :: Int, position :: Cell} deriving (Eq, Show)

-- data PlayerInfo = PlayerInfo {player1 :: Attributes, player2 :: Attributes} deriving (Eq, Show)

-- data End = Win Player | Lose Player deriving (Eq, Show)

-- ------------------------------------------------------------------------------------------

-- -- | Part 1: Moving to an Empty Cell with no special Tokens

-- ------------------------------------------------------------------------------------------

-- moveOne :: Cell -> Player -> State Game ()
-- moveOne c p = do
--   g <- S.get
--   let newPlayerInfo = updatePosition p (playerInfo g) c
--    in S.put (g {playerInfo = newPlayerInfo, current = switchPlayer p})

-- ------------------------------------------------------------------------------------------

-- -- | Part 2: Moving to a Cell with a Portal & Entering it

-- ------------------------------------------------------------------------------------------

-- enterPortal :: Cell -> Player -> State Game ()
-- enterPortal c p = do
--   g <- S.get
--   let exitPortal = fetchPortal g c
--    in let newPlayerInfo = updatePosition p (playerInfo g) exitPortal
--        in S.put (g {playerInfo = newPlayerInfo, current = switchPlayer p})

-- fetchPortal :: Game -> Cell -> Cell
-- fetchPortal g c =
--   let myPortal = Prelude.filter (\p -> entrance p == c) (portals (board g))
--    in case myPortal of
--         [x] -> exit x
--         _ -> Cell {x = -1, y = -1, isWall = True} -- error? how to handle case where != 1 portals found?

-- ------------------------------------------------------------------------------------------

-- -- | Part 3: Moving to a Cell with a Coin on it

-- ------------------------------------------------------------------------------------------

-- -- assuming coin = 1 point
-- collectCoin :: Cell -> Player -> State Game ()
-- collectCoin c p =
--   do
--     g <- S.get
--     let newPlayerInfo = updatePoints p (playerInfo g) c
--      in let updatedMaze = removeCoin (board g) c
--          in S.put Game {board = updatedMaze, playerInfo = newPlayerInfo, current = switchPlayer p}

-- removeCoin :: Maze -> Cell -> Maze
-- removeCoin maze cell =
--   let updatedCoins = Prelude.filter (/= cell) (coins maze)
--    in maze {coins = updatedCoins}

-- ------------------------------------------------------------------------------------------

-- -- | Part 4: Moving to a Cell with a Compass on it

-- ------------------------------------------------------------------------------------------

-- collectCompass :: Cell -> Player -> State Game ()
-- collectCompass c p = do
--   g <- S.get
--   let newPosition = pointMe (board g) c p
--    in let updatedMaze = removeCompass (board g) c
--        in let newPlayerInfo = updatePoints p (playerInfo g) c
--            in S.put (S.execState (whichMove newPosition p) (g {playerInfo = newPlayerInfo, board = updatedMaze, current = p}))

-- -- in do
-- -- S.put (g {playerInfo = newPlayerInfo, board = updatedMaze})
-- -- whichMove newPosition p -- we must move again according to the PointMe spell :)

-- pointMe :: Maze -> Cell -> Player -> Cell
-- pointMe maze c p =
--   let (cX, cY) = (x c, y c)
--    in let possibleNeighbors = [(cX - 1, cY), (cX + 1, cY), (cX, cY - 1), (cX, cY + 1)]
--        in let validNeighbors = filterNeighbors maze possibleNeighbors ++ [(cX, cY)]
--            in extractBest maze validNeighbors

-- filterNeighbors :: Maze -> [(Int, Int)] -> [(Int, Int)]
-- filterNeighbors maze pos =
--   let firstPass = filter (`elem` map (\c -> (x c, y c)) (cells maze)) pos
--    in filter (\c -> not (getCellWallVal c maze)) firstPass

-- extractBest :: Maze -> [(Int, Int)] -> Cell
-- extractBest m pos =
--   let cup = (x (goal m), y (goal m))
--    in let distances = map (\c -> (c, getManhattanDistance cup c)) pos
--        in getMin distances

-- getManhattanDistance :: (Int, Int) -> (Int, Int) -> Int
-- getManhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- getMin :: [((Int, Int), Int)] -> Cell
-- getMin [] = Cell {x = -1, y = -1, isWall = True}
-- getMin lst = case List.sortBy (comparing snd) lst of
--   (l@((a, b), c) : ls) -> Cell {x = a, y = b, isWall = False}
--   _ -> Cell {x = -1, y = -1, isWall = True}

-- removeCompass :: Maze -> Cell -> Maze
-- removeCompass maze cell =
--   let updatedCompasses = Prelude.filter (/= cell) (compasses maze)
--    in maze {compasses = updatedCompasses}

-- whichMove :: Cell -> Player -> State Game ()
-- whichMove c p = do
--   g <- S.get
--   if c `elem` coins (board g)
--     then collectCoin c p
--     else
--       if c `elem` getPortalEntrances (board g)
--         then enterPortal c p
--         else
--           if c `elem` compasses (board g)
--             then collectCompass c p
--             else moveOne c p

-- getPortalEntrances :: Maze -> [Cell]
-- getPortalEntrances maze = map entrance (portals maze)

-- ------------------------------------------------------------------------------------------

-- -- | Part 5: Helper Functions for Moving

-- ------------------------------------------------------------------------------------------

-- updatePosition :: Player -> PlayerInfo -> Cell -> PlayerInfo
-- updatePosition One pI c =
--   let p1 = player1 pI
--    in pI {player1 = p1 {position = c}}
-- updatePosition Two pI c =
--   let p2 = player2 pI
--    in pI {player2 = p2 {position = c}}

-- updatePoints :: Player -> PlayerInfo -> Cell -> PlayerInfo
-- updatePoints One pI c =
--   let p1 = player1 pI
--    in pI {player1 = p1 {position = c, numPoints = numPoints p1 + 1}}
-- updatePoints Two pI c =
--   let p2 = player2 pI
--    in pI {player2 = p2 {position = c, numPoints = numPoints p2 + 1}}

-- -- helper to switch current player after each turn
-- switchPlayer :: Player -> Player
-- switchPlayer One = Two
-- switchPlayer Two = One

-- getCellWallVal :: (Int, Int) -> Maze -> Bool
-- getCellWallVal tup maze =
--   let fCells = filter (\c -> (x c, y c) == tup) (cells maze)
--    in case fCells of
--         [] -> True
--         (x : xs) -> isWall x

-- ------------------------------------------------------------------------------------------

-- -- | Part 6: Game Logic

-- ------------------------------------------------------------------------------------------

-- actionsToCell :: Maze -> Cell -> Action -> Cell
-- actionsToCell maze pos MLeft =
--   let newX = x pos
--    in let newY = y pos - 1
--        in let wallVal = getCellWallVal (newX, newY) maze
--            in Cell {x = newX, y = newY, isWall = wallVal}
-- actionsToCell maze pos MRight =
--   let newX = x pos
--    in let newY = y pos + 1
--        in let wallVal = getCellWallVal (newX, newY) maze
--            in Cell {x = newX, y = newY, isWall = wallVal}
-- actionsToCell maze pos MUp =
--   let newX = x pos - 1
--    in let newY = y pos
--        in let wallVal = getCellWallVal (newX, newY) maze
--            in Cell {x = newX, y = newY, isWall = wallVal}
-- actionsToCell maze pos MDown =
--   let newX = x pos + 1
--    in let newY = y pos
--        in let wallVal = getCellWallVal (newX, newY) maze
--            in Cell {x = newX, y = newY, isWall = wallVal}

-- extractPosition :: Player -> Game -> Cell
-- extractPosition p g =
--   if p == One
--     then position (player1 (playerInfo g))
--     else position (player2 (playerInfo g))

-- getPlayerStr :: Player -> String
-- getPlayerStr One = "One"
-- getPlayerStr Two = "Two"

-- printPlayerAttributes :: Attributes -> String
-- printPlayerAttributes attrs =
--   "Points: "
--     ++ show (numPoints attrs)
--     ++ "\n \
--        \ Position in Maze: x="
--     ++ show (x (position attrs))
--     ++ ", y="
--     ++ show (y (position attrs))

-- initializeGameState :: Game -> Maze -> Game
-- initializeGameState g m =
--   let p1 = startPlayerOne m
--    in let p2 = startPlayerTwo m
--        in let pI =
--                 PlayerInfo
--                   { player1 = Attributes {numPoints = 0, position = p1},
--                     player2 = Attributes {numPoints = 0, position = p2}
--                   }
--            in g {board = m, playerInfo = pI}

-- initialGame :: Game
-- initialGame =
--   Game
--     { board =
--         Maze
--           { cells = [],
--             startPlayerOne = Cell {x = -1, y = -1, isWall = True},
--             startPlayerTwo = Cell {x = -1, y = -1, isWall = True},
--             goal = Cell {x = -1, y = -1, isWall = True},
--             coins = [],
--             compasses = [],
--             portals = [],
--             rows = 1,
--             cols = 1
--           },
--       current = One,
--       playerInfo =
--         PlayerInfo
--           { player1 = Attributes {numPoints = 0, position = Cell {x = -1, y = -1, isWall = True}},
--             player2 = Attributes {numPoints = 0, position = Cell {x = -1, y = -1, isWall = True}}
--           }
--     }

-- playMaze :: IO ()
-- playMaze = do
--   putStrLn "Welcome to The Wizard Maze!"
--   putStrLn "Your Goal is to get to the Tri-wizard cup before your opponent taking turns to move"
--   putStrLn
--     "There are some components of the Maze you should be aware of: \n \
--     \ - Coins (indicated by Q): collecting a coin adds 1 to your score \n \
--     \ - Portals (indicated by P): beware: portals can transport you anywhere in the Maze \n \
--     \ - Compasses (indicated by C): collecting a compass adds 1 to your score & takes you a step closer to the goal"
--   playGame initialGame

-- playGame :: Game -> IO ()
-- playGame emptyGame = do
--   putStrLn "please enter your desired difficulty level(easy, medium, or hard)"
--   putStr ">>> "
--   difficulty <- getLine
--   case difficulty of
--     "easy" -> do
--       initMaze <- P.parseFromFile (many mazeFileParser) "data/easy.txt"
--       getMaze initMaze "data/easy_portals.txt" 3 1 -- coins, compases
--     "medium" -> do
--       initMaze <- P.parseFromFile (many mazeFileParser) "data/medium.txt"
--       getMaze initMaze "data/medium_portals.txt" 5 2 -- coins, compasses
--     "hard" -> do
--       initMaze <- P.parseFromFile (many mazeFileParser) "data/hard.txt"
--       getMaze initMaze "data/hard_portals.txt" 7 3 -- coins, compasses
--     "quit" -> return ()
--     _ -> do
--       putStrLn "invalid difficulty level, please try again!"
--       playGame initialGame
--   where
--     getMaze initMaze fName numCoins numCompasses =
--       case initMaze of
--         Just (rawMaze, s) -> do
--           portalsRes <- P.parseFromFile P.portalFileParser fName
--           case portalsRes of
--             Just (portalsList, _) -> do
--               -- putStrLn (show (portalsList)) -- here for debugging purposes
--               let maze = parseMaze rawMaze
--                in let updatedMaze = maze {portals = portalsList}
--                    in let updatedCoins = P.addCoinsToMazeRandom numCoins updatedMaze
--                        in let updatedCompasses = P.addCompassesToMazeRandom numCompasses updatedCoins
--                            in let game = initializeGameState emptyGame updatedCompasses
--                                in do
--                                     print (board game) -- here for debugging purposes
--                                     go game
--             _ -> do
--               putStrLn "error loading maze, please try again later"
--               return ()
--         _ -> do
--           putStrLn "error loading maze, please try again later"
--           return ()
--     go game =
--       do
--         let curPlayer = current game
--          in do
--               putStrLn "------ Player One Info: -------"
--               putStrLn (printPlayerAttributes (player1 (playerInfo game)))
--               putStrLn "------ Player Two Info: -------"
--               putStrLn (printPlayerAttributes (player2 (playerInfo game)))
--               putStrLn "-------------------------------\n"
--               putStr ("Player " ++ getPlayerStr curPlayer ++ "'s Turn >>> ")
--               dir <- getLine
--               case dir of
--                 "l" -> makeMove MLeft curPlayer game
--                 "r" -> makeMove MRight curPlayer game
--                 "u" -> makeMove MUp curPlayer game
--                 "d" -> makeMove MDown curPlayer game
--                 "quit" -> return ()
--                 _ -> do
--                   putStrLn "invalid move, please enter a valid move (l, r, u, d, quit)\n"
--                   go game
--     makeMove action p g = do
--       let pos = extractPosition p g
--        in let c = actionsToCell (board g) pos action
--            in if c `elem` cells (board g)
--                 then
--                   if isWall c
--                     then do
--                       putStrLn "can't move to a Wall! Please try another move\n" -- do something if trying to move to wall
--                       go g
--                     else
--                       if p == current g
--                         then
--                           if c == goal (board g)
--                             then do
--                               putStrLn ("Congrats Player " ++ getPlayerStr p ++ "! You Won the TriWizard Cup!") -- cur player wins!
--                               return ()
--                             else
--                               if c `elem` coins (board g)
--                                 then do
--                                   let g' = S.execState (collectCoin c p) g
--                                    in go g'
--                                 else
--                                   if c `elem` getPortalEntrances (board g)
--                                     then do
--                                       let g' = S.execState (enterPortal c p) g
--                                        in go g'
--                                     else
--                                       if c `elem` compasses (board g)
--                                         then do
--                                           -- putStrLn "compass found!"
--                                           let g' = S.execState (collectCompass c p) g
--                                            in go g'
--                                         else do
--                                           let g' = S.execState (moveOne c p) g
--                                            in go g'
--                         else do
--                           putStrLn "something went wrong, please try again later" -- do something if player isn't current
--                           return ()
--                 else do
--                   putStrLn "you are trying to move outside of the maze, please choose a valid direction" -- error trying to move outside of board?
--                   go g

-- ------------------------------------------------------------------------------------------

-- -- | Part 7: Testing

-- ------------------------------------------------------------------------------------------

-- testGame :: Game
-- testGame =
--   Game
--     { board =
--         Maze
--           { cells =
--               [ Cell {x = 0, y = 0, isWall = False},
--                 Cell {x = 0, y = 1, isWall = False},
--                 Cell {x = 0, y = 2, isWall = True},
--                 Cell {x = 0, y = 3, isWall = False},
--                 Cell {x = 1, y = 0, isWall = True},
--                 Cell {x = 1, y = 1, isWall = False},
--                 Cell {x = 1, y = 2, isWall = False},
--                 Cell {x = 1, y = 3, isWall = False},
--                 Cell {x = 2, y = 0, isWall = False},
--                 Cell {x = 2, y = 1, isWall = False},
--                 Cell {x = 2, y = 2, isWall = False},
--                 Cell {x = 2, y = 3, isWall = False},
--                 Cell {x = 3, y = 0, isWall = True},
--                 Cell {x = 3, y = 1, isWall = False},
--                 Cell {x = 3, y = 2, isWall = False},
--                 Cell {x = 3, y = 3, isWall = False}
--               ],
--             startPlayerOne = Cell {x = 3, y = 1, isWall = False},
--             startPlayerTwo = Cell {x = 0, y = 3, isWall = False},
--             goal = Cell {x = 0, y = 0, isWall = False},
--             coins = [Cell {x = 2, y = 2, isWall = False}],
--             compasses = [Cell {x = 1, y = 1, isWall = False}],
--             portals =
--               [ Portal
--                   { entrance = Cell {x = 2, y = 0, isWall = False},
--                     exit = Cell {x = 0, y = 1, isWall = False}
--                   }
--               ],
--             rows = 4,
--             cols = 4
--           },
--       current = One,
--       playerInfo =
--         PlayerInfo
--           { player1 =
--               Attributes
--                 { numPoints = 0,
--                   position = Cell {x = 3, y = 1, isWall = False}
--                 },
--             player2 =
--               Attributes
--                 { numPoints = 0,
--                   position = Cell {x = 0, y = 3, isWall = False}
--                 }
--           }
--     }

-- test_moveOne :: Test
-- test_moveOne =
--   "moveOne tests" ~:
--     TestList
--       [ player1 (playerInfo (S.execState (moveOne Cell {x = 2, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 2, y = 1, isWall = False}},
--         player2 (playerInfo (S.execState (moveOne Cell {x = 1, y = 3, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}},
--         current (S.execState (moveOne Cell {x = 2, y = 1, isWall = False} One) testGame) ~?= Two,
--         current (S.execState (moveOne Cell {x = 1, y = 3, isWall = False} Two) testGame) ~?= One
--       ]

-- test_enterPortal :: Test
-- test_enterPortal =
--   "enterPortal tests" ~:
--     TestList
--       [ player1 (playerInfo (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
--         player2 (playerInfo (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
--         current (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} One) testGame) ~?= Two,
--         current (S.execState (enterPortal Cell {x = 2, y = 0, isWall = False} Two) testGame) ~?= One
--       ]

-- test_fetchPortal :: Test
-- test_fetchPortal =
--   "fetchPortal tests" ~:
--     TestList
--       [ fetchPortal testGame Cell {x = 2, y = 0, isWall = False} ~?= Cell {x = 0, y = 1, isWall = False},
--         fetchPortal testGame Cell {x = 2, y = 3, isWall = False} ~?= Cell {x = -1, y = -1, isWall = True}
--       ]

-- test_collectCoin :: Test
-- test_collectCoin =
--   "collectCoin tests" ~:
--     TestList
--       [ player1 (playerInfo (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
--         player2 (playerInfo (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
--         current (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} One) testGame) ~?= Two,
--         current (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame) ~?= One,
--         coins (board (S.execState (collectCoin Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= []
--       ]

-- test_removeCoin :: Test
-- test_removeCoin =
--   "removeCoin tests" ~:
--     TestList
--       [ coins (removeCoin (board testGame) Cell {x = 2, y = 2, isWall = False}) ~?= [],
--         coins (removeCoin (board testGame) Cell {x = 2, y = 1, isWall = False}) ~?= [Cell {x = 2, y = 2, isWall = False}]
--       ]

-- test_collectCompass :: Test
-- test_collectCompass =
--   "collectCompass tests" ~:
--     TestList
--       [ player1 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
--         player2 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
--         current (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame) ~?= Two,
--         current (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame) ~?= One,
--         compasses (board (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} Two) testGame)) ~?= []
--       ]

-- test_pointMe :: Test
-- test_pointMe =
--   "pointMe tests" ~:
--     TestList
--       [ pointMe (board testGame) Cell {x = 1, y = 1, isWall = False} One ~?= Cell {x = 0, y = 1, isWall = False},
--         pointMe (board testGame) Cell {x = 2, y = 2, isWall = False} Two ~?= Cell {x = 1, y = 2, isWall = False},
--         pointMe (board testGame) Cell {x = 2, y = 0, isWall = False} One ~?= Cell {x = 2, y = 0, isWall = False}
--       ]

-- test_filterNeighbors :: Test
-- test_filterNeighbors =
--   "filterNeighbors tests" ~:
--     TestList
--       [ filterNeighbors (board testGame) [(0, 1), (2, 1), (1, 0), (1, 2)] ~?= [(0, 1), (2, 1), (1, 2)],
--         filterNeighbors (board testGame) [] ~?= []
--       ]

-- test_extractBest :: Test
-- test_extractBest =
--   "extractBest tests" ~:
--     TestList
--       [ extractBest (board testGame) [(0, 1), (2, 1), (1, 0), (1, 2)] ~?= Cell {x = 0, y = 1, isWall = False},
--         extractBest (board testGame) [] ~?= Cell {x = -1, y = -1, isWall = True},
--         extractBest (board testGame) [(2, 0)] ~?= Cell {x = 2, y = 0, isWall = False}
--       ]

-- test_getManhattanDistance :: Test
-- test_getManhattanDistance =
--   "getManhattanDistance tests" ~:
--     TestList
--       [ getManhattanDistance (1, 1) (0, 0) ~?= 2,
--         getManhattanDistance (2, 2) (2, 2) ~?= 0,
--         getManhattanDistance (2, 0) (0, 1) ~?= 3
--       ]

-- test_getMin :: Test
-- test_getMin =
--   "getMin tests" ~:
--     TestList
--       [ getMin [((0, 0), 100), ((2, 1), 10), ((3, 2), 1), ((1, 3), 4), ((0, 2), 8)] ~?= Cell {x = 3, y = 2, isWall = False},
--         getMin [((2, 1), 10)] ~?= Cell {x = 2, y = 1, isWall = False},
--         getMin [] ~?= Cell {x = -1, y = -1, isWall = True},
--         getMin [((2, 1), 1), ((3, 2), 1)] ~?= Cell {x = 2, y = 1, isWall = False}
--       ]

-- test_removeCompass :: Test
-- test_removeCompass =
--   "removeCompass tests" ~:
--     TestList
--       [ compasses (removeCompass (board testGame) Cell {x = 1, y = 1, isWall = False}) ~?= [],
--         compasses (removeCompass (board testGame) Cell {x = 2, y = 1, isWall = False}) ~?= [Cell {x = 1, y = 1, isWall = False}]
--       ]

-- test_whichMove :: Test
-- test_whichMove =
--   "whichMove tests" ~:
--     TestList
--       [ player1 (playerInfo (S.execState (whichMove Cell {x = 1, y = 1, isWall = False} One) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}},
--         player2 (playerInfo (S.execState (whichMove Cell {x = 2, y = 2, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}},
--         player1 (playerInfo (S.execState (whichMove Cell {x = 2, y = 0, isWall = False} One) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 0, y = 1, isWall = False}},
--         player2 (playerInfo (S.execState (whichMove Cell {x = 1, y = 3, isWall = False} Two) testGame)) ~?= Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}}
--       ]

-- test_getPortalEntrances :: Test
-- test_getPortalEntrances =
--   "getPortalEntrances tests" ~:
--     TestList
--       [ getPortalEntrances (board testGame) ~?= [Cell {x = 2, y = 0, isWall = False}]
--       ]

-- test_updatePosition :: Test
-- test_updatePosition =
--   "updatePosition tests" ~:
--     TestList
--       [ updatePosition One (playerInfo testGame) Cell {x = 2, y = 1, isWall = False} ~?= (playerInfo testGame) {player1 = Attributes {numPoints = 0, position = Cell {x = 2, y = 1, isWall = False}}},
--         updatePosition Two (playerInfo testGame) Cell {x = 1, y = 3, isWall = False} ~?= (playerInfo testGame) {player2 = Attributes {numPoints = 0, position = Cell {x = 1, y = 3, isWall = False}}}
--       ]

-- test_updatePoints :: Test
-- test_updatePoints =
--   "updatePoints tests" ~:
--     TestList
--       [ updatePoints One (playerInfo testGame) Cell {x = 1, y = 1, isWall = False} ~?= (playerInfo testGame) {player1 = Attributes {numPoints = 1, position = Cell {x = 1, y = 1, isWall = False}}},
--         updatePoints Two (playerInfo testGame) Cell {x = 2, y = 2, isWall = False} ~?= (playerInfo testGame) {player2 = Attributes {numPoints = 1, position = Cell {x = 2, y = 2, isWall = False}}}
--       ]

-- test_getCellWallVal :: Test
-- test_getCellWallVal =
--   "getCellWallVal tests" ~:
--     TestList
--       [ getCellWallVal (1, 1) (board testGame) ~?= False,
--         getCellWallVal (3, 0) (board testGame) ~?= True
--       ]

-- test_actionsToCell :: Test
-- test_actionsToCell =
--   "actionsToCell tests" ~:
--     TestList
--       [ actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MUp ~?= Cell {x = 1, y = 2, isWall = False},
--         actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MDown ~?= Cell {x = 3, y = 2, isWall = False},
--         actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MLeft ~?= Cell {x = 2, y = 1, isWall = False},
--         actionsToCell (board testGame) Cell {x = 2, y = 2, isWall = False} MRight ~?= Cell {x = 2, y = 3, isWall = False}
--       ]

-- test_extractPosition :: Test
-- test_extractPosition =
--   "extractPosition tests" ~:
--     TestList
--       [ extractPosition One testGame ~?= Cell {x = 3, y = 1, isWall = False},
--         extractPosition Two testGame ~?= Cell {x = 0, y = 3, isWall = False}
--       ]

-- test_initializeGameState :: Test
-- test_initializeGameState =
--   "initializeGameState tests" ~:
--     TestList
--       [ initializeGameState initialGame (board testGame) ~?= testGame
--       ]

-- test_allMethods :: IO Counts
-- test_allMethods =
--   runTestTT $
--     TestList
--       [ test_moveOne,
--         test_enterPortal,
--         test_fetchPortal,
--         test_collectCoin,
--         test_removeCoin,
--         test_collectCompass,
--         test_pointMe,
--         test_filterNeighbors,
--         test_extractBest,
--         test_getManhattanDistance,
--         test_getMin,
--         test_removeCompass,
--         test_whichMove,
--         test_getPortalEntrances,
--         test_updatePosition,
--         test_updatePoints,
--         test_getCellWallVal,
--         test_actionsToCell,
--         test_extractPosition,
--         test_initializeGameState
--       ]

-- -- >>> test_allMethods
-- -- Counts {cases = 57, tried = 57, errors = 0, failures = 0}

-- easyMaze :: Maze
-- easyMaze =
--   Maze
--     { cells =
--         [ Cell {x = 0, y = 0, isWall = False},
--           Cell {x = 0, y = 1, isWall = True},
--           Cell {x = 0, y = 2, isWall = False},
--           Cell {x = 0, y = 3, isWall = True},
--           Cell {x = 0, y = 4, isWall = False},
--           Cell {x = 0, y = 5, isWall = True},
--           Cell {x = 0, y = 6, isWall = True},
--           Cell {x = 0, y = 7, isWall = False},
--           Cell {x = 0, y = 8, isWall = False},
--           Cell {x = 0, y = 9, isWall = False},
--           Cell {x = 0, y = 10, isWall = True},
--           Cell {x = 1, y = 0, isWall = False},
--           Cell {x = 1, y = 1, isWall = False},
--           Cell {x = 1, y = 2, isWall = False},
--           Cell {x = 1, y = 3, isWall = True},
--           Cell {x = 1, y = 4, isWall = False},
--           Cell {x = 1, y = 5, isWall = True},
--           Cell {x = 1, y = 6, isWall = True},
--           Cell {x = 1, y = 7, isWall = True},
--           Cell {x = 1, y = 8, isWall = True},
--           Cell {x = 1, y = 9, isWall = False},
--           Cell {x = 1, y = 10, isWall = True},
--           Cell {x = 2, y = 0, isWall = False},
--           Cell {x = 2, y = 1, isWall = False},
--           Cell {x = 2, y = 2, isWall = False},
--           Cell {x = 2, y = 3, isWall = False},
--           Cell {x = 2, y = 4, isWall = False},
--           Cell {x = 2, y = 5, isWall = False},
--           Cell {x = 2, y = 6, isWall = False},
--           Cell {x = 2, y = 7, isWall = False},
--           Cell {x = 2, y = 8, isWall = False},
--           Cell {x = 2, y = 9, isWall = True},
--           Cell {x = 2, y = 10, isWall = True},
--           Cell {x = 3, y = 0, isWall = True},
--           Cell {x = 3, y = 1, isWall = False},
--           Cell {x = 3, y = 2, isWall = False},
--           Cell {x = 3, y = 3, isWall = True},
--           Cell {x = 3, y = 4, isWall = False},
--           Cell {x = 3, y = 5, isWall = False},
--           Cell {x = 3, y = 6, isWall = False},
--           Cell {x = 3, y = 7, isWall = False},
--           Cell {x = 3, y = 8, isWall = False},
--           Cell {x = 3, y = 9, isWall = True},
--           Cell {x = 3, y = 10, isWall = True},
--           Cell {x = 4, y = 0, isWall = True},
--           Cell {x = 4, y = 1, isWall = True},
--           Cell {x = 4, y = 2, isWall = False},
--           Cell {x = 4, y = 3, isWall = True},
--           Cell {x = 4, y = 4, isWall = True},
--           Cell {x = 4, y = 5, isWall = True},
--           Cell {x = 4, y = 6, isWall = False},
--           Cell {x = 4, y = 7, isWall = True},
--           Cell {x = 4, y = 8, isWall = True},
--           Cell {x = 4, y = 9, isWall = True},
--           Cell {x = 4, y = 10, isWall = True},
--           Cell {x = 5, y = 0, isWall = True},
--           Cell {x = 5, y = 1, isWall = False},
--           Cell {x = 5, y = 2, isWall = True},
--           Cell {x = 5, y = 3, isWall = True},
--           Cell {x = 5, y = 4, isWall = True},
--           Cell {x = 5, y = 5, isWall = True},
--           Cell {x = 5, y = 6, isWall = False},
--           Cell {x = 5, y = 7, isWall = False},
--           Cell {x = 5, y = 8, isWall = False},
--           Cell {x = 5, y = 9, isWall = True},
--           Cell {x = 5, y = 10, isWall = True},
--           Cell {x = 6, y = 0, isWall = False},
--           Cell {x = 6, y = 1, isWall = False},
--           Cell {x = 6, y = 2, isWall = False},
--           Cell {x = 6, y = 3, isWall = True},
--           Cell {x = 6, y = 4, isWall = True},
--           Cell {x = 6, y = 5, isWall = False},
--           Cell {x = 6, y = 6, isWall = False},
--           Cell {x = 6, y = 7, isWall = False},
--           Cell {x = 6, y = 8, isWall = True},
--           Cell {x = 6, y = 9, isWall = True},
--           Cell {x = 6, y = 10, isWall = True},
--           Cell {x = 7, y = 0, isWall = True},
--           Cell {x = 7, y = 1, isWall = True},
--           Cell {x = 7, y = 2, isWall = False},
--           Cell {x = 7, y = 3, isWall = False},
--           Cell {x = 7, y = 4, isWall = False},
--           Cell {x = 7, y = 5, isWall = False},
--           Cell {x = 7, y = 6, isWall = True},
--           Cell {x = 7, y = 7, isWall = False},
--           Cell {x = 7, y = 8, isWall = True},
--           Cell {x = 7, y = 9, isWall = True},
--           Cell {x = 7, y = 10, isWall = True},
--           Cell {x = 8, y = 0, isWall = True},
--           Cell {x = 8, y = 1, isWall = True},
--           Cell {x = 8, y = 2, isWall = True},
--           Cell {x = 8, y = 3, isWall = False},
--           Cell {x = 8, y = 4, isWall = True},
--           Cell {x = 8, y = 5, isWall = False},
--           Cell {x = 8, y = 6, isWall = True},
--           Cell {x = 8, y = 7, isWall = False},
--           Cell {x = 8, y = 8, isWall = True},
--           Cell {x = 8, y = 9, isWall = True},
--           Cell {x = 8, y = 10, isWall = False}
--         ],
--       startPlayerOne = Cell {x = 0, y = 4, isWall = False},
--       startPlayerTwo = Cell {x = 8, y = 5, isWall = False},
--       goal = Cell {x = 2, y = 8, isWall = False},
--       coins = [Cell {x = 6, y = 5, isWall = False}, Cell {x = 8, y = 3, isWall = False}, Cell {x = 5, y = 8, isWall = False}],
--       compasses = [Cell {x = 6, y = 5, isWall = False}],
--       portals = [Portal {entrance = Cell {x = 6, y = 0, isWall = False}, exit = Cell {x = 4, y = 7, isWall = False}}, Portal {entrance = Cell {x = 2, y = 5, isWall = False}, exit = Cell {x = 1, y = 0, isWall = False}}],
--       rows = 9,
--       cols = 11
--     }

-- easyGame :: Game
-- easyGame = initializeGameState initialGame easyMaze

-- -- >>> player1 (playerInfo (S.execState (collectCompass Cell {x = 6, y = 5, isWall = False} One) easyGame))
-- -- Attributes {numPoints = 1, position = Cell {x = 6, y = 6, isWall = False}}

-- -- >>> player1 (playerInfo (S.execState (collectCompass Cell {x = 1, y = 1, isWall = False} One) testGame))
-- -- Attributes {numPoints = 1, position = Cell {x = 0, y = 1, isWall = False}}

-- --- >>> getManhattanDistance (2, 8) (6, 6)
-- -- 6

-- -- >>> pointMe easyMaze Cell {x=6,y=5,isWall=False} One
-- -- Cell {x = 6, y = 6, isWall = False}
