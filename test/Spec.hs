import GameStateTest
import Lib
import MazeTest
import Test.HUnit
import Test.QuickCheck qualified as QC

main :: IO ()
main = do
  putStrLn "--------- MAZE HUNIT TESTS ... ---------"
  MazeTest.test_all
  putStrLn "\n"
  putStrLn "--------- MAZE QUICKCHECK TESTS ... ---------"
  MazeTest.qc
  putStrLn "Done!"
  putStrLn "--------- GAME STATE HUNIT TESTS ... ---------"
  GameStateTest.test_allMethods
  putStrLn "\n"
  putStrLn "--------- GAME STATE QUICKCHECK TESTS ... ---------"
  GameStateTest.qcGameState
  putStrLn "Done!"
