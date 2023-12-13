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
