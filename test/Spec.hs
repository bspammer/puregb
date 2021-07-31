import Control.Monad (sequence)
import Data.ByteString as B (readFile)
import System.Exit (exitFailure, exitSuccess)

import Rom (runTests)
import Tile (runTests)

main :: IO ()
main = do
    putStrLn "Running tests"
    results <- sequence [
      Tile.runTests,
      Rom.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
