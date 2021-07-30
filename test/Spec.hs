import Paths_puregb
import Tile
import Rom.Rom
import Data.ByteString as B (readFile)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (sequence)

main :: IO ()
main = do
    putStrLn "Running tests"
    results <- sequence [
      -- Tile.runTests,
      Rom.Rom.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure