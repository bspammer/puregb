import Tile
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (sequence)


main :: IO ()
main = do
    putStrLn "Running tests"
    results <- sequence [Tile.runTests]
    if and results
      then exitSuccess
      else exitFailure