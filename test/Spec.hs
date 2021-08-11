import Control.Monad (sequence)
import Data.ByteString as B (readFile)
import System.Exit (exitFailure, exitSuccess)

import Data.Bits
import CPU 
import Rom (runTests)
import Tile (runTests)

main :: IO ()
main = do
    putStrLn "Running tests"
    print $ joinRegister (SubRegister 0xff, SubRegister 0xff)
    results <- sequence [
      Tile.runTests,
      Rom.runTests,
      CPU.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
