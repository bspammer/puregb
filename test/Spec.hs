import Control.Monad (sequence)
import Data.Bits
import Data.ByteString as B (readFile)
import System.Exit (exitFailure, exitSuccess)

import CPU
import CPU.Instruction (runTests)
import Rom (runTests)
import Tile (runTests)

main :: IO ()
main = do
    putStrLn "Running tests"
    results <- sequence [
      Tile.runTests,
      Rom.runTests,
      CPU.runTests,
      CPU.Instruction.runTests
      ]
    if and results
      then exitSuccess
      else exitFailure
