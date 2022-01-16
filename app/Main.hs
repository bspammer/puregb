module Main where

import CPU (CPU)
import qualified Data.ByteString (ByteString, readFile)

main :: IO ()
main = do
    file <- Data.ByteString.readFile "data/boot.gb"
    putStrLn "hi"
