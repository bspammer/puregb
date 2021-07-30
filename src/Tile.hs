{-# LANGUAGE TemplateHaskell #-}
module Tile where

import Data.Bits ((.&.), shift)
import Data.Word (Word8)
import Data.ByteString as B (ByteString, replicate, index, pack, splitAt, head)
import Test.QuickCheck.All (quickCheckAll)

tileSize = 16
-- https://www.huderlem.com/demos/gameboy2bpp.html
exampleTile = Tile (pack [0x7C, 0x7C, 0x00, 0xC6, 0xC6, 0x00, 0x00, 0xFE, 0xC6, 0xC6, 0x00, 0xC6, 0xC6, 0x00])

newtype Tile = Tile ByteString
data Pixel = P0 | P1 | P2 | P3 deriving (Eq, Ord)

prop_emptyshow = show emptyTile == concat (Prelude.replicate 16 "                \n")
instance Show Tile where
    show (Tile t) = concat (Prelude.replicate 16 "                \n")

emptyTile :: Tile
emptyTile = Tile (B.replicate tileSize 0x00)

prop_pixelFromBytePair0 = pixelFromBytePair (0x00, 0x00) 0 == P0
prop_pixelFromBytePair1 = pixelFromBytePair (0x80, 0x00) 0 == P1
prop_pixelFromBytePair2 = pixelFromBytePair (0x00, 0x80) 0 == P2
prop_pixelFromBytePair3 = pixelFromBytePair (0x80, 0x80) 0 == P3
prop_pixelFromBytePair4 = pixelFromBytePair (0xc6, 0x00) 0 == P1
pixelFromBytePair :: (Word8, Word8) -> Int -> Pixel
pixelFromBytePair (left, right) i
    | not leftTrue && not rightTrue = P0
    | leftTrue && not rightTrue = P1
    | not leftTrue && rightTrue = P2
    | otherwise = P3
    where 
        leftTrue = left .&. shift 0x80 (-i) > 0
        rightTrue = right .&. shift 0x80 (-i) > 0

prop_getTilePixel0 = P0 == getTilePixel exampleTile 0
prop_getTilePixel1 = P1 == getTilePixel exampleTile 16
prop_getTilePixel2 = P2 == getTilePixel exampleTile 8
prop_getTilePixel3 = P3 == getTilePixel exampleTile 1
getTilePixel :: Tile -> Int -> Pixel
getTilePixel (Tile tile) i = pixelFromBytePair (B.head left, B.head right) (i `mod` 8)
    where 
        pairIndex = ((i `mod` 8) + (i - (i `mod` 8)) * 2) `div` 8
        (_, b) = B.splitAt pairIndex tile
        (bytePair, _) = B.splitAt (pairIndex + 2) b
        (left, right) = B.splitAt 1 bytePair

-- getTilePixelXY :: Tile -> Int -> Int -> Pixel
-- getTilePixelXY (Tile t) x y = index t 0

return []
runTests = $quickCheckAll