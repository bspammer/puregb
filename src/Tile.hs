{-# LANGUAGE TemplateHaskell #-}
module Tile where

import Data.Bits ((.&.), shift)
import Data.Word (Word8)
import Data.ByteString as B (ByteString, replicate, index, pack)
import Test.QuickCheck.All (quickCheckAll)

data Pixel = P0 | P1 | P2 | P3 deriving (Eq, Ord)
newtype Tile = Tile ByteString

emptyTile = Tile (pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
-- Examples from https://www.huderlem.com/demos/gameboy2bpp.html
exampleTileLetterA = Tile (pack [0x7C, 0x7C, 0x00, 0xC6, 0xC6, 0x00, 0x00, 0xFE, 0xC6, 0xC6, 0x00, 0xC6, 0xC6, 0x00, 0x00, 0x00])
exampleTileWindow = Tile (pack [0xFF, 0x00, 0x7E, 0xFF, 0x85, 0x81, 0x89, 0x83, 0x93, 0x85, 0xA5, 0x8B, 0xC9, 0x97, 0x7E, 0xFF])

instance Show Pixel where
    show P0 = " "
    show P1 = "."
    show P2 = "o"
    show P3 = "O"

prop_showEmpty = show emptyTile == "        \n        \n        \n        \n        \n        \n        \n        \n"
prop_showLetterA = show exampleTileLetterA == " OOOOO  \noo   oo \n..   .. \nooooooo \nOO   OO \noo   oo \n..   .. \n        \n"
prop_showWindow = show exampleTileWindow == "........\noOOOOOOo\nO    . O\nO   . oO\nO  . o.O\nO . o.oO\nO. o.ooO\noOOOOOOo\n"
instance Show Tile where
    show tile = unlines [concat [show (getTilePixelXY tile x y) | x <- [0..7]] | y <- [0..7]]

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

prop_getTilePixel0 = P0 == getTilePixel exampleTileLetterA 0
prop_getTilePixel1 = P1 == getTilePixel exampleTileLetterA 16
prop_getTilePixel2 = P2 == getTilePixel exampleTileLetterA 8
prop_getTilePixel3 = P3 == getTilePixel exampleTileLetterA 1
getTilePixel :: Tile -> Int -> Pixel
getTilePixel (Tile tile) i = pixelFromBytePair (left, right) mod8
    where 
        mod8 = i `mod` 8 
        pairIndex = (mod8 + (i - mod8) * 2) `div` 8
        left = B.index tile pairIndex
        right =  B.index tile (pairIndex + 1)

prop_getTilePixelXY0 = P0 == getTilePixelXY exampleTileLetterA 0 0
prop_getTilePixelXY1 = P1 == getTilePixelXY exampleTileLetterA 0 2
prop_getTilePixelXY2 = P2 == getTilePixelXY exampleTileLetterA 0 1
prop_getTilePixelXY3 = P3 == getTilePixelXY exampleTileLetterA 1 0
getTilePixelXY :: Tile -> Int -> Int -> Pixel
getTilePixelXY t x y = getTilePixel t (y * 8 + x)

return []
runTests = $quickCheckAll