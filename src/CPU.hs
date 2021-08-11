{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module CPU where

import Lens.Micro
import Lens.Micro.Platform
import Data.Bits (shiftR, shiftL, (.&.), (.|.), xor)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.All (quickCheckAll)
import Data.Word (Word8, Word16)

newtype Register = Register Word16 deriving (Eq, Show)
newtype SubRegister = SubRegister Word8 deriving (Eq, Show)

data RegisterHalf = Front | Back deriving (Eq, Show)

data CPU = CPU {
    _accumulator :: !SubRegister,
    _flags :: !SubRegister,
    _bc :: !Register,
    _de :: !Register,
    _hl :: !Register
} deriving (Eq, Show)

makeLenses ''CPU

data Flag = Zero | Subtraction | HalfCarry | Carry

exampleZeroCPU = CPU {
    _accumulator = SubRegister 0x00,
    _flags = SubRegister 0x00,
    _bc = Register 0x0000,
    _de = Register 0x0000,
    _hl = Register 0x0000
}

prop_splitRegister0 b1 b2 = splitRegister (Register (shiftL (fromIntegral b1) 8 .|. fromIntegral b2)) == (SubRegister b1, SubRegister b2)
prop_splitRegister1 = splitRegister (Register 0xffff) == (SubRegister 0xff, SubRegister 0xff)
prop_splitRegister2 = splitRegister (Register 0xabcd) == (SubRegister 0xab, SubRegister 0xcd)
splitRegister :: Register -> (SubRegister, SubRegister)
splitRegister (Register word) = (SubRegister $ fromIntegral (shiftR (word .&. 0xff00) 8), SubRegister $ fromIntegral (word .&. 0xff))

prop_joinRegister0 b1 b2 = Register (shiftL (fromIntegral b1) 8 .|. fromIntegral b2) == joinRegister (SubRegister b1, SubRegister b2)
prop_joinRegister1 = Register 0xffff == joinRegister (SubRegister 0xff, SubRegister 0xff)
prop_joinRegister2 = Register 0xabcd == joinRegister (SubRegister 0xab, SubRegister 0xcd)
joinRegister :: (SubRegister, SubRegister) -> Register
joinRegister (SubRegister word1, SubRegister word2) = Register $ fromIntegral (shiftL (fromIntegral word1 :: Word16) 8 .|. (fromIntegral word2 :: Word16))

bitForFlag :: Flag -> Int
bitForFlag Zero = 7
bitForFlag Subtraction = 6
bitForFlag HalfCarry = 5
bitForFlag Carry = 4

prop_getFlagZero byte = getFlag Zero (flags .~ SubRegister byte $ exampleZeroCPU) == (byte .&. 0x80 > 0)
prop_getFlagSubtraction byte = getFlag Subtraction (flags .~ SubRegister byte $ exampleZeroCPU) == (byte .&. 0x40 > 0)
prop_getFlagHalfCarry byte = getFlag HalfCarry (flags .~ SubRegister byte $ exampleZeroCPU) == (byte .&. 0x20 > 0)
prop_getFlagCarry byte = getFlag Carry (flags .~ SubRegister byte $ exampleZeroCPU) == (byte .&. 0x10 > 0)
getFlag :: Flag -> CPU -> Bool
getFlag flag = (^. flagLens flag)

prop_flagLensGet0 = not (exampleZeroCPU ^. flagLens Zero)
prop_flagLensGet1 = not (exampleZeroCPU ^. flagLens Subtraction)
prop_flagLensGet2 = not (exampleZeroCPU ^. flagLens HalfCarry)
prop_flagLensGet3 = not (exampleZeroCPU ^. flagLens Carry)
prop_flagLensSet0 = (exampleZeroCPU & flagLens Zero .~ True) == exampleZeroCPU {_flags=SubRegister 0x80}
prop_flagLensSet1 = (exampleZeroCPU & flagLens Subtraction .~ True) == exampleZeroCPU {_flags=SubRegister 0x40}
prop_flagLensSet2 = (exampleZeroCPU & flagLens HalfCarry .~ True) == exampleZeroCPU {_flags=SubRegister 0x20}
prop_flagLensSet3 = (exampleZeroCPU & flagLens Carry .~ True) == exampleZeroCPU {_flags=SubRegister 0x10}
prop_flagLensSet4 = (exampleZeroCPU & flagLens Carry .~ True & flagLens Carry .~ False) == exampleZeroCPU {_flags=SubRegister 0x00}
prop_flagLensSet5 = (exampleZeroCPU & flagLens Zero .~ True
                                    & flagLens Subtraction .~ True
                                    & flagLens HalfCarry .~ True
                                    & flagLens Carry .~ True
                    ) == exampleZeroCPU {_flags=SubRegister 0xf0}
flagLens :: Flag -> Lens' CPU Bool
flagLens flag = lens getter setter
    where
        flagIndex = shiftL 1 (bitForFlag flag)
        getter CPU {_flags=(SubRegister w)} = w .&. flagIndex > 0
        setter cpu@CPU {_flags=(SubRegister w)} b = cpu & flags .~ SubRegister (if b then w .|. flagIndex else w .&. (flagIndex `xor` 0xff))

zeroFlagLens :: Lens' CPU Bool
zeroFlagLens = flagLens Zero

subtractionFlagLens :: Lens' CPU Bool
subtractionFlagLens = flagLens Subtraction

halfCarryFlagLens :: Lens' CPU Bool
halfCarryFlagLens = flagLens HalfCarry

carryFlagLens :: Lens' CPU Bool
carryFlagLens = flagLens Carry

prop_registerHalfLensGet0 = Register 0x1234 ^. registerHalfLens Front == SubRegister 0x12
prop_registerHalfLensGet1 = Register 0x1234 ^. registerHalfLens Back == SubRegister 0x34
prop_registerHalfLensSet0 = (Register 0x1234 & (registerHalfLens Front .~ SubRegister 0x56)) == Register 0x5634
prop_registerHalfLensSet1 = (Register 0x1234 & (registerHalfLens Back .~ SubRegister 0x56)) == Register 0x1256
registerHalfLens :: RegisterHalf -> Lens' Register SubRegister 
registerHalfLens rHalf = lens getter setter
    where
        getter = (^. (if rHalf == Front then _1 else _2)) . splitRegister
        setter r s = joinRegister $ splitRegister r & (if rHalf == Front then _1 else _2) .~ s

return []
runTests = $quickCheckAll
