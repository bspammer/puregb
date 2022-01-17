{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module CPU where
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word16, Word8)
import Data.Array (Array, array)
import Lens.Micro
import Lens.Micro.Platform ( makeLenses )
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.All (quickCheckAll)

type Register = Word16
type SubRegister = Word8

data RegisterHalf = Front | Back deriving (Eq, Show)

data CPU = CPU {
    _accumulator :: !SubRegister,
    _flags :: !SubRegister,
    _bc :: !Register,
    _de :: !Register,
    _hl :: !Register,
    _sp :: !Register,
    _pc :: !Register,
    _ram :: !(Array Word16 Word8)
} deriving (Eq, Show)

makeLenses ''CPU
b :: Lens' CPU SubRegister
b = bc . registerHalf Front
c :: Lens' CPU SubRegister
c = bc . registerHalf Back
d :: Lens' CPU SubRegister
d = de . registerHalf Front
e :: Lens' CPU SubRegister
e = de . registerHalf Back
h :: Lens' CPU SubRegister
h = hl . registerHalf Front
l :: Lens' CPU SubRegister
l = hl . registerHalf Back

data Flag = Zero | Subtraction | HalfCarry | Carry

memorySize = 8 * 1024
exampleZeroCPU = CPU {
    _accumulator = 0x00,
    _flags = 0x00,
    _bc = 0x0000,
    _de = 0x0000,
    _hl = 0x0000,
    _sp = 0x0000,
    _pc = 0x0000,
    _ram = array (0, memorySize) [(i, 0) | i <- [0..memorySize]]
}

prop_splitRegister0 b1 b2 = splitRegister (shiftL (fromIntegral b1) 8 .|. fromIntegral b2) == (b1, b2)
prop_splitRegister1 = splitRegister 0xffff == (0xff, 0xff)
prop_splitRegister2 = splitRegister 0xabcd == (0xab, 0xcd)
splitRegister :: Register -> (SubRegister, SubRegister)
splitRegister word = (fromIntegral (shiftR (word .&. 0xff00) 8), fromIntegral (word .&. 0xff))

prop_joinRegister0 :: Word8 -> Word8 -> Bool
prop_joinRegister0 b1 b2 = (shiftL (fromIntegral b1) 8 .|. fromIntegral b2) == joinRegister (b1, b2)
prop_joinRegister1 = 0xffff == joinRegister (0xff, 0xff)
prop_joinRegister2 = 0xabcd == joinRegister (0xab, 0xcd)
joinRegister :: (SubRegister, SubRegister) -> Register
joinRegister (word1, word2) = fromIntegral (shiftL (fromIntegral word1 :: Word16) 8 .|. (fromIntegral word2 :: Word16))

bitForFlag :: Flag -> Int
bitForFlag Zero = 7
bitForFlag Subtraction = 6
bitForFlag HalfCarry = 5
bitForFlag Carry = 4

prop_getFlagZero byte = getFlag Zero (flags .~ byte $ exampleZeroCPU) == (byte .&. 0x80 > 0)
prop_getFlagSubtraction byte = getFlag Subtraction (flags .~ byte $ exampleZeroCPU) == (byte .&. 0x40 > 0)
prop_getFlagHalfCarry byte = getFlag HalfCarry (flags .~ byte $ exampleZeroCPU) == (byte .&. 0x20 > 0)
prop_getFlagCarry byte = getFlag Carry (flags .~ byte $ exampleZeroCPU) == (byte .&. 0x10 > 0)
getFlag :: Flag -> CPU -> Bool
getFlag flag = (^. flagLens flag)

prop_flagLensGet0 = not (exampleZeroCPU ^. flagLens Zero)
prop_flagLensGet1 = not (exampleZeroCPU ^. flagLens Subtraction)
prop_flagLensGet2 = not (exampleZeroCPU ^. flagLens HalfCarry)
prop_flagLensGet3 = not (exampleZeroCPU ^. flagLens Carry)
prop_flagLensSet0 = (exampleZeroCPU & flagLens Zero .~ True) == exampleZeroCPU {_flags=0x80}
prop_flagLensSet1 = (exampleZeroCPU & flagLens Subtraction .~ True) == exampleZeroCPU {_flags=0x40}
prop_flagLensSet2 = (exampleZeroCPU & flagLens HalfCarry .~ True) == exampleZeroCPU {_flags=0x20}
prop_flagLensSet3 = (exampleZeroCPU & flagLens Carry .~ True) == exampleZeroCPU {_flags=0x10}
prop_flagLensSet4 = (exampleZeroCPU & flagLens Carry .~ True & flagLens Carry .~ False) == exampleZeroCPU {_flags=0x00}
prop_flagLensSet5 = (exampleZeroCPU & flagLens Zero .~ True
                                    & flagLens Subtraction .~ True
                                    & flagLens HalfCarry .~ True
                                    & flagLens Carry .~ True
                    ) == exampleZeroCPU {_flags=0xf0}
flagLens :: Flag -> Lens' CPU Bool
flagLens flag = lens getter setter
    where
        flagIndex = shiftL 1 (bitForFlag flag)
        getter CPU {_flags=w} = w .&. flagIndex > 0
        setter cpu@CPU {_flags=w} b = cpu & flags .~ (if b then w .|. flagIndex else w .&. (flagIndex `xor` 0xff))

zeroFlag :: Lens' CPU Bool
zeroFlag = flagLens Zero

subtractionFlag :: Lens' CPU Bool
subtractionFlag = flagLens Subtraction

halfCarryFlag :: Lens' CPU Bool
halfCarryFlag = flagLens HalfCarry

carryFlag :: Lens' CPU Bool
carryFlag = flagLens Carry

prop_registerHalfLensGet0 = 0x1234 ^. registerHalf Front == 0x12
prop_registerHalfLensGet1 = 0x1234 ^. registerHalf Back == 0x34
prop_registerHalfLensSet0 = (0x1234 & (registerHalf Front .~ 0x56)) == 0x5634
prop_registerHalfLensSet1 = (0x1234 & (registerHalf Back .~ 0x56)) == 0x1256
registerHalf :: RegisterHalf -> Lens' Register SubRegister 
registerHalf rHalf = lens getter setter
    where
        getter = (^. (if rHalf == Front then _1 else _2)) . splitRegister
        setter r s = joinRegister $ splitRegister r & (if rHalf == Front then _1 else _2) .~ s


return []
runTests = $quickCheckAll
