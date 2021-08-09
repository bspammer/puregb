{-# LANGUAGE TemplateHaskell #-}
module CPU where
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Test.QuickCheck.All (quickCheckAll)
import Data.Word (Word8, Word16)

newtype Register = Register Word16 deriving (Eq, Show)
newtype SubRegister = SubRegister Word8 deriving (Eq, Show)

data CPU = CPU {
    accumulator :: SubRegister,
    flags :: SubRegister,
    bc :: Register,
    de :: Register,
    hl :: Register
}

data Flag = Zero | Subtraction | HalfCarry | Carry

prop_splitRegister0 b1 b2 = splitRegister (Register (shiftL (fromIntegral b1) 8 .|. fromIntegral b2)) == (SubRegister b1, SubRegister b2)
prop_splitRegister1 = splitRegister (Register 0xffff) == (SubRegister 0xff, SubRegister 0xff)
prop_splitRegister2 = splitRegister (Register 0xabcd) == (SubRegister 0xab, SubRegister 0xcd)
splitRegister :: Register -> (SubRegister, SubRegister)
splitRegister (Register word) = (SubRegister $ fromIntegral (shiftR (word .&. 0xff00) 8), SubRegister $ fromIntegral (word .&. 0xff))

bitForFlag :: Flag -> Int
bitForFlag Zero = 7
bitForFlag Subtraction = 6
bitForFlag HalfCarry = 5
bitForFlag Carry = 4

prop_getFlagZero byte = getFlag Zero CPU {flags = SubRegister byte} == (byte .&. 0x80 > 0)
prop_getFlagSubtraction byte = getFlag Subtraction CPU {flags = SubRegister byte} == (byte .&. 0x40 > 0)
prop_getFlagHalfCarry byte = getFlag HalfCarry CPU {flags = SubRegister byte} == (byte .&. 0x20 > 0)
prop_getFlagCarry byte = getFlag Carry CPU {flags = SubRegister byte} == (byte .&. 0x10 > 0)
getFlag :: Flag -> CPU -> Bool
getFlag flag CPU {flags = SubRegister f} = extractBit (bitForFlag flag) > 0
    where
        extractBit bit = f .&. shiftL 1 bit

return []
runTests = $quickCheckAll
