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

prop_splitRegister0 b1 b2 = splitRegister (Register (shiftL (fromIntegral b1) 8 .|. fromIntegral b2)) == (SubRegister b1, SubRegister b2)
prop_splitRegister1 = splitRegister (Register 0xffff) == (SubRegister 0xff, SubRegister 0xff)
prop_splitRegister2 = splitRegister (Register 0xabcd) == (SubRegister 0xab, SubRegister 0xcd)
splitRegister :: Register -> (SubRegister, SubRegister)
splitRegister (Register word) = (SubRegister $ fromIntegral (shiftR (word .&. 0xff00) 8), SubRegister $ fromIntegral (word .&. 0xff))

return []
runTests = $quickCheckAll
