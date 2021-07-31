{-# LANGUAGE TemplateHaskell #-}
module CPU where
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Test.QuickCheck.All (quickCheckAll)
import Data.Word (Word8, Word16)

newtype Register = Register Word16 deriving (Eq, Show)
newtype SubRegister = SubRegister Word8 deriving (Eq, Show)

prop_splitRegister0 b1 b2 = splitRegister (Register (shiftL (fromIntegral b1) 8 .|. fromIntegral b2)) == (SubRegister b1, SubRegister b2)
splitRegister :: Register -> (SubRegister, SubRegister)
splitRegister (Register word) = (SubRegister $ fromIntegral (shiftR (word .&. 0xff00) 8), SubRegister $ fromIntegral (word .&. 0xff))

return []
runTests = $quickCheckAll
