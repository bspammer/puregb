{-# LANGUAGE TemplateHaskell #-}
module Rom where
import Control.Exception (assert, displayException, throw, Exception)
import Data.Bits (shift, (.|.))
import Data.ByteString as B
       (break, index, readFile, splitAt, unpack, ByteString)
import Data.ByteString.UTF8 as UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.Map (lookup)
import Data.Word (Word16, Word8)

import Path.Posix (fromAbsFile, mkAbsFile, Abs, File, Path)
import Test.QuickCheck.All (quickCheckAll)

import Rom.Enum
       (cartridgeTypeMap, licenseeMap, ramSizeMap, romSizeMap, LicenseeCode(..),
        CartridgeType(..), RomSize(..), RamSize(..), DestinationCode(..))

data RomException = InvalidHeaderChecksum Word8 Word8 deriving Show
instance Exception RomException where
    displayException (InvalidHeaderChecksum romChecksum computedChecksum) = "ROM header checksum '" ++ show romChecksum ++ "' does not match computed header checksum '" ++ show computedChecksum ++ "'."

data Rom = Rom {
    title :: String,
    licenseeCode :: LicenseeCode,
    sGBFlag :: Bool,
    cartridgeType :: CartridgeType,
    romSize :: RomSize,
    ramSize :: RamSize,
    destinationCode :: DestinationCode,
    versionNumber :: Word8,
    headerChecksum :: Word8,
    globalChecksum :: Word16
}

titleStartByte = 0x134
titleEndByte = 0x144
licenseeByte = 0x144
sGBFlagByte = 0x146
cartridgeTypeByte = 0x147
romSizeByte = 0x148
ramSizeByte = 0x149
destinationCodeByte = 0x14a
versionNumberByte = 0x14c
headerChecksumByte = 0x14d
globalChecksumStartByte = 0x14e
globalChecksumEndByte = 0x14f

exampleRom = $(embedFile "data/tobutobugirl/tobu.gb")

readRom :: FilePath -> IO Rom
readRom filepath = do
    rawBytes <- B.readFile filepath
    let headerChecksum = verifyHeaderChecksum rawBytes
    return Rom {
        title = extractTitle rawBytes,
        licenseeCode = extractLicenseeCode rawBytes,
        sGBFlag = extractSGBFlag rawBytes,
        cartridgeType = extractCartridgeType rawBytes,
        romSize = extractRomSize rawBytes,
        ramSize = extractRamSize rawBytes,
        destinationCode = extractDestinationCode rawBytes,
        versionNumber = extractVersionNumber rawBytes,
        headerChecksum = headerChecksum,
        globalChecksum = extractGlobalChecksum rawBytes
    }

-- Returns bytes with indices low <= i < high
rangeFromByteString :: ByteString -> Int -> Int -> ByteString
rangeFromByteString byteString low high = result
    where
        (_, right) = B.splitAt low byteString
        (result, _) = B.splitAt (high - low) right

prop_packBytes0 = packBytes 0xff 0xff == 0xffff
prop_packBytes1 = packBytes 0xaa 0xbb == 0xaabb
prop_packBytes2 = packBytes 0x12 0x34 == 0x1234
prop_packBytes3 = packBytes 0x00 0x00 == 0x0000
packBytes :: Word8 -> Word8 -> Word16
packBytes b1 b2 = shift (fromIntegral b1) 8 .|. fromIntegral b2

prop_extractTitle = extractTitle exampleRom == "TOBU"
extractTitle :: ByteString -> String
extractTitle rom = UTF8.toString result
    where
        rawBytes = rangeFromByteString rom titleStartByte titleEndByte
        (result, _) = B.break (== 0) rawBytes

prop_extractLicenseeCode = extractLicenseeCode exampleRom == None
extractLicenseeCode :: ByteString -> LicenseeCode
extractLicenseeCode rom = case maybeCode of
    (Just code) -> code
    Nothing -> None
    where
        maybeCode = Data.Map.lookup (B.index rom licenseeByte) licenseeMap

prop_extractSGBFlag = not (extractSGBFlag exampleRom)
extractSGBFlag :: ByteString -> Bool
extractSGBFlag rom = B.index rom sGBFlagByte == 0x03

prop_extractCartridgeType = extractCartridgeType exampleRom == MBC1_RAM_BATTERY
extractCartridgeType :: ByteString -> CartridgeType
extractCartridgeType rom = case maybeCartridgeType of
    (Just cartridgeType) -> cartridgeType
    Nothing -> ROM_ONLY
    where
        maybeCartridgeType = Data.Map.lookup (B.index rom cartridgeTypeByte) cartridgeTypeMap

prop_extractRomSize = extractRomSize exampleRom == RomSize_256_KB
extractRomSize :: ByteString -> RomSize
extractRomSize rom = case maybeRomSize of
    (Just romSize) -> romSize
    Nothing -> RomSize_32_KB
    where
        maybeRomSize = Data.Map.lookup (B.index rom romSizeByte) romSizeMap

prop_extractRamSize = extractRamSize exampleRom == RamSize_8_KB
extractRamSize :: ByteString -> RamSize
extractRamSize rom = case maybeRamSize of
    (Just ramSize) -> ramSize
    Nothing -> RamSize_8_KB
    where
        maybeRamSize = Data.Map.lookup (B.index rom ramSizeByte) ramSizeMap

prop_extractDestinationCode = extractDestinationCode exampleRom == Japanese
extractDestinationCode :: ByteString -> DestinationCode
extractDestinationCode rom
    | destinationCode == 0x00 = Japanese
    | otherwise = NonJapanese
    where
        destinationCode = B.index rom destinationCodeByte

prop_extractVersionNumber = extractVersionNumber exampleRom == 1
extractVersionNumber :: ByteString -> Word8
extractVersionNumber = flip B.index versionNumberByte

prop_extractHeaderChecksum = extractHeaderChecksum exampleRom == 0xa4
extractHeaderChecksum :: ByteString -> Word8
extractHeaderChecksum = flip B.index headerChecksumByte

prop_extractGlobalChecksum = extractGlobalChecksum exampleRom == 0xb596
extractGlobalChecksum :: ByteString -> Word16
extractGlobalChecksum rom = packBytes (B.index rom globalChecksumStartByte) (B.index rom globalChecksumEndByte)

prop_computeHeaderChecksum = computeHeaderChecksum exampleRom == 0xa4
computeHeaderChecksum :: ByteString -> Word8
computeHeaderChecksum rom = checksum
    where
        header = rangeFromByteString rom titleStartByte headerChecksumByte
        checksum = foldl (\x y -> x - y - 1) 0 (unpack header)

prop_verifyHeaderChecksum = verifyHeaderChecksum exampleRom == 0xa4
verifyHeaderChecksum :: ByteString -> Word8
verifyHeaderChecksum rom = if romChecksum == computedChecksum then romChecksum else throw $ InvalidHeaderChecksum romChecksum computedChecksum 
    where
        romChecksum = extractHeaderChecksum rom
        computedChecksum = computeHeaderChecksum rom

return []
runTests = $quickCheckAll
