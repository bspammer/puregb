{-# LANGUAGE TemplateHaskell #-}
module Rom.Rom where

import Data.Map (lookup)
import Data.Bits ((.|.), shift)
import Data.ByteString as B (ByteString, readFile, index, splitAt, pack, break)
import Data.ByteString.UTF8 as UTF8 (toString)
import Path.Posix (Path, Abs, File, fromAbsFile, mkAbsFile)
import Data.Word (Word8, Word16)
import Data.FileEmbed (embedFile)
import Test.QuickCheck
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Gen (elements)
import Rom.Enum (LicenseeCode(..), licenseeMap, CartridgeType(..), cartridgeTypeMap, RomSize(..), romSizeMap, RamSize(..), ramSizeMap, DestinationCode(..))

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

exampleRom = $(embedFile "data/tobutobugirl/tobu.gb")

readRom :: FilePath -> IO Rom
readRom filepath = do
    rawBytes <- B.readFile filepath
    return Rom {
        title = extractTitle rawBytes,
        licenseeCode = extractLicenseeCode rawBytes,
        sGBFlag = extractSGBFlag rawBytes,
        cartridgeType = extractCartridgeType rawBytes,
        romSize = extractRomSize rawBytes,
        ramSize = extractRamSize rawBytes,
        destinationCode = extractDestinationCode rawBytes,
        versionNumber = extractVersionNumber rawBytes,
        headerChecksum = extractHeaderChecksum rawBytes,
        globalChecksum = extractGlobalChecksum rawBytes
    }

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
        titleStartByte = 0x134
        titleEndByte = 0x143
        rawBytes = rangeFromByteString rom titleStartByte titleEndByte
        (result, _) = B.break (== 0) rawBytes

prop_extractLicenseeCode = extractLicenseeCode exampleRom == None
extractLicenseeCode :: ByteString -> LicenseeCode
extractLicenseeCode rom = case maybeCode of
    (Just code) -> code
    Nothing -> None
    where
        licenseeByte = 0x144
        maybeCode = Data.Map.lookup (B.index rom licenseeByte) licenseeMap

prop_extractSGBFlag = not (extractSGBFlag exampleRom)
extractSGBFlag :: ByteString -> Bool
extractSGBFlag rom = B.index rom sGBFlagByte == 0x03
    where sGBFlagByte = 0x146

prop_extractCartridgeType = extractCartridgeType exampleRom == MBC1_RAM_BATTERY
extractCartridgeType :: ByteString -> CartridgeType
extractCartridgeType rom = case maybeCartridgeType of
    (Just cartridgeType) -> cartridgeType
    Nothing -> ROM_ONLY
    where
        cartridgeTypeByte = 0x147
        maybeCartridgeType = Data.Map.lookup (B.index rom cartridgeTypeByte) cartridgeTypeMap

prop_extractRomSize = extractRomSize exampleRom == RomSize_256_KB
extractRomSize :: ByteString -> RomSize
extractRomSize rom = case maybeRomSize of
    (Just romSize) -> romSize
    Nothing -> RomSize_32_KB
    where
        romSizeByte = 0x148
        maybeRomSize = Data.Map.lookup (B.index rom romSizeByte) romSizeMap 

prop_extractRamSize = extractRamSize exampleRom == RamSize_8_KB
extractRamSize :: ByteString -> RamSize
extractRamSize rom = case maybeRamSize of
    (Just ramSize) -> ramSize
    Nothing -> RamSize_8_KB
    where
        ramSizeByte = 0x149
        maybeRamSize = Data.Map.lookup (B.index rom ramSizeByte) ramSizeMap 

prop_extractDestinationCode = extractDestinationCode exampleRom == Japanese
extractDestinationCode :: ByteString -> DestinationCode
extractDestinationCode rom
    | destinationCode == 0x00 = Japanese
    | otherwise = NonJapanese
    where
        destinationCode = B.index rom 0x14a

prop_extractVersionNumber = extractVersionNumber exampleRom == 1
extractVersionNumber :: ByteString -> Word8
extractVersionNumber = flip B.index 0x14c

prop_extractHeaderChecksum = extractHeaderChecksum exampleRom == 0xa4
extractHeaderChecksum :: ByteString -> Word8
extractHeaderChecksum = flip B.index 0x14d

prop_extractGlobalChecksum = extractGlobalChecksum exampleRom == 0xb596
extractGlobalChecksum :: ByteString -> Word16
extractGlobalChecksum rom = packBytes (B.index rom 0x14e) (B.index rom 0x14f)

return []
runTests = $quickCheckAll