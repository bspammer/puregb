module Rom.Enum where
import Data.Map (fromList, Map)
import Data.Word (Word8)

data LicenseeCode = None
    | Nintendo_RD1
    | Capcom
    | Electronic_Arts
    | Hudson_Soft
    | B_ai
    | Kss
    | Pow
    | PCM_Complete
    | San_x
    | Kemco_Japan
    | Seta
    | Viacom
    | Nintendo
    | Bandai
    | Ocean_Acclaim
    | Konami
    | Hector
    | Taito
    | Hudson
    | Banpresto
    | Ubi_Soft
    | Atlus
    | Malibu
    | Angel
    | Bullet_Proof
    | Irem
    | Absolute
    | Acclaim
    | Activision
    | American_sammy
    | Hi_tech_entertainment
    | LJN
    | Matchbox
    | Mattel
    | Milton_Bradley
    | Titus
    | Virgin
    | LucasArts
    | Ocean
    | Infogrames
    | Interplay
    | Broderbund
    | Sculptured
    | Sci
    | THQ
    | Accolade
    | Misawa
    | Lozc
    | Tokuma_Shoten_Intermedia
    | Tsukuda_Original
    | Chunsoft
    | Video_system
    | Varie
    | Yonezawas_pal
    | Kaneko
    | Pack_in_soft
    | Konami_Yu_Gi_Oh deriving (Eq ,Show)

licenseeMap :: Map Word8 LicenseeCode
licenseeMap = fromList [
    (0x00, None),
    (0x01, Nintendo_RD1),
    (0x08, Capcom),
    (0x13, Electronic_Arts),
    (0x18, Hudson_Soft),
    (0x19, B_ai),
    (0x20, Kss),
    (0x22, Pow),
    (0x24, PCM_Complete),
    (0x25, San_x),
    (0x28, Kemco_Japan),
    (0x29, Seta),
    (0x30, Viacom),
    (0x31, Nintendo),
    (0x32, Bandai),
    (0x33, Ocean_Acclaim),
    (0x34, Konami),
    (0x35, Hector),
    (0x37, Taito),
    (0x38, Hudson),
    (0x39, Banpresto),
    (0x41, Ubi_Soft),
    (0x42, Atlus),
    (0x44, Malibu),
    (0x46, Angel),
    (0x47, Bullet_Proof),
    (0x49, Irem),
    (0x50, Absolute),
    (0x51, Acclaim),
    (0x52, Activision),
    (0x53, American_sammy),
    (0x54, Konami),
    (0x55, Hi_tech_entertainment),
    (0x56, LJN),
    (0x57, Matchbox),
    (0x58, Mattel),
    (0x59, Milton_Bradley),
    (0x60, Titus),
    (0x61, Virgin),
    (0x64, LucasArts),
    (0x67, Ocean),
    (0x69, Electronic_Arts),
    (0x70, Infogrames),
    (0x71, Interplay),
    (0x72, Broderbund),
    (0x73, Sculptured),
    (0x75, Sci),
    (0x78, THQ),
    (0x79, Accolade),
    (0x80, Misawa),
    (0x83, Lozc),
    (0x86, Tokuma_Shoten_Intermedia),
    (0x87, Tsukuda_Original),
    (0x91, Chunsoft),
    (0x92, Video_system),
    (0x93, Ocean_Acclaim),
    (0x95, Varie),
    (0x96, Yonezawas_pal),
    (0x97, Kaneko),
    (0x99, Pack_in_soft),
    (0xA4, Konami_Yu_Gi_Oh)
    ]

data CartridgeType = ROM_ONLY
    | MBC1
    | MBC1_RAM
    | MBC1_RAM_BATTERY
    | MBC2
    | MBC2_BATTERY
    | ROM_RAM
    | ROM_RAM_BATTERY
    | MMM01
    | MMM01_RAM
    | MMM01_RAM_BATTERY
    | MBC3_TIMER_BATTERY
    | MBC3_TIMER_RAM_BATTERY
    | MBC3
    | MBC3_RAM
    | MBC3_RAM_BATTERY
    | MBC5
    | MBC5_RAM
    | MBC5_RAM_BATTERY
    | MBC5_RUMBLE
    | MBC5_RUMBLE_RAM
    | MBC5_RUMBLE_RAM_BATTERY
    | MBC6
    | MBC7_SENSOR_RUMBLE_RAM_BATTERY
    | POCKET_CAMERA
    | BANDAI_TAMA5
    | HuC3
    | HuC1_RAM_BATTERY deriving (Eq, Show)

cartridgeTypeMap :: Map Word8 CartridgeType
cartridgeTypeMap = fromList [
    (0x00, ROM_ONLY),
    (0x01, MBC1),
    (0x02, MBC1_RAM),
    (0x03, MBC1_RAM_BATTERY),
    (0x05, MBC2),
    (0x06, MBC2_BATTERY),
    (0x08, ROM_RAM),
    (0x09, ROM_RAM_BATTERY),
    (0x0B, MMM01),
    (0x0C, MMM01_RAM),
    (0x0D, MMM01_RAM_BATTERY),
    (0x0F, MBC3_TIMER_BATTERY),
    (0x10, MBC3_TIMER_RAM_BATTERY),
    (0x11, MBC3),
    (0x12, MBC3_RAM),
    (0x13, MBC3_RAM_BATTERY),
    (0x19, MBC5),
    (0x1A, MBC5_RAM),
    (0x1B, MBC5_RAM_BATTERY),
    (0x1C, MBC5_RUMBLE),
    (0x1D, MBC5_RUMBLE_RAM),
    (0x1E, MBC5_RUMBLE_RAM_BATTERY),
    (0x20, MBC6),
    (0x22, MBC7_SENSOR_RUMBLE_RAM_BATTERY),
    (0xFC, POCKET_CAMERA),
    (0xFD, BANDAI_TAMA5),
    (0xFE, HuC3),
    (0xFF, HuC1_RAM_BATTERY)
    ]

data RomSize = RomSize_32_KB
    | RomSize_64_KB
    | RomSize_128_KB
    | RomSize_256_KB
    | RomSize_512_KB
    | RomSize_1_MB
    | RomSize_2_MB
    | RomSize_4_MB
    | RomSize_8_MB
    | RomSize_1_1_MB
    | RomSize_1_2_MB
    | RomSize_1_5_MB deriving (Eq, Show)

romSizeMap :: Map Word8 RomSize
romSizeMap = fromList [
    (0x00, RomSize_32_KB),
    (0x01, RomSize_64_KB),
    (0x02, RomSize_128_KB),
    (0x03, RomSize_256_KB),
    (0x04, RomSize_512_KB),
    (0x05, RomSize_1_MB),
    (0x06, RomSize_2_MB),
    (0x07, RomSize_4_MB),
    (0x08, RomSize_8_MB),
    (0x52, RomSize_1_1_MB),
    (0x53, RomSize_1_2_MB),
    (0x54, RomSize_1_5_MB)
    ]

data RamSize = RamSize_No_Ram
    | RamSize_8_KB
    | RamSize_32_KB
    | RamSize_128_KB
    | RamSize_64_KB deriving (Eq, Show)

ramSizeMap :: Map Word8 RamSize
ramSizeMap = fromList [
    (0x00, RamSize_No_Ram),
    (0x02, RamSize_8_KB),
    (0x03, RamSize_32_KB),
    (0x04, RamSize_128_KB),
    (0x05, RamSize_64_KB)
    ]

data DestinationCode = Japanese | NonJapanese deriving (Eq, Show)
