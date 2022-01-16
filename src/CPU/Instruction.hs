{-# LANGUAGE TemplateHaskell #-}
module CPU.Instruction where

import Data.Map (fromList, Map)
import Data.Word (Word8)
import Lens.Micro.Platform

import CPU ( CPU (..))

type RunnableInstruction = CPU -> CPU

type Instruction1 = Word8 -> RunnableInstruction
type Instruction2 = Word8 -> Word8 -> RunnableInstruction

stubInstruction :: String -> RunnableInstruction
stubInstruction text = error $ "Instruction with no implementation: " ++ text

-- 0x00 NOP, 1 byte operand, 4 cycles -,-,-,-
instruction_00 :: RunnableInstruction
instruction_00 = id

-- 0x01 "LD BC,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_01 :: Instruction2
instruction_01 b1 b2 = stubInstruction "0x01"

-- 0x02 "LD (BC),A", 1 byte operand, 8 cycles -,-,-,-
instruction_02 :: RunnableInstruction
instruction_02 = stubInstruction "0x02"

-- 0x03 INC BC, 1 byte operand, 8 cycles -,-,-,-
instruction_03 :: RunnableInstruction
instruction_03 = stubInstruction "0x03"

-- 0x04 INC B, 1 byte operand, 4 cycles Z,0,H,-
instruction_04 :: RunnableInstruction
instruction_04 = stubInstruction "0x04"

-- 0x05 DEC B, 1 byte operand, 4 cycles Z,1,H,-
instruction_05 :: RunnableInstruction
instruction_05 = stubInstruction "0x05"

-- 0x06 "LD B,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_06 :: Instruction1
instruction_06 b = stubInstruction "0x06"

-- 0x07 RLCA, 1 byte operand, 4 cycles 0,0,0,C
instruction_07 :: RunnableInstruction
instruction_07 = stubInstruction "0x07"

-- 0x08 "LD (a16),SP", 3 byte operand, 20 cycles -,-,-,-
instruction_08 :: Instruction2
instruction_08 b1 b2 = stubInstruction "0x08"

-- 0x09 "ADD HL,BC", 1 byte operand, 8 cycles -,0,H,C
instruction_09 :: RunnableInstruction
instruction_09 = stubInstruction "0x09"

-- 0x0a "LD A,(BC)", 1 byte operand, 8 cycles -,-,-,-
instruction_0a :: RunnableInstruction
instruction_0a = stubInstruction "0x0a"

-- 0x0b DEC BC, 1 byte operand, 8 cycles -,-,-,-
instruction_0b :: RunnableInstruction
instruction_0b = stubInstruction "0x0b"

-- 0x0c INC C, 1 byte operand, 4 cycles Z,0,H,-
instruction_0c :: RunnableInstruction
instruction_0c = stubInstruction "0x0c"

-- 0x0d DEC C, 1 byte operand, 4 cycles Z,1,H,-
instruction_0d :: RunnableInstruction
instruction_0d = stubInstruction "0x0d"

-- 0x0e "LD C,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_0e :: Instruction1
instruction_0e b = stubInstruction "0x0e"

-- 0x0f RRCA, 1 byte operand, 4 cycles 0,0,0,C
instruction_0f :: RunnableInstruction
instruction_0f = stubInstruction "0x0f"

-- 0x10 STOP 0, 2 byte operand, 4 cycles -,-,-,-
instruction_10 :: Instruction1
instruction_10 b = stubInstruction "0x10"

-- 0x11 "LD DE,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_11 :: Instruction2
instruction_11 b1 b2 = stubInstruction "0x11"

-- 0x12 "LD (DE),A", 1 byte operand, 8 cycles -,-,-,-
instruction_12 :: RunnableInstruction
instruction_12 = stubInstruction "0x12"

-- 0x13 INC DE, 1 byte operand, 8 cycles -,-,-,-
instruction_13 :: RunnableInstruction
instruction_13 = stubInstruction "0x13"

-- 0x14 INC D, 1 byte operand, 4 cycles Z,0,H,-
instruction_14 :: RunnableInstruction
instruction_14 = stubInstruction "0x14"

-- 0x15 DEC D, 1 byte operand, 4 cycles Z,1,H,-
instruction_15 :: RunnableInstruction
instruction_15 = stubInstruction "0x15"

-- 0x16 "LD D,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_16 :: Instruction1
instruction_16 b = stubInstruction "0x16"

-- 0x17 RLA, 1 byte operand, 4 cycles 0,0,0,C
instruction_17 :: RunnableInstruction
instruction_17 = stubInstruction "0x17"

-- 0x18 JR r8, 2 byte operand, 12 cycles -,-,-,-
instruction_18 :: Instruction1
instruction_18 b = stubInstruction "0x18"

-- 0x19 "ADD HL,DE", 1 byte operand, 8 cycles -,0,H,C
instruction_19 :: RunnableInstruction
instruction_19 = stubInstruction "0x19"

-- 0x1a "LD A,(DE)", 1 byte operand, 8 cycles -,-,-,-
instruction_1a :: RunnableInstruction
instruction_1a = stubInstruction "0x1a"

-- 0x1b DEC DE, 1 byte operand, 8 cycles -,-,-,-
instruction_1b :: RunnableInstruction
instruction_1b = stubInstruction "0x1b"

-- 0x1c INC E, 1 byte operand, 4 cycles Z,0,H,-
instruction_1c :: RunnableInstruction
instruction_1c = stubInstruction "0x1c"

-- 0x1d DEC E, 1 byte operand, 4 cycles Z,1,H,-
instruction_1d :: RunnableInstruction
instruction_1d = stubInstruction "0x1d"

-- 0x1e "LD E,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_1e :: Instruction1
instruction_1e b = stubInstruction "0x1e"

-- 0x1f RRA, 1 byte operand, 4 cycles 0,0,0,C
instruction_1f :: RunnableInstruction
instruction_1f = stubInstruction "0x1f"

-- 0x20 "JR NZ,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_20 :: Instruction1
instruction_20 b = stubInstruction "0x20"

-- 0x21 "LD HL,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_21 :: Instruction2
instruction_21 b1 b2 = stubInstruction "0x21"

-- 0x22 "LD (HL+),A", 1 byte operand, 8 cycles -,-,-,-
instruction_22 :: RunnableInstruction
instruction_22 = stubInstruction "0x22"

-- 0x23 INC HL, 1 byte operand, 8 cycles -,-,-,-
instruction_23 :: RunnableInstruction
instruction_23 = stubInstruction "0x23"

-- 0x24 INC H, 1 byte operand, 4 cycles Z,0,H,-
instruction_24 :: RunnableInstruction
instruction_24 = stubInstruction "0x24"

-- 0x25 DEC H, 1 byte operand, 4 cycles Z,1,H,-
instruction_25 :: RunnableInstruction
instruction_25 = stubInstruction "0x25"

-- 0x26 "LD H,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_26 :: Instruction1
instruction_26 b = stubInstruction "0x26"

-- 0x27 DAA, 1 byte operand, 4 cycles Z,-,0,C
instruction_27 :: RunnableInstruction
instruction_27 = stubInstruction "0x27"

-- 0x28 "JR Z,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_28 :: Instruction1
instruction_28 b = stubInstruction "0x28"

-- 0x29 "ADD HL,HL", 1 byte operand, 8 cycles -,0,H,C
instruction_29 :: RunnableInstruction
instruction_29 = stubInstruction "0x29"

-- 0x2a "LD A,(HL+)", 1 byte operand, 8 cycles -,-,-,-
instruction_2a :: RunnableInstruction
instruction_2a = stubInstruction "0x2a"

-- 0x2b DEC HL, 1 byte operand, 8 cycles -,-,-,-
instruction_2b :: RunnableInstruction
instruction_2b = stubInstruction "0x2b"

-- 0x2c INC L, 1 byte operand, 4 cycles Z,0,H,-
instruction_2c :: RunnableInstruction
instruction_2c = stubInstruction "0x2c"

-- 0x2d DEC L, 1 byte operand, 4 cycles Z,1,H,-
instruction_2d :: RunnableInstruction
instruction_2d = stubInstruction "0x2d"

-- 0x2e "LD L,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_2e :: Instruction1
instruction_2e b = stubInstruction "0x2e"

-- 0x2f CPL, 1 byte operand, 4 cycles -,1,1,-
instruction_2f :: RunnableInstruction
instruction_2f = stubInstruction "0x2f"

-- 0x30 "JR NC,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_30 :: Instruction1
instruction_30 b = stubInstruction "0x30"

-- 0x31 "LD SP,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_31 :: Instruction2
instruction_31 b1 b2 = stubInstruction "0x31"

-- 0x32 "LD (HL-),A", 1 byte operand, 8 cycles -,-,-,-
instruction_32 :: RunnableInstruction
instruction_32 = stubInstruction "0x32"

-- 0x33 INC SP, 1 byte operand, 8 cycles -,-,-,-
instruction_33 :: RunnableInstruction
instruction_33 = stubInstruction "0x33"

-- 0x34 INC (HL), 1 byte operand, 12 cycles Z,0,H,-
instruction_34 :: RunnableInstruction
instruction_34 = stubInstruction "0x34"

-- 0x35 DEC (HL), 1 byte operand, 12 cycles Z,1,H,-
instruction_35 :: RunnableInstruction
instruction_35 = stubInstruction "0x35"

-- 0x36 "LD (HL),d8", 2 byte operand, 12 cycles -,-,-,-
instruction_36 :: Instruction1
instruction_36 b = stubInstruction "0x36"

-- 0x37 SCF, 1 byte operand, 4 cycles -,0,0,1
instruction_37 :: RunnableInstruction
instruction_37 = stubInstruction "0x37"

-- 0x38 "JR C,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_38 :: Instruction1
instruction_38 b = stubInstruction "0x38"

-- 0x39 "ADD HL,SP", 1 byte operand, 8 cycles -,0,H,C
instruction_39 :: RunnableInstruction
instruction_39 = stubInstruction "0x39"

-- 0x3a "LD A,(HL-)", 1 byte operand, 8 cycles -,-,-,-
instruction_3a :: RunnableInstruction
instruction_3a = stubInstruction "0x3a"

-- 0x3b DEC SP, 1 byte operand, 8 cycles -,-,-,-
instruction_3b :: RunnableInstruction
instruction_3b = stubInstruction "0x3b"

-- 0x3c INC A, 1 byte operand, 4 cycles Z,0,H,-
instruction_3c :: RunnableInstruction
instruction_3c = stubInstruction "0x3c"

-- 0x3d DEC A, 1 byte operand, 4 cycles Z,1,H,-
instruction_3d :: RunnableInstruction
instruction_3d = stubInstruction "0x3d"

-- 0x3e "LD A,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_3e :: Instruction1
instruction_3e b = stubInstruction "0x3e"

-- 0x3f CCF, 1 byte operand, 4 cycles -,0,0,C
instruction_3f :: RunnableInstruction
instruction_3f = stubInstruction "0x3f"

-- 0x40 "LD B,B", 1 byte operand, 4 cycles -,-,-,-
instruction_40 :: RunnableInstruction
instruction_40 = stubInstruction "0x40"

-- 0x41 "LD B,C", 1 byte operand, 4 cycles -,-,-,-
instruction_41 :: RunnableInstruction
instruction_41 = stubInstruction "0x41"

-- 0x42 "LD B,D", 1 byte operand, 4 cycles -,-,-,-
instruction_42 :: RunnableInstruction
instruction_42 = stubInstruction "0x42"

-- 0x43 "LD B,E", 1 byte operand, 4 cycles -,-,-,-
instruction_43 :: RunnableInstruction
instruction_43 = stubInstruction "0x43"

-- 0x44 "LD B,H", 1 byte operand, 4 cycles -,-,-,-
instruction_44 :: RunnableInstruction
instruction_44 = stubInstruction "0x44"

-- 0x45 "LD B,L", 1 byte operand, 4 cycles -,-,-,-
instruction_45 :: RunnableInstruction
instruction_45 = stubInstruction "0x45"

-- 0x46 "LD B,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_46 :: RunnableInstruction
instruction_46 = stubInstruction "0x46"

-- 0x47 "LD B,A", 1 byte operand, 4 cycles -,-,-,-
instruction_47 :: RunnableInstruction
instruction_47 = stubInstruction "0x47"

-- 0x48 "LD C,B", 1 byte operand, 4 cycles -,-,-,-
instruction_48 :: RunnableInstruction
instruction_48 = stubInstruction "0x48"

-- 0x49 "LD C,C", 1 byte operand, 4 cycles -,-,-,-
instruction_49 :: RunnableInstruction
instruction_49 = stubInstruction "0x49"

-- 0x4a "LD C,D", 1 byte operand, 4 cycles -,-,-,-
instruction_4a :: RunnableInstruction
instruction_4a = stubInstruction "0x4a"

-- 0x4b "LD C,E", 1 byte operand, 4 cycles -,-,-,-
instruction_4b :: RunnableInstruction
instruction_4b = stubInstruction "0x4b"

-- 0x4c "LD C,H", 1 byte operand, 4 cycles -,-,-,-
instruction_4c :: RunnableInstruction
instruction_4c = stubInstruction "0x4c"

-- 0x4d "LD C,L", 1 byte operand, 4 cycles -,-,-,-
instruction_4d :: RunnableInstruction
instruction_4d = stubInstruction "0x4d"

-- 0x4e "LD C,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_4e :: RunnableInstruction
instruction_4e = stubInstruction "0x4e"

-- 0x4f "LD C,A", 1 byte operand, 4 cycles -,-,-,-
instruction_4f :: RunnableInstruction
instruction_4f = stubInstruction "0x4f"

-- 0x50 "LD D,B", 1 byte operand, 4 cycles -,-,-,-
instruction_50 :: RunnableInstruction
instruction_50 = stubInstruction "0x50"

-- 0x51 "LD D,C", 1 byte operand, 4 cycles -,-,-,-
instruction_51 :: RunnableInstruction
instruction_51 = stubInstruction "0x51"

-- 0x52 "LD D,D", 1 byte operand, 4 cycles -,-,-,-
instruction_52 :: RunnableInstruction
instruction_52 = stubInstruction "0x52"

-- 0x53 "LD D,E", 1 byte operand, 4 cycles -,-,-,-
instruction_53 :: RunnableInstruction
instruction_53 = stubInstruction "0x53"

-- 0x54 "LD D,H", 1 byte operand, 4 cycles -,-,-,-
instruction_54 :: RunnableInstruction
instruction_54 = stubInstruction "0x54"

-- 0x55 "LD D,L", 1 byte operand, 4 cycles -,-,-,-
instruction_55 :: RunnableInstruction
instruction_55 = stubInstruction "0x55"

-- 0x56 "LD D,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_56 :: RunnableInstruction
instruction_56 = stubInstruction "0x56"

-- 0x57 "LD D,A", 1 byte operand, 4 cycles -,-,-,-
instruction_57 :: RunnableInstruction
instruction_57 = stubInstruction "0x57"

-- 0x58 "LD E,B", 1 byte operand, 4 cycles -,-,-,-
instruction_58 :: RunnableInstruction
instruction_58 = stubInstruction "0x58"

-- 0x59 "LD E,C", 1 byte operand, 4 cycles -,-,-,-
instruction_59 :: RunnableInstruction
instruction_59 = stubInstruction "0x59"

-- 0x5a "LD E,D", 1 byte operand, 4 cycles -,-,-,-
instruction_5a :: RunnableInstruction
instruction_5a = stubInstruction "0x5a"

-- 0x5b "LD E,E", 1 byte operand, 4 cycles -,-,-,-
instruction_5b :: RunnableInstruction
instruction_5b = stubInstruction "0x5b"

-- 0x5c "LD E,H", 1 byte operand, 4 cycles -,-,-,-
instruction_5c :: RunnableInstruction
instruction_5c = stubInstruction "0x5c"

-- 0x5d "LD E,L", 1 byte operand, 4 cycles -,-,-,-
instruction_5d :: RunnableInstruction
instruction_5d = stubInstruction "0x5d"

-- 0x5e "LD E,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_5e :: RunnableInstruction
instruction_5e = stubInstruction "0x5e"

-- 0x5f "LD E,A", 1 byte operand, 4 cycles -,-,-,-
instruction_5f :: RunnableInstruction
instruction_5f = stubInstruction "0x5f"

-- 0x60 "LD H,B", 1 byte operand, 4 cycles -,-,-,-
instruction_60 :: RunnableInstruction
instruction_60 = stubInstruction "0x60"

-- 0x61 "LD H,C", 1 byte operand, 4 cycles -,-,-,-
instruction_61 :: RunnableInstruction
instruction_61 = stubInstruction "0x61"

-- 0x62 "LD H,D", 1 byte operand, 4 cycles -,-,-,-
instruction_62 :: RunnableInstruction
instruction_62 = stubInstruction "0x62"

-- 0x63 "LD H,E", 1 byte operand, 4 cycles -,-,-,-
instruction_63 :: RunnableInstruction
instruction_63 = stubInstruction "0x63"

-- 0x64 "LD H,H", 1 byte operand, 4 cycles -,-,-,-
instruction_64 :: RunnableInstruction
instruction_64 = stubInstruction "0x64"

-- 0x65 "LD H,L", 1 byte operand, 4 cycles -,-,-,-
instruction_65 :: RunnableInstruction
instruction_65 = stubInstruction "0x65"

-- 0x66 "LD H,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_66 :: RunnableInstruction
instruction_66 = stubInstruction "0x66"

-- 0x67 "LD H,A", 1 byte operand, 4 cycles -,-,-,-
instruction_67 :: RunnableInstruction
instruction_67 = stubInstruction "0x67"

-- 0x68 "LD L,B", 1 byte operand, 4 cycles -,-,-,-
instruction_68 :: RunnableInstruction
instruction_68 = stubInstruction "0x68"

-- 0x69 "LD L,C", 1 byte operand, 4 cycles -,-,-,-
instruction_69 :: RunnableInstruction
instruction_69 = stubInstruction "0x69"

-- 0x6a "LD L,D", 1 byte operand, 4 cycles -,-,-,-
instruction_6a :: RunnableInstruction
instruction_6a = stubInstruction "0x6a"

-- 0x6b "LD L,E", 1 byte operand, 4 cycles -,-,-,-
instruction_6b :: RunnableInstruction
instruction_6b = stubInstruction "0x6b"

-- 0x6c "LD L,H", 1 byte operand, 4 cycles -,-,-,-
instruction_6c :: RunnableInstruction
instruction_6c = stubInstruction "0x6c"

-- 0x6d "LD L,L", 1 byte operand, 4 cycles -,-,-,-
instruction_6d :: RunnableInstruction
instruction_6d = stubInstruction "0x6d"

-- 0x6e "LD L,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_6e :: RunnableInstruction
instruction_6e = stubInstruction "0x6e"

-- 0x6f "LD L,A", 1 byte operand, 4 cycles -,-,-,-
instruction_6f :: RunnableInstruction
instruction_6f = stubInstruction "0x6f"

-- 0x70 "LD (HL),B", 1 byte operand, 8 cycles -,-,-,-
instruction_70 :: RunnableInstruction
instruction_70 = stubInstruction "0x70"

-- 0x71 "LD (HL),C", 1 byte operand, 8 cycles -,-,-,-
instruction_71 :: RunnableInstruction
instruction_71 = stubInstruction "0x71"

-- 0x72 "LD (HL),D", 1 byte operand, 8 cycles -,-,-,-
instruction_72 :: RunnableInstruction
instruction_72 = stubInstruction "0x72"

-- 0x73 "LD (HL),E", 1 byte operand, 8 cycles -,-,-,-
instruction_73 :: RunnableInstruction
instruction_73 = stubInstruction "0x73"

-- 0x74 "LD (HL),H", 1 byte operand, 8 cycles -,-,-,-
instruction_74 :: RunnableInstruction
instruction_74 = stubInstruction "0x74"

-- 0x75 "LD (HL),L", 1 byte operand, 8 cycles -,-,-,-
instruction_75 :: RunnableInstruction
instruction_75 = stubInstruction "0x75"

-- 0x76 HALT, 1 byte operand, 4 cycles -,-,-,-
instruction_76 :: RunnableInstruction
instruction_76 = stubInstruction "0x76"

-- 0x77 "LD (HL),A", 1 byte operand, 8 cycles -,-,-,-
instruction_77 :: RunnableInstruction
instruction_77 = stubInstruction "0x77"

-- 0x78 "LD A,B", 1 byte operand, 4 cycles -,-,-,-
instruction_78 :: RunnableInstruction
instruction_78 = stubInstruction "0x78"

-- 0x79 "LD A,C", 1 byte operand, 4 cycles -,-,-,-
instruction_79 :: RunnableInstruction
instruction_79 = stubInstruction "0x79"

-- 0x7a "LD A,D", 1 byte operand, 4 cycles -,-,-,-
instruction_7a :: RunnableInstruction
instruction_7a = stubInstruction "0x7a"

-- 0x7b "LD A,E", 1 byte operand, 4 cycles -,-,-,-
instruction_7b :: RunnableInstruction
instruction_7b = stubInstruction "0x7b"

-- 0x7c "LD A,H", 1 byte operand, 4 cycles -,-,-,-
instruction_7c :: RunnableInstruction
instruction_7c = stubInstruction "0x7c"

-- 0x7d "LD A,L", 1 byte operand, 4 cycles -,-,-,-
instruction_7d :: RunnableInstruction
instruction_7d = stubInstruction "0x7d"

-- 0x7e "LD A,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_7e :: RunnableInstruction
instruction_7e = stubInstruction "0x7e"

-- 0x7f "LD A,A", 1 byte operand, 4 cycles -,-,-,-
instruction_7f :: RunnableInstruction
instruction_7f = stubInstruction "0x7f"

-- 0x80 "ADD A,B", 1 byte operand, 4 cycles Z,0,H,C
instruction_80 :: RunnableInstruction
instruction_80 = stubInstruction "0x80"

-- 0x81 "ADD A,C", 1 byte operand, 4 cycles Z,0,H,C
instruction_81 :: RunnableInstruction
instruction_81 = stubInstruction "0x81"

-- 0x82 "ADD A,D", 1 byte operand, 4 cycles Z,0,H,C
instruction_82 :: RunnableInstruction
instruction_82 = stubInstruction "0x82"

-- 0x83 "ADD A,E", 1 byte operand, 4 cycles Z,0,H,C
instruction_83 :: RunnableInstruction
instruction_83 = stubInstruction "0x83"

-- 0x84 "ADD A,H", 1 byte operand, 4 cycles Z,0,H,C
instruction_84 :: RunnableInstruction
instruction_84 = stubInstruction "0x84"

-- 0x85 "ADD A,L", 1 byte operand, 4 cycles Z,0,H,C
instruction_85 :: RunnableInstruction
instruction_85 = stubInstruction "0x85"

-- 0x86 "ADD A,(HL)", 1 byte operand, 8 cycles Z,0,H,C
instruction_86 :: RunnableInstruction
instruction_86 = stubInstruction "0x86"

-- 0x87 "ADD A,A", 1 byte operand, 4 cycles Z,0,H,C
instruction_87 :: RunnableInstruction
instruction_87 = stubInstruction "0x87"

-- 0x88 "ADC A,B", 1 byte operand, 4 cycles Z,0,H,C
instruction_88 :: RunnableInstruction
instruction_88 = stubInstruction "0x88"

-- 0x89 "ADC A,C", 1 byte operand, 4 cycles Z,0,H,C
instruction_89 :: RunnableInstruction
instruction_89 = stubInstruction "0x89"

-- 0x8a "ADC A,D", 1 byte operand, 4 cycles Z,0,H,C
instruction_8a :: RunnableInstruction
instruction_8a = stubInstruction "0x8a"

-- 0x8b "ADC A,E", 1 byte operand, 4 cycles Z,0,H,C
instruction_8b :: RunnableInstruction
instruction_8b = stubInstruction "0x8b"

-- 0x8c "ADC A,H", 1 byte operand, 4 cycles Z,0,H,C
instruction_8c :: RunnableInstruction
instruction_8c = stubInstruction "0x8c"

-- 0x8d "ADC A,L", 1 byte operand, 4 cycles Z,0,H,C
instruction_8d :: RunnableInstruction
instruction_8d = stubInstruction "0x8d"

-- 0x8e "ADC A,(HL)", 1 byte operand, 8 cycles Z,0,H,C
instruction_8e :: RunnableInstruction
instruction_8e = stubInstruction "0x8e"

-- 0x8f "ADC A,A", 1 byte operand, 4 cycles Z,0,H,C
instruction_8f :: RunnableInstruction
instruction_8f = stubInstruction "0x8f"

-- 0x90 SUB B, 1 byte operand, 4 cycles Z,1,H,C
instruction_90 :: RunnableInstruction
instruction_90 = stubInstruction "0x90"

-- 0x91 SUB C, 1 byte operand, 4 cycles Z,1,H,C
instruction_91 :: RunnableInstruction
instruction_91 = stubInstruction "0x91"

-- 0x92 SUB D, 1 byte operand, 4 cycles Z,1,H,C
instruction_92 :: RunnableInstruction
instruction_92 = stubInstruction "0x92"

-- 0x93 SUB E, 1 byte operand, 4 cycles Z,1,H,C
instruction_93 :: RunnableInstruction
instruction_93 = stubInstruction "0x93"

-- 0x94 SUB H, 1 byte operand, 4 cycles Z,1,H,C
instruction_94 :: RunnableInstruction
instruction_94 = stubInstruction "0x94"

-- 0x95 SUB L, 1 byte operand, 4 cycles Z,1,H,C
instruction_95 :: RunnableInstruction
instruction_95 = stubInstruction "0x95"

-- 0x96 SUB (HL), 1 byte operand, 8 cycles Z,1,H,C
instruction_96 :: RunnableInstruction
instruction_96 = stubInstruction "0x96"

-- 0x97 SUB A, 1 byte operand, 4 cycles Z,1,H,C
instruction_97 :: RunnableInstruction
instruction_97 = stubInstruction "0x97"

-- 0x98 "SBC A,B", 1 byte operand, 4 cycles Z,1,H,C
instruction_98 :: RunnableInstruction
instruction_98 = stubInstruction "0x98"

-- 0x99 "SBC A,C", 1 byte operand, 4 cycles Z,1,H,C
instruction_99 :: RunnableInstruction
instruction_99 = stubInstruction "0x99"

-- 0x9a "SBC A,D", 1 byte operand, 4 cycles Z,1,H,C
instruction_9a :: RunnableInstruction
instruction_9a = stubInstruction "0x9a"

-- 0x9b "SBC A,E", 1 byte operand, 4 cycles Z,1,H,C
instruction_9b :: RunnableInstruction
instruction_9b = stubInstruction "0x9b"

-- 0x9c "SBC A,H", 1 byte operand, 4 cycles Z,1,H,C
instruction_9c :: RunnableInstruction
instruction_9c = stubInstruction "0x9c"

-- 0x9d "SBC A,L", 1 byte operand, 4 cycles Z,1,H,C
instruction_9d :: RunnableInstruction
instruction_9d = stubInstruction "0x9d"

-- 0x9e "SBC A,(HL)", 1 byte operand, 8 cycles Z,1,H,C
instruction_9e :: RunnableInstruction
instruction_9e = stubInstruction "0x9e"

-- 0x9f "SBC A,A", 1 byte operand, 4 cycles Z,1,H,C
instruction_9f :: RunnableInstruction
instruction_9f = stubInstruction "0x9f"

-- 0xa0 AND B, 1 byte operand, 4 cycles Z,0,1,0
instruction_a0 :: RunnableInstruction
instruction_a0 = stubInstruction "0xa0"

-- 0xa1 AND C, 1 byte operand, 4 cycles Z,0,1,0
instruction_a1 :: RunnableInstruction
instruction_a1 = stubInstruction "0xa1"

-- 0xa2 AND D, 1 byte operand, 4 cycles Z,0,1,0
instruction_a2 :: RunnableInstruction
instruction_a2 = stubInstruction "0xa2"

-- 0xa3 AND E, 1 byte operand, 4 cycles Z,0,1,0
instruction_a3 :: RunnableInstruction
instruction_a3 = stubInstruction "0xa3"

-- 0xa4 AND H, 1 byte operand, 4 cycles Z,0,1,0
instruction_a4 :: RunnableInstruction
instruction_a4 = stubInstruction "0xa4"

-- 0xa5 AND L, 1 byte operand, 4 cycles Z,0,1,0
instruction_a5 :: RunnableInstruction
instruction_a5 = stubInstruction "0xa5"

-- 0xa6 AND (HL), 1 byte operand, 8 cycles Z,0,1,0
instruction_a6 :: RunnableInstruction
instruction_a6 = stubInstruction "0xa6"

-- 0xa7 AND A, 1 byte operand, 4 cycles Z,0,1,0
instruction_a7 :: RunnableInstruction
instruction_a7 = stubInstruction "0xa7"

-- 0xa8 XOR B, 1 byte operand, 4 cycles Z,0,0,0
instruction_a8 :: RunnableInstruction
instruction_a8 = stubInstruction "0xa8"

-- 0xa9 XOR C, 1 byte operand, 4 cycles Z,0,0,0
instruction_a9 :: RunnableInstruction
instruction_a9 = stubInstruction "0xa9"

-- 0xaa XOR D, 1 byte operand, 4 cycles Z,0,0,0
instruction_aa :: RunnableInstruction
instruction_aa = stubInstruction "0xaa"

-- 0xab XOR E, 1 byte operand, 4 cycles Z,0,0,0
instruction_ab :: RunnableInstruction
instruction_ab = stubInstruction "0xab"

-- 0xac XOR H, 1 byte operand, 4 cycles Z,0,0,0
instruction_ac :: RunnableInstruction
instruction_ac = stubInstruction "0xac"

-- 0xad XOR L, 1 byte operand, 4 cycles Z,0,0,0
instruction_ad :: RunnableInstruction
instruction_ad = stubInstruction "0xad"

-- 0xae XOR (HL), 1 byte operand, 8 cycles Z,0,0,0
instruction_ae :: RunnableInstruction
instruction_ae = stubInstruction "0xae"

-- 0xaf XOR A, 1 byte operand, 4 cycles Z,0,0,0
instruction_af :: RunnableInstruction
instruction_af = stubInstruction "0xaf"

-- 0xb0 OR B, 1 byte operand, 4 cycles Z,0,0,0
instruction_b0 :: RunnableInstruction
instruction_b0 = stubInstruction "0xb0"

-- 0xb1 OR C, 1 byte operand, 4 cycles Z,0,0,0
instruction_b1 :: RunnableInstruction
instruction_b1 = stubInstruction "0xb1"

-- 0xb2 OR D, 1 byte operand, 4 cycles Z,0,0,0
instruction_b2 :: RunnableInstruction
instruction_b2 = stubInstruction "0xb2"

-- 0xb3 OR E, 1 byte operand, 4 cycles Z,0,0,0
instruction_b3 :: RunnableInstruction
instruction_b3 = stubInstruction "0xb3"

-- 0xb4 OR H, 1 byte operand, 4 cycles Z,0,0,0
instruction_b4 :: RunnableInstruction
instruction_b4 = stubInstruction "0xb4"

-- 0xb5 OR L, 1 byte operand, 4 cycles Z,0,0,0
instruction_b5 :: RunnableInstruction
instruction_b5 = stubInstruction "0xb5"

-- 0xb6 OR (HL), 1 byte operand, 8 cycles Z,0,0,0
instruction_b6 :: RunnableInstruction
instruction_b6 = stubInstruction "0xb6"

-- 0xb7 OR A, 1 byte operand, 4 cycles Z,0,0,0
instruction_b7 :: RunnableInstruction
instruction_b7 = stubInstruction "0xb7"

-- 0xb8 CP B, 1 byte operand, 4 cycles Z,1,H,C
instruction_b8 :: RunnableInstruction
instruction_b8 = stubInstruction "0xb8"

-- 0xb9 CP C, 1 byte operand, 4 cycles Z,1,H,C
instruction_b9 :: RunnableInstruction
instruction_b9 = stubInstruction "0xb9"

-- 0xba CP D, 1 byte operand, 4 cycles Z,1,H,C
instruction_ba :: RunnableInstruction
instruction_ba = stubInstruction "0xba"

-- 0xbb CP E, 1 byte operand, 4 cycles Z,1,H,C
instruction_bb :: RunnableInstruction
instruction_bb = stubInstruction "0xbb"

-- 0xbc CP H, 1 byte operand, 4 cycles Z,1,H,C
instruction_bc :: RunnableInstruction
instruction_bc = stubInstruction "0xbc"

-- 0xbd CP L, 1 byte operand, 4 cycles Z,1,H,C
instruction_bd :: RunnableInstruction
instruction_bd = stubInstruction "0xbd"

-- 0xbe CP (HL), 1 byte operand, 8 cycles Z,1,H,C
instruction_be :: RunnableInstruction
instruction_be = stubInstruction "0xbe"

-- 0xbf CP A, 1 byte operand, 4 cycles Z,1,H,C
instruction_bf :: RunnableInstruction
instruction_bf = stubInstruction "0xbf"

-- 0xc0 RET NZ, 1 byte operand, 20/8 cycles -,-,-,-
instruction_c0 :: RunnableInstruction
instruction_c0 = stubInstruction "0xc0"

-- 0xc1 POP BC, 1 byte operand, 12 cycles -,-,-,-
instruction_c1 :: RunnableInstruction
instruction_c1 = stubInstruction "0xc1"

-- 0xc2 "JP NZ,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_c2 :: Instruction2
instruction_c2 b1 b2 = stubInstruction "0xc2"

-- 0xc3 JP a16, 3 byte operand, 16 cycles -,-,-,-
instruction_c3 :: Instruction2
instruction_c3 b1 b2 = stubInstruction "0xc3"

-- 0xc4 "CALL NZ,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_c4 :: Instruction2
instruction_c4 b1 b2 = stubInstruction "0xc4"

-- 0xc5 PUSH BC, 1 byte operand, 16 cycles -,-,-,-
instruction_c5 :: RunnableInstruction
instruction_c5 = stubInstruction "0xc5"

-- 0xc6 "ADD A,d8", 2 byte operand, 8 cycles Z,0,H,C
instruction_c6 :: Instruction1
instruction_c6 b = stubInstruction "0xc6"

-- 0xc7 RST 00H, 1 byte operand, 16 cycles -,-,-,-
instruction_c7 :: RunnableInstruction
instruction_c7 = stubInstruction "0xc7"

-- 0xc8 RET Z, 1 byte operand, 20/8 cycles -,-,-,-
instruction_c8 :: RunnableInstruction
instruction_c8 = stubInstruction "0xc8"

-- 0xc9 RET, 1 byte operand, 16 cycles -,-,-,-
instruction_c9 :: RunnableInstruction
instruction_c9 = stubInstruction "0xc9"

-- 0xca "JP Z,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_ca :: Instruction2
instruction_ca b1 b2 = stubInstruction "0xca"

-- 0xcb PREFIX CB, 1 byte operand, 4 cycles -,-,-,-
instruction_cb :: RunnableInstruction
instruction_cb = stubInstruction "0xcb"

-- 0xcc "CALL Z,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_cc :: Instruction2
instruction_cc b1 b2 = stubInstruction "0xcc"

-- 0xcd CALL a16, 3 byte operand, 24 cycles -,-,-,-
instruction_cd :: Instruction2
instruction_cd b1 b2 = stubInstruction "0xcd"

-- 0xce "ADC A,d8", 2 byte operand, 8 cycles Z,0,H,C
instruction_ce :: Instruction1
instruction_ce b = stubInstruction "0xce"

-- 0xcf RST 08H, 1 byte operand, 16 cycles -,-,-,-
instruction_cf :: RunnableInstruction
instruction_cf = stubInstruction "0xcf"

-- 0xd0 RET NC, 1 byte operand, 20/8 cycles -,-,-,-
instruction_d0 :: RunnableInstruction
instruction_d0 = stubInstruction "0xd0"

-- 0xd1 POP DE, 1 byte operand, 12 cycles -,-,-,-
instruction_d1 :: RunnableInstruction
instruction_d1 = stubInstruction "0xd1"

-- 0xd2 "JP NC,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_d2 :: Instruction2
instruction_d2 b1 b2 = stubInstruction "0xd2"

-- 0xd3 ,,,,,,
instruction_d3 :: RunnableInstruction
instruction_d3 = stubInstruction "0xd3"

-- 0xd4 "CALL NC,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_d4 :: Instruction2
instruction_d4 b1 b2 = stubInstruction "0xd4"

-- 0xd5 PUSH DE, 1 byte operand, 16 cycles -,-,-,-
instruction_d5 :: RunnableInstruction
instruction_d5 = stubInstruction "0xd5"

-- 0xd6 SUB d8, 2 byte operand, 8 cycles Z,1,H,C
instruction_d6 :: Instruction1
instruction_d6 b = stubInstruction "0xd6"

-- 0xd7 RST 10H, 1 byte operand, 16 cycles -,-,-,-
instruction_d7 :: RunnableInstruction
instruction_d7 = stubInstruction "0xd7"

-- 0xd8 RET C, 1 byte operand, 20/8 cycles -,-,-,-
instruction_d8 :: RunnableInstruction
instruction_d8 = stubInstruction "0xd8"

-- 0xd9 RETI, 1 byte operand, 16 cycles -,-,-,-
instruction_d9 :: RunnableInstruction
instruction_d9 = stubInstruction "0xd9"

-- 0xda "JP C,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_da :: Instruction2
instruction_da b1 b2 = stubInstruction "0xda"

-- 0xdb ,,,,,,
instruction_db :: RunnableInstruction
instruction_db = stubInstruction "0xdb"

-- 0xdc "CALL C,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_dc :: Instruction2
instruction_dc b1 b2 = stubInstruction "0xdc"

-- 0xdd,,,,,,,
instruction_dd :: RunnableInstruction
instruction_dd = stubInstruction "0xdd"

-- 0xde "SBC A,d8", 2 byte operand, 8 cycles Z,1,H,C
instruction_de :: Instruction1
instruction_de b = stubInstruction "0xde"

-- 0xdf RST 18H, 1 byte operand, 16 cycles -,-,-,-
instruction_df :: RunnableInstruction
instruction_df = stubInstruction "0xdf"

-- 0xe0 "LDH (a8),A", 2 byte operand, 12 cycles -,-,-,-
instruction_e0 :: Instruction1
instruction_e0 b = stubInstruction "0xe0"

-- 0xe1 POP HL, 1 byte operand, 12 cycles -,-,-,-
instruction_e1 :: RunnableInstruction
instruction_e1 = stubInstruction "0xe1"

-- 0xe2 "LD (C),A", 2 byte operand, 8 cycles -,-,-,-
instruction_e2 :: Instruction1
instruction_e2 b = stubInstruction "0xe2"

-- 0xe3,,,,,,,
instruction_e3 :: RunnableInstruction
instruction_e3 = stubInstruction "0xe3"

-- 0xe4,,,,,,,
instruction_e4 :: RunnableInstruction
instruction_e4 = stubInstruction "0xe4"

-- 0xe5 PUSH HL, 1 byte operand, 16 cycles -,-,-,-
instruction_e5 :: RunnableInstruction
instruction_e5 = stubInstruction "0xe5"

-- 0xe6 AND d8, 2 byte operand, 8 cycles Z,0,1,0
instruction_e6 :: Instruction1
instruction_e6 b = stubInstruction "0xe6"

-- 0xe7 RST 20H, 1 byte operand, 16 cycles -,-,-,-
instruction_e7 :: RunnableInstruction
instruction_e7 = stubInstruction "0xe7"

-- 0xe8 "ADD SP,r8", 2 byte operand, 16 cycles 0,0,H,C
instruction_e8 :: Instruction1
instruction_e8 b = stubInstruction "0xe8"

-- 0xe9 JP (HL), 1 byte operand, 4 cycles -,-,-,-
instruction_e9 :: RunnableInstruction
instruction_e9 = stubInstruction "0xe9"

-- 0xea "LD (a16),A", 3 byte operand, 16 cycles -,-,-,-
instruction_ea :: Instruction2
instruction_ea b1 b2 = stubInstruction "0xea"

-- 0xeb,,,,,,,
instruction_eb :: RunnableInstruction
instruction_eb = stubInstruction "0xeb"

-- 0xec,,,,,,,
instruction_ec :: RunnableInstruction
instruction_ec = stubInstruction "0xec"

-- 0xed,,,,,,,
instruction_ed :: RunnableInstruction
instruction_ed = stubInstruction "0xed"

-- 0xee XOR d8, 2 byte operand, 8 cycles Z,0,0,0
instruction_ee :: Instruction1
instruction_ee b = stubInstruction "0xee"

-- 0xef RST 28H, 1 byte operand, 16 cycles -,-,-,-
instruction_ef :: RunnableInstruction
instruction_ef = stubInstruction "0xef"

-- 0xf0 "LDH A,(a8)", 2 byte operand, 12 cycles -,-,-,-
instruction_f0 :: Instruction1
instruction_f0 b = stubInstruction "0xf0"

-- 0xf1 POP AF, 1 byte operand, 12 cycles Z,N,H,C
instruction_f1 :: RunnableInstruction
instruction_f1 = stubInstruction "0xf1"

-- 0xf2 "LD A,(C)", 2 byte operand, 8 cycles -,-,-,-
instruction_f2 :: Instruction1
instruction_f2 b = stubInstruction "0xf2"

-- 0xf3 DI, 1 byte operand, 4 cycles -,-,-,-
instruction_f3 :: RunnableInstruction
instruction_f3 = stubInstruction "0xf3"

-- 0xf4,,,,,,,
instruction_f4 :: RunnableInstruction
instruction_f4 = stubInstruction "0xf4"

-- 0xf5 PUSH AF, 1 byte operand, 16 cycles -,-,-,-
instruction_f5 :: RunnableInstruction
instruction_f5 = stubInstruction "0xf5"

-- 0xf6 OR d8, 2 byte operand, 8 cycles Z,0,0,0
instruction_f6 :: Instruction1
instruction_f6 b = stubInstruction "0xf6"

-- 0xf7 RST 30H, 1 byte operand, 16 cycles -,-,-,-
instruction_f7 :: RunnableInstruction
instruction_f7 = stubInstruction "0xf7"

-- 0xf8 "LD HL,SP+r8", 2 byte operand, 12 cycles 0,0,H,C
instruction_f8 :: Instruction1
instruction_f8 b = stubInstruction "0xf8"

-- 0xf9 "LD SP,HL", 1 byte operand, 8 cycles -,-,-,-
instruction_f9 :: RunnableInstruction
instruction_f9 = stubInstruction "0xf9"

-- 0xfa "LD A,(a16)", 3 byte operand, 16 cycles -,-,-,-
instruction_fa :: Instruction2
instruction_fa b1 b2 = stubInstruction "0xfa"

-- 0xfb EI, 1 byte operand, 4 cycles -,-,-,-
instruction_fb :: RunnableInstruction
instruction_fb = stubInstruction "0xfb"

-- 0xfc,,,,,,,
instruction_fc :: RunnableInstruction
instruction_fc = stubInstruction "0xfc"

-- 0xfd,,,,,,,
instruction_fd :: RunnableInstruction
instruction_fd = stubInstruction "0xfd"

-- 0xfe CP d8, 2 byte operand, 8 cycles Z,1,H,C
instruction_fe :: Instruction1
instruction_fe b = stubInstruction "0xfe"

-- 0xff RST 38H, 1 byte operand, 16 cycles -,-,-,-
instruction_ff :: RunnableInstruction
instruction_ff = stubInstruction "0xff"

-- 0xcb00 RLC B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb00 :: Instruction1
instruction_cb00 b = stubInstruction "0xcb00"

-- 0xcb01 RLC C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb01 :: Instruction1
instruction_cb01 b = stubInstruction "0xcb01"

-- 0xcb02 RLC D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb02 :: Instruction1
instruction_cb02 b = stubInstruction "0xcb02"

-- 0xcb03 RLC E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb03 :: Instruction1
instruction_cb03 b = stubInstruction "0xcb03"

-- 0xcb04 RLC H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb04 :: Instruction1
instruction_cb04 b = stubInstruction "0xcb04"

-- 0xcb05 RLC L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb05 :: Instruction1
instruction_cb05 b = stubInstruction "0xcb05"

-- 0xcb06 RLC (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb06 :: Instruction1
instruction_cb06 b = stubInstruction "0xcb06"

-- 0xcb07 RLC A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb07 :: Instruction1
instruction_cb07 b = stubInstruction "0xcb07"

-- 0xcb08 RRC B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb08 :: Instruction1
instruction_cb08 b = stubInstruction "0xcb08"

-- 0xcb09 RRC C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb09 :: Instruction1
instruction_cb09 b = stubInstruction "0xcb09"

-- 0xcb0a RRC D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0a :: Instruction1
instruction_cb0a b = stubInstruction "0xcb0a"

-- 0xcb0b RRC E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0b :: Instruction1
instruction_cb0b b = stubInstruction "0xcb0b"

-- 0xcb0c RRC H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0c :: Instruction1
instruction_cb0c b = stubInstruction "0xcb0c"

-- 0xcb0d RRC L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0d :: Instruction1
instruction_cb0d b = stubInstruction "0xcb0d"

-- 0xcb0e RRC (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb0e :: Instruction1
instruction_cb0e b = stubInstruction "0xcb0e"

-- 0xcb0f RRC A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0f :: Instruction1
instruction_cb0f b = stubInstruction "0xcb0f"

-- 0xcb10 RL B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb10 :: Instruction1
instruction_cb10 b = stubInstruction "0xcb10"

-- 0xcb11 RL C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb11 :: Instruction1
instruction_cb11 b = stubInstruction "0xcb11"

-- 0xcb12 RL D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb12 :: Instruction1
instruction_cb12 b = stubInstruction "0xcb12"

-- 0xcb13 RL E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb13 :: Instruction1
instruction_cb13 b = stubInstruction "0xcb13"

-- 0xcb14 RL H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb14 :: Instruction1
instruction_cb14 b = stubInstruction "0xcb14"

-- 0xcb15 RL L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb15 :: Instruction1
instruction_cb15 b = stubInstruction "0xcb15"

-- 0xcb16 RL (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb16 :: Instruction1
instruction_cb16 b = stubInstruction "0xcb16"

-- 0xcb17 RL A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb17 :: Instruction1
instruction_cb17 b = stubInstruction "0xcb17"

-- 0xcb18 RR B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb18 :: Instruction1
instruction_cb18 b = stubInstruction "0xcb18"

-- 0xcb19 RR C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb19 :: Instruction1
instruction_cb19 b = stubInstruction "0xcb19"

-- 0xcb1a RR D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1a :: Instruction1
instruction_cb1a b = stubInstruction "0xcb1a"

-- 0xcb1b RR E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1b :: Instruction1
instruction_cb1b b = stubInstruction "0xcb1b"

-- 0xcb1c RR H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1c :: Instruction1
instruction_cb1c b = stubInstruction "0xcb1c"

-- 0xcb1d RR L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1d :: Instruction1
instruction_cb1d b = stubInstruction "0xcb1d"

-- 0xcb1e RR (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb1e :: Instruction1
instruction_cb1e b = stubInstruction "0xcb1e"

-- 0xcb1f RR A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1f :: Instruction1
instruction_cb1f b = stubInstruction "0xcb1f"

-- 0xcb20 SLA B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb20 :: Instruction1
instruction_cb20 b = stubInstruction "0xcb20"

-- 0xcb21 SLA C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb21 :: Instruction1
instruction_cb21 b = stubInstruction "0xcb21"

-- 0xcb22 SLA D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb22 :: Instruction1
instruction_cb22 b = stubInstruction "0xcb22"

-- 0xcb23 SLA E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb23 :: Instruction1
instruction_cb23 b = stubInstruction "0xcb23"

-- 0xcb24 SLA H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb24 :: Instruction1
instruction_cb24 b = stubInstruction "0xcb24"

-- 0xcb25 SLA L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb25 :: Instruction1
instruction_cb25 b = stubInstruction "0xcb25"

-- 0xcb26 SLA (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb26 :: Instruction1
instruction_cb26 b = stubInstruction "0xcb26"

-- 0xcb27 SLA A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb27 :: Instruction1
instruction_cb27 b = stubInstruction "0xcb27"

-- 0xcb28 SRA B, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb28 :: Instruction1
instruction_cb28 b = stubInstruction "0xcb28"

-- 0xcb29 SRA C, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb29 :: Instruction1
instruction_cb29 b = stubInstruction "0xcb29"

-- 0xcb2a SRA D, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2a :: Instruction1
instruction_cb2a b = stubInstruction "0xcb2a"

-- 0xcb2b SRA E, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2b :: Instruction1
instruction_cb2b b = stubInstruction "0xcb2b"

-- 0xcb2c SRA H, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2c :: Instruction1
instruction_cb2c b = stubInstruction "0xcb2c"

-- 0xcb2d SRA L, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2d :: Instruction1
instruction_cb2d b = stubInstruction "0xcb2d"

-- 0xcb2e SRA (HL), 2 byte operand, 16 cycles Z,0,0,0
instruction_cb2e :: Instruction1
instruction_cb2e b = stubInstruction "0xcb2e"

-- 0xcb2f SRA A, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2f :: Instruction1
instruction_cb2f b = stubInstruction "0xcb2f"

-- 0xcb30 SWAP B, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb30 :: Instruction1
instruction_cb30 b = stubInstruction "0xcb30"

-- 0xcb31 SWAP C, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb31 :: Instruction1
instruction_cb31 b = stubInstruction "0xcb31"

-- 0xcb32 SWAP D, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb32 :: Instruction1
instruction_cb32 b = stubInstruction "0xcb32"

-- 0xcb33 SWAP E, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb33 :: Instruction1
instruction_cb33 b = stubInstruction "0xcb33"

-- 0xcb34 SWAP H, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb34 :: Instruction1
instruction_cb34 b = stubInstruction "0xcb34"

-- 0xcb35 SWAP L, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb35 :: Instruction1
instruction_cb35 b = stubInstruction "0xcb35"

-- 0xcb36 SWAP (HL), 2 byte operand, 16 cycles Z,0,0,0
instruction_cb36 :: Instruction1
instruction_cb36 b = stubInstruction "0xcb36"

-- 0xcb37 SWAP A, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb37 :: Instruction1
instruction_cb37 b = stubInstruction "0xcb37"

-- 0xcb38 SRL B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb38 :: Instruction1
instruction_cb38 b = stubInstruction "0xcb38"

-- 0xcb39 SRL C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb39 :: Instruction1
instruction_cb39 b = stubInstruction "0xcb39"

-- 0xcb3a SRL D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3a :: Instruction1
instruction_cb3a b = stubInstruction "0xcb3a"

-- 0xcb3b SRL E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3b :: Instruction1
instruction_cb3b b = stubInstruction "0xcb3b"

-- 0xcb3c SRL H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3c :: Instruction1
instruction_cb3c b = stubInstruction "0xcb3c"

-- 0xcb3d SRL L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3d :: Instruction1
instruction_cb3d b = stubInstruction "0xcb3d"

-- 0xcb3e SRL (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb3e :: Instruction1
instruction_cb3e b = stubInstruction "0xcb3e"

-- 0xcb3f SRL A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3f :: Instruction1
instruction_cb3f b = stubInstruction "0xcb3f"

-- 0xcb40 "BIT 0,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb40 :: Instruction1
instruction_cb40 b = stubInstruction "0xcb40"

-- 0xcb41 "BIT 0,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb41 :: Instruction1
instruction_cb41 b = stubInstruction "0xcb41"

-- 0xcb42 "BIT 0,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb42 :: Instruction1
instruction_cb42 b = stubInstruction "0xcb42"

-- 0xcb43 "BIT 0,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb43 :: Instruction1
instruction_cb43 b = stubInstruction "0xcb43"

-- 0xcb44 "BIT 0,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb44 :: Instruction1
instruction_cb44 b = stubInstruction "0xcb44"

-- 0xcb45 "BIT 0,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb45 :: Instruction1
instruction_cb45 b = stubInstruction "0xcb45"

-- 0xcb46 "BIT 0,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb46 :: Instruction1
instruction_cb46 b = stubInstruction "0xcb46"

-- 0xcb47 "BIT 0,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb47 :: Instruction1
instruction_cb47 b = stubInstruction "0xcb47"

-- 0xcb48 "BIT 1,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb48 :: Instruction1
instruction_cb48 b = stubInstruction "0xcb48"

-- 0xcb49 "BIT 1,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb49 :: Instruction1
instruction_cb49 b = stubInstruction "0xcb49"

-- 0xcb4a "BIT 1,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4a :: Instruction1
instruction_cb4a b = stubInstruction "0xcb4a"

-- 0xcb4b "BIT 1,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4b :: Instruction1
instruction_cb4b b = stubInstruction "0xcb4b"

-- 0xcb4c "BIT 1,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4c :: Instruction1
instruction_cb4c b = stubInstruction "0xcb4c"

-- 0xcb4d "BIT 1,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4d :: Instruction1
instruction_cb4d b = stubInstruction "0xcb4d"

-- 0xcb4e "BIT 1,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb4e :: Instruction1
instruction_cb4e b = stubInstruction "0xcb4e"

-- 0xcb4f "BIT 1,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4f :: Instruction1
instruction_cb4f b = stubInstruction "0xcb4f"

-- 0xcb50 "BIT 2,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb50 :: Instruction1
instruction_cb50 b = stubInstruction "0xcb50"

-- 0xcb51 "BIT 2,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb51 :: Instruction1
instruction_cb51 b = stubInstruction "0xcb51"

-- 0xcb52 "BIT 2,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb52 :: Instruction1
instruction_cb52 b = stubInstruction "0xcb52"

-- 0xcb53 "BIT 2,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb53 :: Instruction1
instruction_cb53 b = stubInstruction "0xcb53"

-- 0xcb54 "BIT 2,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb54 :: Instruction1
instruction_cb54 b = stubInstruction "0xcb54"

-- 0xcb55 "BIT 2,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb55 :: Instruction1
instruction_cb55 b = stubInstruction "0xcb55"

-- 0xcb56 "BIT 2,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb56 :: Instruction1
instruction_cb56 b = stubInstruction "0xcb56"

-- 0xcb57 "BIT 2,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb57 :: Instruction1
instruction_cb57 b = stubInstruction "0xcb57"

-- 0xcb58 "BIT 3,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb58 :: Instruction1
instruction_cb58 b = stubInstruction "0xcb58"

-- 0xcb59 "BIT 3,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb59 :: Instruction1
instruction_cb59 b = stubInstruction "0xcb59"

-- 0xcb5a "BIT 3,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5a :: Instruction1
instruction_cb5a b = stubInstruction "0xcb5a"

-- 0xcb5b "BIT 3,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5b :: Instruction1
instruction_cb5b b = stubInstruction "0xcb5b"

-- 0xcb5c "BIT 3,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5c :: Instruction1
instruction_cb5c b = stubInstruction "0xcb5c"

-- 0xcb5d "BIT 3,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5d :: Instruction1
instruction_cb5d b = stubInstruction "0xcb5d"

-- 0xcb5e "BIT 3,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb5e :: Instruction1
instruction_cb5e b = stubInstruction "0xcb5e"

-- 0xcb5f "BIT 3,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5f :: Instruction1
instruction_cb5f b = stubInstruction "0xcb5f"

-- 0xcb60 "BIT 4,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb60 :: Instruction1
instruction_cb60 b = stubInstruction "0xcb60"

-- 0xcb61 "BIT 4,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb61 :: Instruction1
instruction_cb61 b = stubInstruction "0xcb61"

-- 0xcb62 "BIT 4,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb62 :: Instruction1
instruction_cb62 b = stubInstruction "0xcb62"

-- 0xcb63 "BIT 4,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb63 :: Instruction1
instruction_cb63 b = stubInstruction "0xcb63"

-- 0xcb64 "BIT 4,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb64 :: Instruction1
instruction_cb64 b = stubInstruction "0xcb64"

-- 0xcb65 "BIT 4,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb65 :: Instruction1
instruction_cb65 b = stubInstruction "0xcb65"

-- 0xcb66 "BIT 4,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb66 :: Instruction1
instruction_cb66 b = stubInstruction "0xcb66"

-- 0xcb67 "BIT 4,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb67 :: Instruction1
instruction_cb67 b = stubInstruction "0xcb67"

-- 0xcb68 "BIT 5,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb68 :: Instruction1
instruction_cb68 b = stubInstruction "0xcb68"

-- 0xcb69 "BIT 5,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb69 :: Instruction1
instruction_cb69 b = stubInstruction "0xcb69"

-- 0xcb6a "BIT 5,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6a :: Instruction1
instruction_cb6a b = stubInstruction "0xcb6a"

-- 0xcb6b "BIT 5,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6b :: Instruction1
instruction_cb6b b = stubInstruction "0xcb6b"

-- 0xcb6c "BIT 5,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6c :: Instruction1
instruction_cb6c b = stubInstruction "0xcb6c"

-- 0xcb6d "BIT 5,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6d :: Instruction1
instruction_cb6d b = stubInstruction "0xcb6d"

-- 0xcb6e "BIT 5,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb6e :: Instruction1
instruction_cb6e b = stubInstruction "0xcb6e"

-- 0xcb6f "BIT 5,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6f :: Instruction1
instruction_cb6f b = stubInstruction "0xcb6f"

-- 0xcb70 "BIT 6,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb70 :: Instruction1
instruction_cb70 b = stubInstruction "0xcb70"

-- 0xcb71 "BIT 6,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb71 :: Instruction1
instruction_cb71 b = stubInstruction "0xcb71"

-- 0xcb72 "BIT 6,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb72 :: Instruction1
instruction_cb72 b = stubInstruction "0xcb72"

-- 0xcb73 "BIT 6,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb73 :: Instruction1
instruction_cb73 b = stubInstruction "0xcb73"

-- 0xcb74 "BIT 6,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb74 :: Instruction1
instruction_cb74 b = stubInstruction "0xcb74"

-- 0xcb75 "BIT 6,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb75 :: Instruction1
instruction_cb75 b = stubInstruction "0xcb75"

-- 0xcb76 "BIT 6,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb76 :: Instruction1
instruction_cb76 b = stubInstruction "0xcb76"

-- 0xcb77 "BIT 6,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb77 :: Instruction1
instruction_cb77 b = stubInstruction "0xcb77"

-- 0xcb78 "BIT 7,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb78 :: Instruction1
instruction_cb78 b = stubInstruction "0xcb78"

-- 0xcb79 "BIT 7,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb79 :: Instruction1
instruction_cb79 b = stubInstruction "0xcb79"

-- 0xcb7a "BIT 7,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7a :: Instruction1
instruction_cb7a b = stubInstruction "0xcb7a"

-- 0xcb7b "BIT 7,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7b :: Instruction1
instruction_cb7b b = stubInstruction "0xcb7b"

-- 0xcb7c "BIT 7,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7c :: Instruction1
instruction_cb7c b = stubInstruction "0xcb7c"

-- 0xcb7d "BIT 7,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7d :: Instruction1
instruction_cb7d b = stubInstruction "0xcb7d"

-- 0xcb7e "BIT 7,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb7e :: Instruction1
instruction_cb7e b = stubInstruction "0xcb7e"

-- 0xcb7f "BIT 7,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7f :: Instruction1
instruction_cb7f b = stubInstruction "0xcb7f"

-- 0xcb80 "RES 0,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb80 :: Instruction1
instruction_cb80 b = stubInstruction "0xcb80"

-- 0xcb81 "RES 0,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb81 :: Instruction1
instruction_cb81 b = stubInstruction "0xcb81"

-- 0xcb82 "RES 0,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb82 :: Instruction1
instruction_cb82 b = stubInstruction "0xcb82"

-- 0xcb83 "RES 0,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb83 :: Instruction1
instruction_cb83 b = stubInstruction "0xcb83"

-- 0xcb84 "RES 0,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb84 :: Instruction1
instruction_cb84 b = stubInstruction "0xcb84"

-- 0xcb85 "RES 0,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb85 :: Instruction1
instruction_cb85 b = stubInstruction "0xcb85"

-- 0xcb86 "RES 0,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb86 :: Instruction1
instruction_cb86 b = stubInstruction "0xcb86"

-- 0xcb87 "RES 0,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb87 :: Instruction1
instruction_cb87 b = stubInstruction "0xcb87"

-- 0xcb88 "RES 1,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb88 :: Instruction1
instruction_cb88 b = stubInstruction "0xcb88"

-- 0xcb89 "RES 1,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb89 :: Instruction1
instruction_cb89 b = stubInstruction "0xcb89"

-- 0xcb8a "RES 1,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8a :: Instruction1
instruction_cb8a b = stubInstruction "0xcb8a"

-- 0xcb8b "RES 1,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8b :: Instruction1
instruction_cb8b b = stubInstruction "0xcb8b"

-- 0xcb8c "RES 1,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8c :: Instruction1
instruction_cb8c b = stubInstruction "0xcb8c"

-- 0xcb8d "RES 1,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8d :: Instruction1
instruction_cb8d b = stubInstruction "0xcb8d"

-- 0xcb8e "RES 1,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb8e :: Instruction1
instruction_cb8e b = stubInstruction "0xcb8e"

-- 0xcb8f "RES 1,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8f :: Instruction1
instruction_cb8f b = stubInstruction "0xcb8f"

-- 0xcb90 "RES 2,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb90 :: Instruction1
instruction_cb90 b = stubInstruction "0xcb90"

-- 0xcb91 "RES 2,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb91 :: Instruction1
instruction_cb91 b = stubInstruction "0xcb91"

-- 0xcb92 "RES 2,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb92 :: Instruction1
instruction_cb92 b = stubInstruction "0xcb92"

-- 0xcb93 "RES 2,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb93 :: Instruction1
instruction_cb93 b = stubInstruction "0xcb93"

-- 0xcb94 "RES 2,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb94 :: Instruction1
instruction_cb94 b = stubInstruction "0xcb94"

-- 0xcb95 "RES 2,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb95 :: Instruction1
instruction_cb95 b = stubInstruction "0xcb95"

-- 0xcb96 "RES 2,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb96 :: Instruction1
instruction_cb96 b = stubInstruction "0xcb96"

-- 0xcb97 "RES 2,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb97 :: Instruction1
instruction_cb97 b = stubInstruction "0xcb97"

-- 0xcb98 "RES 3,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb98 :: Instruction1
instruction_cb98 b = stubInstruction "0xcb98"

-- 0xcb99 "RES 3,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb99 :: Instruction1
instruction_cb99 b = stubInstruction "0xcb99"

-- 0xcb9a "RES 3,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9a :: Instruction1
instruction_cb9a b = stubInstruction "0xcb9a"

-- 0xcb9b "RES 3,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9b :: Instruction1
instruction_cb9b b = stubInstruction "0xcb9b"

-- 0xcb9c "RES 3,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9c :: Instruction1
instruction_cb9c b = stubInstruction "0xcb9c"

-- 0xcb9d "RES 3,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9d :: Instruction1
instruction_cb9d b = stubInstruction "0xcb9d"

-- 0xcb9e "RES 3,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb9e :: Instruction1
instruction_cb9e b = stubInstruction "0xcb9e"

-- 0xcb9f "RES 3,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9f :: Instruction1
instruction_cb9f b = stubInstruction "0xcb9f"

-- 0xcba0 "RES 4,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cba0 :: Instruction1
instruction_cba0 b = stubInstruction "0xcba0"

-- 0xcba1 "RES 4,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cba1 :: Instruction1
instruction_cba1 b = stubInstruction "0xcba1"

-- 0xcba2 "RES 4,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cba2 :: Instruction1
instruction_cba2 b = stubInstruction "0xcba2"

-- 0xcba3 "RES 4,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cba3 :: Instruction1
instruction_cba3 b = stubInstruction "0xcba3"

-- 0xcba4 "RES 4,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cba4 :: Instruction1
instruction_cba4 b = stubInstruction "0xcba4"

-- 0xcba5 "RES 4,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cba5 :: Instruction1
instruction_cba5 b = stubInstruction "0xcba5"

-- 0xcba6 "RES 4,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cba6 :: Instruction1
instruction_cba6 b = stubInstruction "0xcba6"

-- 0xcba7 "RES 4,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cba7 :: Instruction1
instruction_cba7 b = stubInstruction "0xcba7"

-- 0xcba8 "RES 5,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cba8 :: Instruction1
instruction_cba8 b = stubInstruction "0xcba8"

-- 0xcba9 "RES 5,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cba9 :: Instruction1
instruction_cba9 b = stubInstruction "0xcba9"

-- 0xcbaa "RES 5,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbaa :: Instruction1
instruction_cbaa b = stubInstruction "0xcbaa"

-- 0xcbab "RES 5,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbab :: Instruction1
instruction_cbab b = stubInstruction "0xcbab"

-- 0xcbac "RES 5,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbac :: Instruction1
instruction_cbac b = stubInstruction "0xcbac"

-- 0xcbad "RES 5,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbad :: Instruction1
instruction_cbad b = stubInstruction "0xcbad"

-- 0xcbae "RES 5,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbae :: Instruction1
instruction_cbae b = stubInstruction "0xcbae"

-- 0xcbaf "RES 5,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbaf :: Instruction1
instruction_cbaf b = stubInstruction "0xcbaf"

-- 0xcbb0 "RES 6,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb0 :: Instruction1
instruction_cbb0 b = stubInstruction "0xcbb0"

-- 0xcbb1 "RES 6,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb1 :: Instruction1
instruction_cbb1 b = stubInstruction "0xcbb1"

-- 0xcbb2 "RES 6,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb2 :: Instruction1
instruction_cbb2 b = stubInstruction "0xcbb2"

-- 0xcbb3 "RES 6,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb3 :: Instruction1
instruction_cbb3 b = stubInstruction "0xcbb3"

-- 0xcbb4 "RES 6,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb4 :: Instruction1
instruction_cbb4 b = stubInstruction "0xcbb4"

-- 0xcbb5 "RES 6,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb5 :: Instruction1
instruction_cbb5 b = stubInstruction "0xcbb5"

-- 0xcbb6 "RES 6,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbb6 :: Instruction1
instruction_cbb6 b = stubInstruction "0xcbb6"

-- 0xcbb7 "RES 6,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb7 :: Instruction1
instruction_cbb7 b = stubInstruction "0xcbb7"

-- 0xcbb8 "RES 7,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb8 :: Instruction1
instruction_cbb8 b = stubInstruction "0xcbb8"

-- 0xcbb9 "RES 7,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb9 :: Instruction1
instruction_cbb9 b = stubInstruction "0xcbb9"

-- 0xcbba "RES 7,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbba :: Instruction1
instruction_cbba b = stubInstruction "0xcbba"

-- 0xcbbb "RES 7,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbb :: Instruction1
instruction_cbbb b = stubInstruction "0xcbbb"

-- 0xcbbc "RES 7,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbc :: Instruction1
instruction_cbbc b = stubInstruction "0xcbbc"

-- 0xcbbd "RES 7,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbd :: Instruction1
instruction_cbbd b = stubInstruction "0xcbbd"

-- 0xcbbe "RES 7,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbbe :: Instruction1
instruction_cbbe b = stubInstruction "0xcbbe"

-- 0xcbbf "RES 7,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbf :: Instruction1
instruction_cbbf b = stubInstruction "0xcbbf"

-- 0xcbc0 "SET 0,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc0 :: Instruction1
instruction_cbc0 b = stubInstruction "0xcbc0"

-- 0xcbc1 "SET 0,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc1 :: Instruction1
instruction_cbc1 b = stubInstruction "0xcbc1"

-- 0xcbc2 "SET 0,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc2 :: Instruction1
instruction_cbc2 b = stubInstruction "0xcbc2"

-- 0xcbc3 "SET 0,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc3 :: Instruction1
instruction_cbc3 b = stubInstruction "0xcbc3"

-- 0xcbc4 "SET 0,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc4 :: Instruction1
instruction_cbc4 b = stubInstruction "0xcbc4"

-- 0xcbc5 "SET 0,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc5 :: Instruction1
instruction_cbc5 b = stubInstruction "0xcbc5"

-- 0xcbc6 "SET 0,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbc6 :: Instruction1
instruction_cbc6 b = stubInstruction "0xcbc6"

-- 0xcbc7 "SET 0,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc7 :: Instruction1
instruction_cbc7 b = stubInstruction "0xcbc7"

-- 0xcbc8 "SET 1,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc8 :: Instruction1
instruction_cbc8 b = stubInstruction "0xcbc8"

-- 0xcbc9 "SET 1,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc9 :: Instruction1
instruction_cbc9 b = stubInstruction "0xcbc9"

-- 0xcbca "SET 1,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbca :: Instruction1
instruction_cbca b = stubInstruction "0xcbca"

-- 0xcbcb "SET 1,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcb :: Instruction1
instruction_cbcb b = stubInstruction "0xcbcb"

-- 0xcbcc "SET 1,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcc :: Instruction1
instruction_cbcc b = stubInstruction "0xcbcc"

-- 0xcbcd "SET 1,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcd :: Instruction1
instruction_cbcd b = stubInstruction "0xcbcd"

-- 0xcbce "SET 1,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbce :: Instruction1
instruction_cbce b = stubInstruction "0xcbce"

-- 0xcbcf "SET 1,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcf :: Instruction1
instruction_cbcf b = stubInstruction "0xcbcf"

-- 0xcbd0 "SET 2,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd0 :: Instruction1
instruction_cbd0 b = stubInstruction "0xcbd0"

-- 0xcbd1 "SET 2,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd1 :: Instruction1
instruction_cbd1 b = stubInstruction "0xcbd1"

-- 0xcbd2 "SET 2,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd2 :: Instruction1
instruction_cbd2 b = stubInstruction "0xcbd2"

-- 0xcbd3 "SET 2,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd3 :: Instruction1
instruction_cbd3 b = stubInstruction "0xcbd3"

-- 0xcbd4 "SET 2,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd4 :: Instruction1
instruction_cbd4 b = stubInstruction "0xcbd4"

-- 0xcbd5 "SET 2,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd5 :: Instruction1
instruction_cbd5 b = stubInstruction "0xcbd5"

-- 0xcbd6 "SET 2,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbd6 :: Instruction1
instruction_cbd6 b = stubInstruction "0xcbd6"

-- 0xcbd7 "SET 2,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd7 :: Instruction1
instruction_cbd7 b = stubInstruction "0xcbd7"

-- 0xcbd8 "SET 3,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd8 :: Instruction1
instruction_cbd8 b = stubInstruction "0xcbd8"

-- 0xcbd9 "SET 3,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd9 :: Instruction1
instruction_cbd9 b = stubInstruction "0xcbd9"

-- 0xcbda "SET 3,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbda :: Instruction1
instruction_cbda b = stubInstruction "0xcbda"

-- 0xcbdb "SET 3,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdb :: Instruction1
instruction_cbdb b = stubInstruction "0xcbdb"

-- 0xcbdc "SET 3,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdc :: Instruction1
instruction_cbdc b = stubInstruction "0xcbdc"

-- 0xcbdd "SET 3,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdd :: Instruction1
instruction_cbdd b = stubInstruction "0xcbdd"

-- 0xcbde "SET 3,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbde :: Instruction1
instruction_cbde b = stubInstruction "0xcbde"

-- 0xcbdf "SET 3,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdf :: Instruction1
instruction_cbdf b = stubInstruction "0xcbdf"

-- 0xcbe0 "SET 4,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe0 :: Instruction1
instruction_cbe0 b = stubInstruction "0xcbe0"

-- 0xcbe1 "SET 4,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe1 :: Instruction1
instruction_cbe1 b = stubInstruction "0xcbe1"

-- 0xcbe2 "SET 4,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe2 :: Instruction1
instruction_cbe2 b = stubInstruction "0xcbe2"

-- 0xcbe3 "SET 4,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe3 :: Instruction1
instruction_cbe3 b = stubInstruction "0xcbe3"

-- 0xcbe4 "SET 4,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe4 :: Instruction1
instruction_cbe4 b = stubInstruction "0xcbe4"

-- 0xcbe5 "SET 4,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe5 :: Instruction1
instruction_cbe5 b = stubInstruction "0xcbe5"

-- 0xcbe6 "SET 4,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbe6 :: Instruction1
instruction_cbe6 b = stubInstruction "0xcbe6"

-- 0xcbe7 "SET 4,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe7 :: Instruction1
instruction_cbe7 b = stubInstruction "0xcbe7"

-- 0xcbe8 "SET 5,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe8 :: Instruction1
instruction_cbe8 b = stubInstruction "0xcbe8"

-- 0xcbe9 "SET 5,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe9 :: Instruction1
instruction_cbe9 b = stubInstruction "0xcbe9"

-- 0xcbea "SET 5,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbea :: Instruction1
instruction_cbea b = stubInstruction "0xcbea"

-- 0xcbeb "SET 5,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbeb :: Instruction1
instruction_cbeb b = stubInstruction "0xcbeb"

-- 0xcbec "SET 5,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbec :: Instruction1
instruction_cbec b = stubInstruction "0xcbec"

-- 0xcbed "SET 5,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbed :: Instruction1
instruction_cbed b = stubInstruction "0xcbed"

-- 0xcbee "SET 5,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbee :: Instruction1
instruction_cbee b = stubInstruction "0xcbee"

-- 0xcbef "SET 5,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbef :: Instruction1
instruction_cbef b = stubInstruction "0xcbef"

-- 0xcbf0 "SET 6,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf0 :: Instruction1
instruction_cbf0 b = stubInstruction "0xcbf0"

-- 0xcbf1 "SET 6,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf1 :: Instruction1
instruction_cbf1 b = stubInstruction "0xcbf1"

-- 0xcbf2 "SET 6,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf2 :: Instruction1
instruction_cbf2 b = stubInstruction "0xcbf2"

-- 0xcbf3 "SET 6,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf3 :: Instruction1
instruction_cbf3 b = stubInstruction "0xcbf3"

-- 0xcbf4 "SET 6,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf4 :: Instruction1
instruction_cbf4 b = stubInstruction "0xcbf4"

-- 0xcbf5 "SET 6,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf5 :: Instruction1
instruction_cbf5 b = stubInstruction "0xcbf5"

-- 0xcbf6 "SET 6,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbf6 :: Instruction1
instruction_cbf6 b = stubInstruction "0xcbf6"

-- 0xcbf7 "SET 6,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf7 :: Instruction1
instruction_cbf7 b = stubInstruction "0xcbf7"

-- 0xcbf8 "SET 7,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf8 :: Instruction1
instruction_cbf8 b = stubInstruction "0xcbf8"

-- 0xcbf9 "SET 7,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf9 :: Instruction1
instruction_cbf9 b = stubInstruction "0xcbf9"

-- 0xcbfa "SET 7,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfa :: Instruction1
instruction_cbfa b = stubInstruction "0xcbfa"

-- 0xcbfb "SET 7,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfb :: Instruction1
instruction_cbfb b = stubInstruction "0xcbfb"

-- 0xcbfc "SET 7,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfc :: Instruction1
instruction_cbfc b = stubInstruction "0xcbfc"

-- 0xcbfd "SET 7,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfd :: Instruction1
instruction_cbfd b = stubInstruction "0xcbfd"

-- 0xcbfe "SET 7,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbfe :: Instruction1
instruction_cbfe b = stubInstruction "0xcbfe"

-- 0xcbff "SET 7,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbff :: Instruction1
instruction_cbff b = stubInstruction "0xcbff"
