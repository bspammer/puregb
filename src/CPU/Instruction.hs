{-# LANGUAGE TemplateHaskell #-}
module CPU.Instruction where

import Data.Map (fromList, Map)
import Data.Word (Word8)
import Lens.Micro.Platform

import CPU ( CPU )

type Instruction = CPU -> [Word8] -> CPU

stubInstruction :: String -> Instruction
stubInstruction text = error $ "Instruction with no implementation: " ++ text

-- 0x00 NOP, 1 byte operand, 4 cycles -,-,-,-
instruction_00 :: Instruction
instruction_00 = stubInstruction "0x00"

-- 0x01 "LD BC,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_01 :: Instruction
instruction_01 = stubInstruction "0x01"

-- 0x02 "LD (BC),A", 1 byte operand, 8 cycles -,-,-,-
instruction_02 :: Instruction
instruction_02 = stubInstruction "0x02"

-- 0x03 INC BC, 1 byte operand, 8 cycles -,-,-,-
instruction_03 :: Instruction
instruction_03 = stubInstruction "0x03"

-- 0x04 INC B, 1 byte operand, 4 cycles Z,0,H,-
instruction_04 :: Instruction
instruction_04 = stubInstruction "0x04"

-- 0x05 DEC B, 1 byte operand, 4 cycles Z,1,H,-
instruction_05 :: Instruction
instruction_05 = stubInstruction "0x05"

-- 0x06 "LD B,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_06 :: Instruction
instruction_06 = stubInstruction "0x06"

-- 0x07 RLCA, 1 byte operand, 4 cycles 0,0,0,C
instruction_07 :: Instruction
instruction_07 = stubInstruction "0x07"

-- 0x08 "LD (a16),SP", 3 byte operand, 20 cycles -,-,-,-
instruction_08 :: Instruction
instruction_08 = stubInstruction "0x08"

-- 0x09 "ADD HL,BC", 1 byte operand, 8 cycles -,0,H,C
instruction_09 :: Instruction
instruction_09 = stubInstruction "0x09"

-- 0x0a "LD A,(BC)", 1 byte operand, 8 cycles -,-,-,-
instruction_0a :: Instruction
instruction_0a = stubInstruction "0x0a"

-- 0x0b DEC BC, 1 byte operand, 8 cycles -,-,-,-
instruction_0b :: Instruction
instruction_0b = stubInstruction "0x0b"

-- 0x0c INC C, 1 byte operand, 4 cycles Z,0,H,-
instruction_0c :: Instruction
instruction_0c = stubInstruction "0x0c"

-- 0x0d DEC C, 1 byte operand, 4 cycles Z,1,H,-
instruction_0d :: Instruction
instruction_0d = stubInstruction "0x0d"

-- 0x0e "LD C,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_0e :: Instruction
instruction_0e = stubInstruction "0x0e"

-- 0x0f RRCA, 1 byte operand, 4 cycles 0,0,0,C
instruction_0f :: Instruction
instruction_0f = stubInstruction "0x0f"

-- 0x10 STOP 0, 2 byte operand, 4 cycles -,-,-,-
instruction_10 :: Instruction
instruction_10 = stubInstruction "0x10"

-- 0x11 "LD DE,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_11 :: Instruction
instruction_11 = stubInstruction "0x11"

-- 0x12 "LD (DE),A", 1 byte operand, 8 cycles -,-,-,-
instruction_12 :: Instruction
instruction_12 = stubInstruction "0x12"

-- 0x13 INC DE, 1 byte operand, 8 cycles -,-,-,-
instruction_13 :: Instruction
instruction_13 = stubInstruction "0x13"

-- 0x14 INC D, 1 byte operand, 4 cycles Z,0,H,-
instruction_14 :: Instruction
instruction_14 = stubInstruction "0x14"

-- 0x15 DEC D, 1 byte operand, 4 cycles Z,1,H,-
instruction_15 :: Instruction
instruction_15 = stubInstruction "0x15"

-- 0x16 "LD D,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_16 :: Instruction
instruction_16 = stubInstruction "0x16"

-- 0x17 RLA, 1 byte operand, 4 cycles 0,0,0,C
instruction_17 :: Instruction
instruction_17 = stubInstruction "0x17"

-- 0x18 JR r8, 2 byte operand, 12 cycles -,-,-,-
instruction_18 :: Instruction
instruction_18 = stubInstruction "0x18"

-- 0x19 "ADD HL,DE", 1 byte operand, 8 cycles -,0,H,C
instruction_19 :: Instruction
instruction_19 = stubInstruction "0x19"

-- 0x1a "LD A,(DE)", 1 byte operand, 8 cycles -,-,-,-
instruction_1a :: Instruction
instruction_1a = stubInstruction "0x1a"

-- 0x1b DEC DE, 1 byte operand, 8 cycles -,-,-,-
instruction_1b :: Instruction
instruction_1b = stubInstruction "0x1b"

-- 0x1c INC E, 1 byte operand, 4 cycles Z,0,H,-
instruction_1c :: Instruction
instruction_1c = stubInstruction "0x1c"

-- 0x1d DEC E, 1 byte operand, 4 cycles Z,1,H,-
instruction_1d :: Instruction
instruction_1d = stubInstruction "0x1d"

-- 0x1e "LD E,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_1e :: Instruction
instruction_1e = stubInstruction "0x1e"

-- 0x1f RRA, 1 byte operand, 4 cycles 0,0,0,C
instruction_1f :: Instruction
instruction_1f = stubInstruction "0x1f"

-- 0x20 "JR NZ,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_20 :: Instruction
instruction_20 = stubInstruction "0x20"

-- 0x21 "LD HL,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_21 :: Instruction
instruction_21 = stubInstruction "0x21"

-- 0x22 "LD (HL+),A", 1 byte operand, 8 cycles -,-,-,-
instruction_22 :: Instruction
instruction_22 = stubInstruction "0x22"

-- 0x23 INC HL, 1 byte operand, 8 cycles -,-,-,-
instruction_23 :: Instruction
instruction_23 = stubInstruction "0x23"

-- 0x24 INC H, 1 byte operand, 4 cycles Z,0,H,-
instruction_24 :: Instruction
instruction_24 = stubInstruction "0x24"

-- 0x25 DEC H, 1 byte operand, 4 cycles Z,1,H,-
instruction_25 :: Instruction
instruction_25 = stubInstruction "0x25"

-- 0x26 "LD H,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_26 :: Instruction
instruction_26 = stubInstruction "0x26"

-- 0x27 DAA, 1 byte operand, 4 cycles Z,-,0,C
instruction_27 :: Instruction
instruction_27 = stubInstruction "0x27"

-- 0x28 "JR Z,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_28 :: Instruction
instruction_28 = stubInstruction "0x28"

-- 0x29 "ADD HL,HL", 1 byte operand, 8 cycles -,0,H,C
instruction_29 :: Instruction
instruction_29 = stubInstruction "0x29"

-- 0x2a "LD A,(HL+)", 1 byte operand, 8 cycles -,-,-,-
instruction_2a :: Instruction
instruction_2a = stubInstruction "0x2a"

-- 0x2b DEC HL, 1 byte operand, 8 cycles -,-,-,-
instruction_2b :: Instruction
instruction_2b = stubInstruction "0x2b"

-- 0x2c INC L, 1 byte operand, 4 cycles Z,0,H,-
instruction_2c :: Instruction
instruction_2c = stubInstruction "0x2c"

-- 0x2d DEC L, 1 byte operand, 4 cycles Z,1,H,-
instruction_2d :: Instruction
instruction_2d = stubInstruction "0x2d"

-- 0x2e "LD L,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_2e :: Instruction
instruction_2e = stubInstruction "0x2e"

-- 0x2f CPL, 1 byte operand, 4 cycles -,1,1,-
instruction_2f :: Instruction
instruction_2f = stubInstruction "0x2f"

-- 0x30 "JR NC,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_30 :: Instruction
instruction_30 = stubInstruction "0x30"

-- 0x31 "LD SP,d16", 3 byte operand, 12 cycles -,-,-,-
instruction_31 :: Instruction
instruction_31 = stubInstruction "0x31"

-- 0x32 "LD (HL-),A", 1 byte operand, 8 cycles -,-,-,-
instruction_32 :: Instruction
instruction_32 = stubInstruction "0x32"

-- 0x33 INC SP, 1 byte operand, 8 cycles -,-,-,-
instruction_33 :: Instruction
instruction_33 = stubInstruction "0x33"

-- 0x34 INC (HL), 1 byte operand, 12 cycles Z,0,H,-
instruction_34 :: Instruction
instruction_34 = stubInstruction "0x34"

-- 0x35 DEC (HL), 1 byte operand, 12 cycles Z,1,H,-
instruction_35 :: Instruction
instruction_35 = stubInstruction "0x35"

-- 0x36 "LD (HL),d8", 2 byte operand, 12 cycles -,-,-,-
instruction_36 :: Instruction
instruction_36 = stubInstruction "0x36"

-- 0x37 SCF, 1 byte operand, 4 cycles -,0,0,1
instruction_37 :: Instruction
instruction_37 = stubInstruction "0x37"

-- 0x38 "JR C,r8", 2 byte operand, 12/8 cycles -,-,-,-
instruction_38 :: Instruction
instruction_38 = stubInstruction "0x38"

-- 0x39 "ADD HL,SP", 1 byte operand, 8 cycles -,0,H,C
instruction_39 :: Instruction
instruction_39 = stubInstruction "0x39"

-- 0x3a "LD A,(HL-)", 1 byte operand, 8 cycles -,-,-,-
instruction_3a :: Instruction
instruction_3a = stubInstruction "0x3a"

-- 0x3b DEC SP, 1 byte operand, 8 cycles -,-,-,-
instruction_3b :: Instruction
instruction_3b = stubInstruction "0x3b"

-- 0x3c INC A, 1 byte operand, 4 cycles Z,0,H,-
instruction_3c :: Instruction
instruction_3c = stubInstruction "0x3c"

-- 0x3d DEC A, 1 byte operand, 4 cycles Z,1,H,-
instruction_3d :: Instruction
instruction_3d = stubInstruction "0x3d"

-- 0x3e "LD A,d8", 2 byte operand, 8 cycles -,-,-,-
instruction_3e :: Instruction
instruction_3e = stubInstruction "0x3e"

-- 0x3f CCF, 1 byte operand, 4 cycles -,0,0,C
instruction_3f :: Instruction
instruction_3f = stubInstruction "0x3f"

-- 0x40 "LD B,B", 1 byte operand, 4 cycles -,-,-,-
instruction_40 :: Instruction
instruction_40 = stubInstruction "0x40"

-- 0x41 "LD B,C", 1 byte operand, 4 cycles -,-,-,-
instruction_41 :: Instruction
instruction_41 = stubInstruction "0x41"

-- 0x42 "LD B,D", 1 byte operand, 4 cycles -,-,-,-
instruction_42 :: Instruction
instruction_42 = stubInstruction "0x42"

-- 0x43 "LD B,E", 1 byte operand, 4 cycles -,-,-,-
instruction_43 :: Instruction
instruction_43 = stubInstruction "0x43"

-- 0x44 "LD B,H", 1 byte operand, 4 cycles -,-,-,-
instruction_44 :: Instruction
instruction_44 = stubInstruction "0x44"

-- 0x45 "LD B,L", 1 byte operand, 4 cycles -,-,-,-
instruction_45 :: Instruction
instruction_45 = stubInstruction "0x45"

-- 0x46 "LD B,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_46 :: Instruction
instruction_46 = stubInstruction "0x46"

-- 0x47 "LD B,A", 1 byte operand, 4 cycles -,-,-,-
instruction_47 :: Instruction
instruction_47 = stubInstruction "0x47"

-- 0x48 "LD C,B", 1 byte operand, 4 cycles -,-,-,-
instruction_48 :: Instruction
instruction_48 = stubInstruction "0x48"

-- 0x49 "LD C,C", 1 byte operand, 4 cycles -,-,-,-
instruction_49 :: Instruction
instruction_49 = stubInstruction "0x49"

-- 0x4a "LD C,D", 1 byte operand, 4 cycles -,-,-,-
instruction_4a :: Instruction
instruction_4a = stubInstruction "0x4a"

-- 0x4b "LD C,E", 1 byte operand, 4 cycles -,-,-,-
instruction_4b :: Instruction
instruction_4b = stubInstruction "0x4b"

-- 0x4c "LD C,H", 1 byte operand, 4 cycles -,-,-,-
instruction_4c :: Instruction
instruction_4c = stubInstruction "0x4c"

-- 0x4d "LD C,L", 1 byte operand, 4 cycles -,-,-,-
instruction_4d :: Instruction
instruction_4d = stubInstruction "0x4d"

-- 0x4e "LD C,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_4e :: Instruction
instruction_4e = stubInstruction "0x4e"

-- 0x4f "LD C,A", 1 byte operand, 4 cycles -,-,-,-
instruction_4f :: Instruction
instruction_4f = stubInstruction "0x4f"

-- 0x50 "LD D,B", 1 byte operand, 4 cycles -,-,-,-
instruction_50 :: Instruction
instruction_50 = stubInstruction "0x50"

-- 0x51 "LD D,C", 1 byte operand, 4 cycles -,-,-,-
instruction_51 :: Instruction
instruction_51 = stubInstruction "0x51"

-- 0x52 "LD D,D", 1 byte operand, 4 cycles -,-,-,-
instruction_52 :: Instruction
instruction_52 = stubInstruction "0x52"

-- 0x53 "LD D,E", 1 byte operand, 4 cycles -,-,-,-
instruction_53 :: Instruction
instruction_53 = stubInstruction "0x53"

-- 0x54 "LD D,H", 1 byte operand, 4 cycles -,-,-,-
instruction_54 :: Instruction
instruction_54 = stubInstruction "0x54"

-- 0x55 "LD D,L", 1 byte operand, 4 cycles -,-,-,-
instruction_55 :: Instruction
instruction_55 = stubInstruction "0x55"

-- 0x56 "LD D,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_56 :: Instruction
instruction_56 = stubInstruction "0x56"

-- 0x57 "LD D,A", 1 byte operand, 4 cycles -,-,-,-
instruction_57 :: Instruction
instruction_57 = stubInstruction "0x57"

-- 0x58 "LD E,B", 1 byte operand, 4 cycles -,-,-,-
instruction_58 :: Instruction
instruction_58 = stubInstruction "0x58"

-- 0x59 "LD E,C", 1 byte operand, 4 cycles -,-,-,-
instruction_59 :: Instruction
instruction_59 = stubInstruction "0x59"

-- 0x5a "LD E,D", 1 byte operand, 4 cycles -,-,-,-
instruction_5a :: Instruction
instruction_5a = stubInstruction "0x5a"

-- 0x5b "LD E,E", 1 byte operand, 4 cycles -,-,-,-
instruction_5b :: Instruction
instruction_5b = stubInstruction "0x5b"

-- 0x5c "LD E,H", 1 byte operand, 4 cycles -,-,-,-
instruction_5c :: Instruction
instruction_5c = stubInstruction "0x5c"

-- 0x5d "LD E,L", 1 byte operand, 4 cycles -,-,-,-
instruction_5d :: Instruction
instruction_5d = stubInstruction "0x5d"

-- 0x5e "LD E,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_5e :: Instruction
instruction_5e = stubInstruction "0x5e"

-- 0x5f "LD E,A", 1 byte operand, 4 cycles -,-,-,-
instruction_5f :: Instruction
instruction_5f = stubInstruction "0x5f"

-- 0x60 "LD H,B", 1 byte operand, 4 cycles -,-,-,-
instruction_60 :: Instruction
instruction_60 = stubInstruction "0x60"

-- 0x61 "LD H,C", 1 byte operand, 4 cycles -,-,-,-
instruction_61 :: Instruction
instruction_61 = stubInstruction "0x61"

-- 0x62 "LD H,D", 1 byte operand, 4 cycles -,-,-,-
instruction_62 :: Instruction
instruction_62 = stubInstruction "0x62"

-- 0x63 "LD H,E", 1 byte operand, 4 cycles -,-,-,-
instruction_63 :: Instruction
instruction_63 = stubInstruction "0x63"

-- 0x64 "LD H,H", 1 byte operand, 4 cycles -,-,-,-
instruction_64 :: Instruction
instruction_64 = stubInstruction "0x64"

-- 0x65 "LD H,L", 1 byte operand, 4 cycles -,-,-,-
instruction_65 :: Instruction
instruction_65 = stubInstruction "0x65"

-- 0x66 "LD H,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_66 :: Instruction
instruction_66 = stubInstruction "0x66"

-- 0x67 "LD H,A", 1 byte operand, 4 cycles -,-,-,-
instruction_67 :: Instruction
instruction_67 = stubInstruction "0x67"

-- 0x68 "LD L,B", 1 byte operand, 4 cycles -,-,-,-
instruction_68 :: Instruction
instruction_68 = stubInstruction "0x68"

-- 0x69 "LD L,C", 1 byte operand, 4 cycles -,-,-,-
instruction_69 :: Instruction
instruction_69 = stubInstruction "0x69"

-- 0x6a "LD L,D", 1 byte operand, 4 cycles -,-,-,-
instruction_6a :: Instruction
instruction_6a = stubInstruction "0x6a"

-- 0x6b "LD L,E", 1 byte operand, 4 cycles -,-,-,-
instruction_6b :: Instruction
instruction_6b = stubInstruction "0x6b"

-- 0x6c "LD L,H", 1 byte operand, 4 cycles -,-,-,-
instruction_6c :: Instruction
instruction_6c = stubInstruction "0x6c"

-- 0x6d "LD L,L", 1 byte operand, 4 cycles -,-,-,-
instruction_6d :: Instruction
instruction_6d = stubInstruction "0x6d"

-- 0x6e "LD L,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_6e :: Instruction
instruction_6e = stubInstruction "0x6e"

-- 0x6f "LD L,A", 1 byte operand, 4 cycles -,-,-,-
instruction_6f :: Instruction
instruction_6f = stubInstruction "0x6f"

-- 0x70 "LD (HL),B", 1 byte operand, 8 cycles -,-,-,-
instruction_70 :: Instruction
instruction_70 = stubInstruction "0x70"

-- 0x71 "LD (HL),C", 1 byte operand, 8 cycles -,-,-,-
instruction_71 :: Instruction
instruction_71 = stubInstruction "0x71"

-- 0x72 "LD (HL),D", 1 byte operand, 8 cycles -,-,-,-
instruction_72 :: Instruction
instruction_72 = stubInstruction "0x72"

-- 0x73 "LD (HL),E", 1 byte operand, 8 cycles -,-,-,-
instruction_73 :: Instruction
instruction_73 = stubInstruction "0x73"

-- 0x74 "LD (HL),H", 1 byte operand, 8 cycles -,-,-,-
instruction_74 :: Instruction
instruction_74 = stubInstruction "0x74"

-- 0x75 "LD (HL),L", 1 byte operand, 8 cycles -,-,-,-
instruction_75 :: Instruction
instruction_75 = stubInstruction "0x75"

-- 0x76 HALT, 1 byte operand, 4 cycles -,-,-,-
instruction_76 :: Instruction
instruction_76 = stubInstruction "0x76"

-- 0x77 "LD (HL),A", 1 byte operand, 8 cycles -,-,-,-
instruction_77 :: Instruction
instruction_77 = stubInstruction "0x77"

-- 0x78 "LD A,B", 1 byte operand, 4 cycles -,-,-,-
instruction_78 :: Instruction
instruction_78 = stubInstruction "0x78"

-- 0x79 "LD A,C", 1 byte operand, 4 cycles -,-,-,-
instruction_79 :: Instruction
instruction_79 = stubInstruction "0x79"

-- 0x7a "LD A,D", 1 byte operand, 4 cycles -,-,-,-
instruction_7a :: Instruction
instruction_7a = stubInstruction "0x7a"

-- 0x7b "LD A,E", 1 byte operand, 4 cycles -,-,-,-
instruction_7b :: Instruction
instruction_7b = stubInstruction "0x7b"

-- 0x7c "LD A,H", 1 byte operand, 4 cycles -,-,-,-
instruction_7c :: Instruction
instruction_7c = stubInstruction "0x7c"

-- 0x7d "LD A,L", 1 byte operand, 4 cycles -,-,-,-
instruction_7d :: Instruction
instruction_7d = stubInstruction "0x7d"

-- 0x7e "LD A,(HL)", 1 byte operand, 8 cycles -,-,-,-
instruction_7e :: Instruction
instruction_7e = stubInstruction "0x7e"

-- 0x7f "LD A,A", 1 byte operand, 4 cycles -,-,-,-
instruction_7f :: Instruction
instruction_7f = stubInstruction "0x7f"

-- 0x80 "ADD A,B", 1 byte operand, 4 cycles Z,0,H,C
instruction_80 :: Instruction
instruction_80 = stubInstruction "0x80"

-- 0x81 "ADD A,C", 1 byte operand, 4 cycles Z,0,H,C
instruction_81 :: Instruction
instruction_81 = stubInstruction "0x81"

-- 0x82 "ADD A,D", 1 byte operand, 4 cycles Z,0,H,C
instruction_82 :: Instruction
instruction_82 = stubInstruction "0x82"

-- 0x83 "ADD A,E", 1 byte operand, 4 cycles Z,0,H,C
instruction_83 :: Instruction
instruction_83 = stubInstruction "0x83"

-- 0x84 "ADD A,H", 1 byte operand, 4 cycles Z,0,H,C
instruction_84 :: Instruction
instruction_84 = stubInstruction "0x84"

-- 0x85 "ADD A,L", 1 byte operand, 4 cycles Z,0,H,C
instruction_85 :: Instruction
instruction_85 = stubInstruction "0x85"

-- 0x86 "ADD A,(HL)", 1 byte operand, 8 cycles Z,0,H,C
instruction_86 :: Instruction
instruction_86 = stubInstruction "0x86"

-- 0x87 "ADD A,A", 1 byte operand, 4 cycles Z,0,H,C
instruction_87 :: Instruction
instruction_87 = stubInstruction "0x87"

-- 0x88 "ADC A,B", 1 byte operand, 4 cycles Z,0,H,C
instruction_88 :: Instruction
instruction_88 = stubInstruction "0x88"

-- 0x89 "ADC A,C", 1 byte operand, 4 cycles Z,0,H,C
instruction_89 :: Instruction
instruction_89 = stubInstruction "0x89"

-- 0x8a "ADC A,D", 1 byte operand, 4 cycles Z,0,H,C
instruction_8a :: Instruction
instruction_8a = stubInstruction "0x8a"

-- 0x8b "ADC A,E", 1 byte operand, 4 cycles Z,0,H,C
instruction_8b :: Instruction
instruction_8b = stubInstruction "0x8b"

-- 0x8c "ADC A,H", 1 byte operand, 4 cycles Z,0,H,C
instruction_8c :: Instruction
instruction_8c = stubInstruction "0x8c"

-- 0x8d "ADC A,L", 1 byte operand, 4 cycles Z,0,H,C
instruction_8d :: Instruction
instruction_8d = stubInstruction "0x8d"

-- 0x8e "ADC A,(HL)", 1 byte operand, 8 cycles Z,0,H,C
instruction_8e :: Instruction
instruction_8e = stubInstruction "0x8e"

-- 0x8f "ADC A,A", 1 byte operand, 4 cycles Z,0,H,C
instruction_8f :: Instruction
instruction_8f = stubInstruction "0x8f"

-- 0x90 SUB B, 1 byte operand, 4 cycles Z,1,H,C
instruction_90 :: Instruction
instruction_90 = stubInstruction "0x90"

-- 0x91 SUB C, 1 byte operand, 4 cycles Z,1,H,C
instruction_91 :: Instruction
instruction_91 = stubInstruction "0x91"

-- 0x92 SUB D, 1 byte operand, 4 cycles Z,1,H,C
instruction_92 :: Instruction
instruction_92 = stubInstruction "0x92"

-- 0x93 SUB E, 1 byte operand, 4 cycles Z,1,H,C
instruction_93 :: Instruction
instruction_93 = stubInstruction "0x93"

-- 0x94 SUB H, 1 byte operand, 4 cycles Z,1,H,C
instruction_94 :: Instruction
instruction_94 = stubInstruction "0x94"

-- 0x95 SUB L, 1 byte operand, 4 cycles Z,1,H,C
instruction_95 :: Instruction
instruction_95 = stubInstruction "0x95"

-- 0x96 SUB (HL), 1 byte operand, 8 cycles Z,1,H,C
instruction_96 :: Instruction
instruction_96 = stubInstruction "0x96"

-- 0x97 SUB A, 1 byte operand, 4 cycles Z,1,H,C
instruction_97 :: Instruction
instruction_97 = stubInstruction "0x97"

-- 0x98 "SBC A,B", 1 byte operand, 4 cycles Z,1,H,C
instruction_98 :: Instruction
instruction_98 = stubInstruction "0x98"

-- 0x99 "SBC A,C", 1 byte operand, 4 cycles Z,1,H,C
instruction_99 :: Instruction
instruction_99 = stubInstruction "0x99"

-- 0x9a "SBC A,D", 1 byte operand, 4 cycles Z,1,H,C
instruction_9a :: Instruction
instruction_9a = stubInstruction "0x9a"

-- 0x9b "SBC A,E", 1 byte operand, 4 cycles Z,1,H,C
instruction_9b :: Instruction
instruction_9b = stubInstruction "0x9b"

-- 0x9c "SBC A,H", 1 byte operand, 4 cycles Z,1,H,C
instruction_9c :: Instruction
instruction_9c = stubInstruction "0x9c"

-- 0x9d "SBC A,L", 1 byte operand, 4 cycles Z,1,H,C
instruction_9d :: Instruction
instruction_9d = stubInstruction "0x9d"

-- 0x9e "SBC A,(HL)", 1 byte operand, 8 cycles Z,1,H,C
instruction_9e :: Instruction
instruction_9e = stubInstruction "0x9e"

-- 0x9f "SBC A,A", 1 byte operand, 4 cycles Z,1,H,C
instruction_9f :: Instruction
instruction_9f = stubInstruction "0x9f"

-- 0xa0 AND B, 1 byte operand, 4 cycles Z,0,1,0
instruction_a0 :: Instruction
instruction_a0 = stubInstruction "0xa0"

-- 0xa1 AND C, 1 byte operand, 4 cycles Z,0,1,0
instruction_a1 :: Instruction
instruction_a1 = stubInstruction "0xa1"

-- 0xa2 AND D, 1 byte operand, 4 cycles Z,0,1,0
instruction_a2 :: Instruction
instruction_a2 = stubInstruction "0xa2"

-- 0xa3 AND E, 1 byte operand, 4 cycles Z,0,1,0
instruction_a3 :: Instruction
instruction_a3 = stubInstruction "0xa3"

-- 0xa4 AND H, 1 byte operand, 4 cycles Z,0,1,0
instruction_a4 :: Instruction
instruction_a4 = stubInstruction "0xa4"

-- 0xa5 AND L, 1 byte operand, 4 cycles Z,0,1,0
instruction_a5 :: Instruction
instruction_a5 = stubInstruction "0xa5"

-- 0xa6 AND (HL), 1 byte operand, 8 cycles Z,0,1,0
instruction_a6 :: Instruction
instruction_a6 = stubInstruction "0xa6"

-- 0xa7 AND A, 1 byte operand, 4 cycles Z,0,1,0
instruction_a7 :: Instruction
instruction_a7 = stubInstruction "0xa7"

-- 0xa8 XOR B, 1 byte operand, 4 cycles Z,0,0,0
instruction_a8 :: Instruction
instruction_a8 = stubInstruction "0xa8"

-- 0xa9 XOR C, 1 byte operand, 4 cycles Z,0,0,0
instruction_a9 :: Instruction
instruction_a9 = stubInstruction "0xa9"

-- 0xaa XOR D, 1 byte operand, 4 cycles Z,0,0,0
instruction_aa :: Instruction
instruction_aa = stubInstruction "0xaa"

-- 0xab XOR E, 1 byte operand, 4 cycles Z,0,0,0
instruction_ab :: Instruction
instruction_ab = stubInstruction "0xab"

-- 0xac XOR H, 1 byte operand, 4 cycles Z,0,0,0
instruction_ac :: Instruction
instruction_ac = stubInstruction "0xac"

-- 0xad XOR L, 1 byte operand, 4 cycles Z,0,0,0
instruction_ad :: Instruction
instruction_ad = stubInstruction "0xad"

-- 0xae XOR (HL), 1 byte operand, 8 cycles Z,0,0,0
instruction_ae :: Instruction
instruction_ae = stubInstruction "0xae"

-- 0xaf XOR A, 1 byte operand, 4 cycles Z,0,0,0
instruction_af :: Instruction
instruction_af = stubInstruction "0xaf"

-- 0xb0 OR B, 1 byte operand, 4 cycles Z,0,0,0
instruction_b0 :: Instruction
instruction_b0 = stubInstruction "0xb0"

-- 0xb1 OR C, 1 byte operand, 4 cycles Z,0,0,0
instruction_b1 :: Instruction
instruction_b1 = stubInstruction "0xb1"

-- 0xb2 OR D, 1 byte operand, 4 cycles Z,0,0,0
instruction_b2 :: Instruction
instruction_b2 = stubInstruction "0xb2"

-- 0xb3 OR E, 1 byte operand, 4 cycles Z,0,0,0
instruction_b3 :: Instruction
instruction_b3 = stubInstruction "0xb3"

-- 0xb4 OR H, 1 byte operand, 4 cycles Z,0,0,0
instruction_b4 :: Instruction
instruction_b4 = stubInstruction "0xb4"

-- 0xb5 OR L, 1 byte operand, 4 cycles Z,0,0,0
instruction_b5 :: Instruction
instruction_b5 = stubInstruction "0xb5"

-- 0xb6 OR (HL), 1 byte operand, 8 cycles Z,0,0,0
instruction_b6 :: Instruction
instruction_b6 = stubInstruction "0xb6"

-- 0xb7 OR A, 1 byte operand, 4 cycles Z,0,0,0
instruction_b7 :: Instruction
instruction_b7 = stubInstruction "0xb7"

-- 0xb8 CP B, 1 byte operand, 4 cycles Z,1,H,C
instruction_b8 :: Instruction
instruction_b8 = stubInstruction "0xb8"

-- 0xb9 CP C, 1 byte operand, 4 cycles Z,1,H,C
instruction_b9 :: Instruction
instruction_b9 = stubInstruction "0xb9"

-- 0xba CP D, 1 byte operand, 4 cycles Z,1,H,C
instruction_ba :: Instruction
instruction_ba = stubInstruction "0xba"

-- 0xbb CP E, 1 byte operand, 4 cycles Z,1,H,C
instruction_bb :: Instruction
instruction_bb = stubInstruction "0xbb"

-- 0xbc CP H, 1 byte operand, 4 cycles Z,1,H,C
instruction_bc :: Instruction
instruction_bc = stubInstruction "0xbc"

-- 0xbd CP L, 1 byte operand, 4 cycles Z,1,H,C
instruction_bd :: Instruction
instruction_bd = stubInstruction "0xbd"

-- 0xbe CP (HL), 1 byte operand, 8 cycles Z,1,H,C
instruction_be :: Instruction
instruction_be = stubInstruction "0xbe"

-- 0xbf CP A, 1 byte operand, 4 cycles Z,1,H,C
instruction_bf :: Instruction
instruction_bf = stubInstruction "0xbf"

-- 0xc0 RET NZ, 1 byte operand, 20/8 cycles -,-,-,-
instruction_c0 :: Instruction
instruction_c0 = stubInstruction "0xc0"

-- 0xc1 POP BC, 1 byte operand, 12 cycles -,-,-,-
instruction_c1 :: Instruction
instruction_c1 = stubInstruction "0xc1"

-- 0xc2 "JP NZ,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_c2 :: Instruction
instruction_c2 = stubInstruction "0xc2"

-- 0xc3 JP a16, 3 byte operand, 16 cycles -,-,-,-
instruction_c3 :: Instruction
instruction_c3 = stubInstruction "0xc3"

-- 0xc4 "CALL NZ,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_c4 :: Instruction
instruction_c4 = stubInstruction "0xc4"

-- 0xc5 PUSH BC, 1 byte operand, 16 cycles -,-,-,-
instruction_c5 :: Instruction
instruction_c5 = stubInstruction "0xc5"

-- 0xc6 "ADD A,d8", 2 byte operand, 8 cycles Z,0,H,C
instruction_c6 :: Instruction
instruction_c6 = stubInstruction "0xc6"

-- 0xc7 RST 00H, 1 byte operand, 16 cycles -,-,-,-
instruction_c7 :: Instruction
instruction_c7 = stubInstruction "0xc7"

-- 0xc8 RET Z, 1 byte operand, 20/8 cycles -,-,-,-
instruction_c8 :: Instruction
instruction_c8 = stubInstruction "0xc8"

-- 0xc9 RET, 1 byte operand, 16 cycles -,-,-,-
instruction_c9 :: Instruction
instruction_c9 = stubInstruction "0xc9"

-- 0xca "JP Z,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_ca :: Instruction
instruction_ca = stubInstruction "0xca"

-- 0xcb PREFIX CB, 1 byte operand, 4 cycles -,-,-,-
instruction_cb :: Instruction
instruction_cb = stubInstruction "0xcb"

-- 0xcc "CALL Z,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_cc :: Instruction
instruction_cc = stubInstruction "0xcc"

-- 0xcd CALL a16, 3 byte operand, 24 cycles -,-,-,-
instruction_cd :: Instruction
instruction_cd = stubInstruction "0xcd"

-- 0xce "ADC A,d8", 2 byte operand, 8 cycles Z,0,H,C
instruction_ce :: Instruction
instruction_ce = stubInstruction "0xce"

-- 0xcf RST 08H, 1 byte operand, 16 cycles -,-,-,-
instruction_cf :: Instruction
instruction_cf = stubInstruction "0xcf"

-- 0xd0 RET NC, 1 byte operand, 20/8 cycles -,-,-,-
instruction_d0 :: Instruction
instruction_d0 = stubInstruction "0xd0"

-- 0xd1 POP DE, 1 byte operand, 12 cycles -,-,-,-
instruction_d1 :: Instruction
instruction_d1 = stubInstruction "0xd1"

-- 0xd2 "JP NC,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_d2 :: Instruction
instruction_d2 = stubInstruction "0xd2"

-- 0xd3 ,,,,,,
instruction_d3 :: Instruction
instruction_d3 = stubInstruction "0xd3"

-- 0xd4 "CALL NC,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_d4 :: Instruction
instruction_d4 = stubInstruction "0xd4"

-- 0xd5 PUSH DE, 1 byte operand, 16 cycles -,-,-,-
instruction_d5 :: Instruction
instruction_d5 = stubInstruction "0xd5"

-- 0xd6 SUB d8, 2 byte operand, 8 cycles Z,1,H,C
instruction_d6 :: Instruction
instruction_d6 = stubInstruction "0xd6"

-- 0xd7 RST 10H, 1 byte operand, 16 cycles -,-,-,-
instruction_d7 :: Instruction
instruction_d7 = stubInstruction "0xd7"

-- 0xd8 RET C, 1 byte operand, 20/8 cycles -,-,-,-
instruction_d8 :: Instruction
instruction_d8 = stubInstruction "0xd8"

-- 0xd9 RETI, 1 byte operand, 16 cycles -,-,-,-
instruction_d9 :: Instruction
instruction_d9 = stubInstruction "0xd9"

-- 0xda "JP C,a16", 3 byte operand, 16/12 cycles -,-,-,-
instruction_da :: Instruction
instruction_da = stubInstruction "0xda"

-- 0xdb ,,,,,,
instruction_db :: Instruction
instruction_db = stubInstruction "0xdb"

-- 0xdc "CALL C,a16", 3 byte operand, 24/12 cycles -,-,-,-
instruction_dc :: Instruction
instruction_dc = stubInstruction "0xdc"

-- 0xdd,,,,,,,
instruction_dd :: Instruction
instruction_dd = stubInstruction "0xdd"

-- 0xde "SBC A,d8", 2 byte operand, 8 cycles Z,1,H,C
instruction_de :: Instruction
instruction_de = stubInstruction "0xde"

-- 0xdf RST 18H, 1 byte operand, 16 cycles -,-,-,-
instruction_df :: Instruction
instruction_df = stubInstruction "0xdf"

-- 0xe0 "LDH (a8),A", 2 byte operand, 12 cycles -,-,-,-
instruction_e0 :: Instruction
instruction_e0 = stubInstruction "0xe0"

-- 0xe1 POP HL, 1 byte operand, 12 cycles -,-,-,-
instruction_e1 :: Instruction
instruction_e1 = stubInstruction "0xe1"

-- 0xe2 "LD (C),A", 2 byte operand, 8 cycles -,-,-,-
instruction_e2 :: Instruction
instruction_e2 = stubInstruction "0xe2"

-- 0xe3,,,,,,,
instruction_e3 :: Instruction
instruction_e3 = stubInstruction "0xe3"

-- 0xe4,,,,,,,
instruction_e4 :: Instruction
instruction_e4 = stubInstruction "0xe4"

-- 0xe5 PUSH HL, 1 byte operand, 16 cycles -,-,-,-
instruction_e5 :: Instruction
instruction_e5 = stubInstruction "0xe5"

-- 0xe6 AND d8, 2 byte operand, 8 cycles Z,0,1,0
instruction_e6 :: Instruction
instruction_e6 = stubInstruction "0xe6"

-- 0xe7 RST 20H, 1 byte operand, 16 cycles -,-,-,-
instruction_e7 :: Instruction
instruction_e7 = stubInstruction "0xe7"

-- 0xe8 "ADD SP,r8", 2 byte operand, 16 cycles 0,0,H,C
instruction_e8 :: Instruction
instruction_e8 = stubInstruction "0xe8"

-- 0xe9 JP (HL), 1 byte operand, 4 cycles -,-,-,-
instruction_e9 :: Instruction
instruction_e9 = stubInstruction "0xe9"

-- 0xea "LD (a16),A", 3 byte operand, 16 cycles -,-,-,-
instruction_ea :: Instruction
instruction_ea = stubInstruction "0xea"

-- 0xeb,,,,,,,
instruction_eb :: Instruction
instruction_eb = stubInstruction "0xeb"

-- 0xec,,,,,,,
instruction_ec :: Instruction
instruction_ec = stubInstruction "0xec"

-- 0xed,,,,,,,
instruction_ed :: Instruction
instruction_ed = stubInstruction "0xed"

-- 0xee XOR d8, 2 byte operand, 8 cycles Z,0,0,0
instruction_ee :: Instruction
instruction_ee = stubInstruction "0xee"

-- 0xef RST 28H, 1 byte operand, 16 cycles -,-,-,-
instruction_ef :: Instruction
instruction_ef = stubInstruction "0xef"

-- 0xf0 "LDH A,(a8)", 2 byte operand, 12 cycles -,-,-,-
instruction_f0 :: Instruction
instruction_f0 = stubInstruction "0xf0"

-- 0xf1 POP AF, 1 byte operand, 12 cycles Z,N,H,C
instruction_f1 :: Instruction
instruction_f1 = stubInstruction "0xf1"

-- 0xf2 "LD A,(C)", 2 byte operand, 8 cycles -,-,-,-
instruction_f2 :: Instruction
instruction_f2 = stubInstruction "0xf2"

-- 0xf3 DI, 1 byte operand, 4 cycles -,-,-,-
instruction_f3 :: Instruction
instruction_f3 = stubInstruction "0xf3"

-- 0xf4,,,,,,,
instruction_f4 :: Instruction
instruction_f4 = stubInstruction "0xf4"

-- 0xf5 PUSH AF, 1 byte operand, 16 cycles -,-,-,-
instruction_f5 :: Instruction
instruction_f5 = stubInstruction "0xf5"

-- 0xf6 OR d8, 2 byte operand, 8 cycles Z,0,0,0
instruction_f6 :: Instruction
instruction_f6 = stubInstruction "0xf6"

-- 0xf7 RST 30H, 1 byte operand, 16 cycles -,-,-,-
instruction_f7 :: Instruction
instruction_f7 = stubInstruction "0xf7"

-- 0xf8 "LD HL,SP+r8", 2 byte operand, 12 cycles 0,0,H,C
instruction_f8 :: Instruction
instruction_f8 = stubInstruction "0xf8"

-- 0xf9 "LD SP,HL", 1 byte operand, 8 cycles -,-,-,-
instruction_f9 :: Instruction
instruction_f9 = stubInstruction "0xf9"

-- 0xfa "LD A,(a16)", 3 byte operand, 16 cycles -,-,-,-
instruction_fa :: Instruction
instruction_fa = stubInstruction "0xfa"

-- 0xfb EI, 1 byte operand, 4 cycles -,-,-,-
instruction_fb :: Instruction
instruction_fb = stubInstruction "0xfb"

-- 0xfc,,,,,,,
instruction_fc :: Instruction
instruction_fc = stubInstruction "0xfc"

-- 0xfd,,,,,,,
instruction_fd :: Instruction
instruction_fd = stubInstruction "0xfd"

-- 0xfe CP d8, 2 byte operand, 8 cycles Z,1,H,C
instruction_fe :: Instruction
instruction_fe = stubInstruction "0xfe"

-- 0xff RST 38H, 1 byte operand, 16 cycles -,-,-,-
instruction_ff :: Instruction
instruction_ff = stubInstruction "0xff"

-- 0xcb00 RLC B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb00 :: Instruction
instruction_cb00 = stubInstruction "0xcb00"

-- 0xcb01 RLC C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb01 :: Instruction
instruction_cb01 = stubInstruction "0xcb01"

-- 0xcb02 RLC D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb02 :: Instruction
instruction_cb02 = stubInstruction "0xcb02"

-- 0xcb03 RLC E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb03 :: Instruction
instruction_cb03 = stubInstruction "0xcb03"

-- 0xcb04 RLC H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb04 :: Instruction
instruction_cb04 = stubInstruction "0xcb04"

-- 0xcb05 RLC L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb05 :: Instruction
instruction_cb05 = stubInstruction "0xcb05"

-- 0xcb06 RLC (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb06 :: Instruction
instruction_cb06 = stubInstruction "0xcb06"

-- 0xcb07 RLC A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb07 :: Instruction
instruction_cb07 = stubInstruction "0xcb07"

-- 0xcb08 RRC B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb08 :: Instruction
instruction_cb08 = stubInstruction "0xcb08"

-- 0xcb09 RRC C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb09 :: Instruction
instruction_cb09 = stubInstruction "0xcb09"

-- 0xcb0a RRC D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0a :: Instruction
instruction_cb0a = stubInstruction "0xcb0a"

-- 0xcb0b RRC E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0b :: Instruction
instruction_cb0b = stubInstruction "0xcb0b"

-- 0xcb0c RRC H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0c :: Instruction
instruction_cb0c = stubInstruction "0xcb0c"

-- 0xcb0d RRC L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0d :: Instruction
instruction_cb0d = stubInstruction "0xcb0d"

-- 0xcb0e RRC (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb0e :: Instruction
instruction_cb0e = stubInstruction "0xcb0e"

-- 0xcb0f RRC A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb0f :: Instruction
instruction_cb0f = stubInstruction "0xcb0f"

-- 0xcb10 RL B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb10 :: Instruction
instruction_cb10 = stubInstruction "0xcb10"

-- 0xcb11 RL C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb11 :: Instruction
instruction_cb11 = stubInstruction "0xcb11"

-- 0xcb12 RL D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb12 :: Instruction
instruction_cb12 = stubInstruction "0xcb12"

-- 0xcb13 RL E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb13 :: Instruction
instruction_cb13 = stubInstruction "0xcb13"

-- 0xcb14 RL H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb14 :: Instruction
instruction_cb14 = stubInstruction "0xcb14"

-- 0xcb15 RL L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb15 :: Instruction
instruction_cb15 = stubInstruction "0xcb15"

-- 0xcb16 RL (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb16 :: Instruction
instruction_cb16 = stubInstruction "0xcb16"

-- 0xcb17 RL A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb17 :: Instruction
instruction_cb17 = stubInstruction "0xcb17"

-- 0xcb18 RR B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb18 :: Instruction
instruction_cb18 = stubInstruction "0xcb18"

-- 0xcb19 RR C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb19 :: Instruction
instruction_cb19 = stubInstruction "0xcb19"

-- 0xcb1a RR D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1a :: Instruction
instruction_cb1a = stubInstruction "0xcb1a"

-- 0xcb1b RR E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1b :: Instruction
instruction_cb1b = stubInstruction "0xcb1b"

-- 0xcb1c RR H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1c :: Instruction
instruction_cb1c = stubInstruction "0xcb1c"

-- 0xcb1d RR L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1d :: Instruction
instruction_cb1d = stubInstruction "0xcb1d"

-- 0xcb1e RR (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb1e :: Instruction
instruction_cb1e = stubInstruction "0xcb1e"

-- 0xcb1f RR A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb1f :: Instruction
instruction_cb1f = stubInstruction "0xcb1f"

-- 0xcb20 SLA B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb20 :: Instruction
instruction_cb20 = stubInstruction "0xcb20"

-- 0xcb21 SLA C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb21 :: Instruction
instruction_cb21 = stubInstruction "0xcb21"

-- 0xcb22 SLA D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb22 :: Instruction
instruction_cb22 = stubInstruction "0xcb22"

-- 0xcb23 SLA E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb23 :: Instruction
instruction_cb23 = stubInstruction "0xcb23"

-- 0xcb24 SLA H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb24 :: Instruction
instruction_cb24 = stubInstruction "0xcb24"

-- 0xcb25 SLA L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb25 :: Instruction
instruction_cb25 = stubInstruction "0xcb25"

-- 0xcb26 SLA (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb26 :: Instruction
instruction_cb26 = stubInstruction "0xcb26"

-- 0xcb27 SLA A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb27 :: Instruction
instruction_cb27 = stubInstruction "0xcb27"

-- 0xcb28 SRA B, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb28 :: Instruction
instruction_cb28 = stubInstruction "0xcb28"

-- 0xcb29 SRA C, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb29 :: Instruction
instruction_cb29 = stubInstruction "0xcb29"

-- 0xcb2a SRA D, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2a :: Instruction
instruction_cb2a = stubInstruction "0xcb2a"

-- 0xcb2b SRA E, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2b :: Instruction
instruction_cb2b = stubInstruction "0xcb2b"

-- 0xcb2c SRA H, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2c :: Instruction
instruction_cb2c = stubInstruction "0xcb2c"

-- 0xcb2d SRA L, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2d :: Instruction
instruction_cb2d = stubInstruction "0xcb2d"

-- 0xcb2e SRA (HL), 2 byte operand, 16 cycles Z,0,0,0
instruction_cb2e :: Instruction
instruction_cb2e = stubInstruction "0xcb2e"

-- 0xcb2f SRA A, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb2f :: Instruction
instruction_cb2f = stubInstruction "0xcb2f"

-- 0xcb30 SWAP B, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb30 :: Instruction
instruction_cb30 = stubInstruction "0xcb30"

-- 0xcb31 SWAP C, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb31 :: Instruction
instruction_cb31 = stubInstruction "0xcb31"

-- 0xcb32 SWAP D, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb32 :: Instruction
instruction_cb32 = stubInstruction "0xcb32"

-- 0xcb33 SWAP E, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb33 :: Instruction
instruction_cb33 = stubInstruction "0xcb33"

-- 0xcb34 SWAP H, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb34 :: Instruction
instruction_cb34 = stubInstruction "0xcb34"

-- 0xcb35 SWAP L, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb35 :: Instruction
instruction_cb35 = stubInstruction "0xcb35"

-- 0xcb36 SWAP (HL), 2 byte operand, 16 cycles Z,0,0,0
instruction_cb36 :: Instruction
instruction_cb36 = stubInstruction "0xcb36"

-- 0xcb37 SWAP A, 2 byte operand, 8 cycles Z,0,0,0
instruction_cb37 :: Instruction
instruction_cb37 = stubInstruction "0xcb37"

-- 0xcb38 SRL B, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb38 :: Instruction
instruction_cb38 = stubInstruction "0xcb38"

-- 0xcb39 SRL C, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb39 :: Instruction
instruction_cb39 = stubInstruction "0xcb39"

-- 0xcb3a SRL D, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3a :: Instruction
instruction_cb3a = stubInstruction "0xcb3a"

-- 0xcb3b SRL E, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3b :: Instruction
instruction_cb3b = stubInstruction "0xcb3b"

-- 0xcb3c SRL H, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3c :: Instruction
instruction_cb3c = stubInstruction "0xcb3c"

-- 0xcb3d SRL L, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3d :: Instruction
instruction_cb3d = stubInstruction "0xcb3d"

-- 0xcb3e SRL (HL), 2 byte operand, 16 cycles Z,0,0,C
instruction_cb3e :: Instruction
instruction_cb3e = stubInstruction "0xcb3e"

-- 0xcb3f SRL A, 2 byte operand, 8 cycles Z,0,0,C
instruction_cb3f :: Instruction
instruction_cb3f = stubInstruction "0xcb3f"

-- 0xcb40 "BIT 0,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb40 :: Instruction
instruction_cb40 = stubInstruction "0xcb40"

-- 0xcb41 "BIT 0,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb41 :: Instruction
instruction_cb41 = stubInstruction "0xcb41"

-- 0xcb42 "BIT 0,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb42 :: Instruction
instruction_cb42 = stubInstruction "0xcb42"

-- 0xcb43 "BIT 0,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb43 :: Instruction
instruction_cb43 = stubInstruction "0xcb43"

-- 0xcb44 "BIT 0,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb44 :: Instruction
instruction_cb44 = stubInstruction "0xcb44"

-- 0xcb45 "BIT 0,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb45 :: Instruction
instruction_cb45 = stubInstruction "0xcb45"

-- 0xcb46 "BIT 0,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb46 :: Instruction
instruction_cb46 = stubInstruction "0xcb46"

-- 0xcb47 "BIT 0,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb47 :: Instruction
instruction_cb47 = stubInstruction "0xcb47"

-- 0xcb48 "BIT 1,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb48 :: Instruction
instruction_cb48 = stubInstruction "0xcb48"

-- 0xcb49 "BIT 1,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb49 :: Instruction
instruction_cb49 = stubInstruction "0xcb49"

-- 0xcb4a "BIT 1,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4a :: Instruction
instruction_cb4a = stubInstruction "0xcb4a"

-- 0xcb4b "BIT 1,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4b :: Instruction
instruction_cb4b = stubInstruction "0xcb4b"

-- 0xcb4c "BIT 1,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4c :: Instruction
instruction_cb4c = stubInstruction "0xcb4c"

-- 0xcb4d "BIT 1,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4d :: Instruction
instruction_cb4d = stubInstruction "0xcb4d"

-- 0xcb4e "BIT 1,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb4e :: Instruction
instruction_cb4e = stubInstruction "0xcb4e"

-- 0xcb4f "BIT 1,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb4f :: Instruction
instruction_cb4f = stubInstruction "0xcb4f"

-- 0xcb50 "BIT 2,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb50 :: Instruction
instruction_cb50 = stubInstruction "0xcb50"

-- 0xcb51 "BIT 2,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb51 :: Instruction
instruction_cb51 = stubInstruction "0xcb51"

-- 0xcb52 "BIT 2,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb52 :: Instruction
instruction_cb52 = stubInstruction "0xcb52"

-- 0xcb53 "BIT 2,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb53 :: Instruction
instruction_cb53 = stubInstruction "0xcb53"

-- 0xcb54 "BIT 2,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb54 :: Instruction
instruction_cb54 = stubInstruction "0xcb54"

-- 0xcb55 "BIT 2,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb55 :: Instruction
instruction_cb55 = stubInstruction "0xcb55"

-- 0xcb56 "BIT 2,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb56 :: Instruction
instruction_cb56 = stubInstruction "0xcb56"

-- 0xcb57 "BIT 2,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb57 :: Instruction
instruction_cb57 = stubInstruction "0xcb57"

-- 0xcb58 "BIT 3,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb58 :: Instruction
instruction_cb58 = stubInstruction "0xcb58"

-- 0xcb59 "BIT 3,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb59 :: Instruction
instruction_cb59 = stubInstruction "0xcb59"

-- 0xcb5a "BIT 3,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5a :: Instruction
instruction_cb5a = stubInstruction "0xcb5a"

-- 0xcb5b "BIT 3,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5b :: Instruction
instruction_cb5b = stubInstruction "0xcb5b"

-- 0xcb5c "BIT 3,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5c :: Instruction
instruction_cb5c = stubInstruction "0xcb5c"

-- 0xcb5d "BIT 3,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5d :: Instruction
instruction_cb5d = stubInstruction "0xcb5d"

-- 0xcb5e "BIT 3,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb5e :: Instruction
instruction_cb5e = stubInstruction "0xcb5e"

-- 0xcb5f "BIT 3,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb5f :: Instruction
instruction_cb5f = stubInstruction "0xcb5f"

-- 0xcb60 "BIT 4,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb60 :: Instruction
instruction_cb60 = stubInstruction "0xcb60"

-- 0xcb61 "BIT 4,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb61 :: Instruction
instruction_cb61 = stubInstruction "0xcb61"

-- 0xcb62 "BIT 4,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb62 :: Instruction
instruction_cb62 = stubInstruction "0xcb62"

-- 0xcb63 "BIT 4,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb63 :: Instruction
instruction_cb63 = stubInstruction "0xcb63"

-- 0xcb64 "BIT 4,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb64 :: Instruction
instruction_cb64 = stubInstruction "0xcb64"

-- 0xcb65 "BIT 4,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb65 :: Instruction
instruction_cb65 = stubInstruction "0xcb65"

-- 0xcb66 "BIT 4,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb66 :: Instruction
instruction_cb66 = stubInstruction "0xcb66"

-- 0xcb67 "BIT 4,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb67 :: Instruction
instruction_cb67 = stubInstruction "0xcb67"

-- 0xcb68 "BIT 5,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb68 :: Instruction
instruction_cb68 = stubInstruction "0xcb68"

-- 0xcb69 "BIT 5,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb69 :: Instruction
instruction_cb69 = stubInstruction "0xcb69"

-- 0xcb6a "BIT 5,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6a :: Instruction
instruction_cb6a = stubInstruction "0xcb6a"

-- 0xcb6b "BIT 5,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6b :: Instruction
instruction_cb6b = stubInstruction "0xcb6b"

-- 0xcb6c "BIT 5,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6c :: Instruction
instruction_cb6c = stubInstruction "0xcb6c"

-- 0xcb6d "BIT 5,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6d :: Instruction
instruction_cb6d = stubInstruction "0xcb6d"

-- 0xcb6e "BIT 5,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb6e :: Instruction
instruction_cb6e = stubInstruction "0xcb6e"

-- 0xcb6f "BIT 5,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb6f :: Instruction
instruction_cb6f = stubInstruction "0xcb6f"

-- 0xcb70 "BIT 6,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb70 :: Instruction
instruction_cb70 = stubInstruction "0xcb70"

-- 0xcb71 "BIT 6,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb71 :: Instruction
instruction_cb71 = stubInstruction "0xcb71"

-- 0xcb72 "BIT 6,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb72 :: Instruction
instruction_cb72 = stubInstruction "0xcb72"

-- 0xcb73 "BIT 6,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb73 :: Instruction
instruction_cb73 = stubInstruction "0xcb73"

-- 0xcb74 "BIT 6,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb74 :: Instruction
instruction_cb74 = stubInstruction "0xcb74"

-- 0xcb75 "BIT 6,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb75 :: Instruction
instruction_cb75 = stubInstruction "0xcb75"

-- 0xcb76 "BIT 6,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb76 :: Instruction
instruction_cb76 = stubInstruction "0xcb76"

-- 0xcb77 "BIT 6,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb77 :: Instruction
instruction_cb77 = stubInstruction "0xcb77"

-- 0xcb78 "BIT 7,B", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb78 :: Instruction
instruction_cb78 = stubInstruction "0xcb78"

-- 0xcb79 "BIT 7,C", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb79 :: Instruction
instruction_cb79 = stubInstruction "0xcb79"

-- 0xcb7a "BIT 7,D", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7a :: Instruction
instruction_cb7a = stubInstruction "0xcb7a"

-- 0xcb7b "BIT 7,E", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7b :: Instruction
instruction_cb7b = stubInstruction "0xcb7b"

-- 0xcb7c "BIT 7,H", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7c :: Instruction
instruction_cb7c = stubInstruction "0xcb7c"

-- 0xcb7d "BIT 7,L", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7d :: Instruction
instruction_cb7d = stubInstruction "0xcb7d"

-- 0xcb7e "BIT 7,(HL)", 2 byte operand, 16 cycles Z,0,1,-
instruction_cb7e :: Instruction
instruction_cb7e = stubInstruction "0xcb7e"

-- 0xcb7f "BIT 7,A", 2 byte operand, 8 cycles Z,0,1,-
instruction_cb7f :: Instruction
instruction_cb7f = stubInstruction "0xcb7f"

-- 0xcb80 "RES 0,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb80 :: Instruction
instruction_cb80 = stubInstruction "0xcb80"

-- 0xcb81 "RES 0,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb81 :: Instruction
instruction_cb81 = stubInstruction "0xcb81"

-- 0xcb82 "RES 0,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb82 :: Instruction
instruction_cb82 = stubInstruction "0xcb82"

-- 0xcb83 "RES 0,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb83 :: Instruction
instruction_cb83 = stubInstruction "0xcb83"

-- 0xcb84 "RES 0,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb84 :: Instruction
instruction_cb84 = stubInstruction "0xcb84"

-- 0xcb85 "RES 0,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb85 :: Instruction
instruction_cb85 = stubInstruction "0xcb85"

-- 0xcb86 "RES 0,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb86 :: Instruction
instruction_cb86 = stubInstruction "0xcb86"

-- 0xcb87 "RES 0,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb87 :: Instruction
instruction_cb87 = stubInstruction "0xcb87"

-- 0xcb88 "RES 1,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb88 :: Instruction
instruction_cb88 = stubInstruction "0xcb88"

-- 0xcb89 "RES 1,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb89 :: Instruction
instruction_cb89 = stubInstruction "0xcb89"

-- 0xcb8a "RES 1,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8a :: Instruction
instruction_cb8a = stubInstruction "0xcb8a"

-- 0xcb8b "RES 1,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8b :: Instruction
instruction_cb8b = stubInstruction "0xcb8b"

-- 0xcb8c "RES 1,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8c :: Instruction
instruction_cb8c = stubInstruction "0xcb8c"

-- 0xcb8d "RES 1,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8d :: Instruction
instruction_cb8d = stubInstruction "0xcb8d"

-- 0xcb8e "RES 1,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb8e :: Instruction
instruction_cb8e = stubInstruction "0xcb8e"

-- 0xcb8f "RES 1,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb8f :: Instruction
instruction_cb8f = stubInstruction "0xcb8f"

-- 0xcb90 "RES 2,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb90 :: Instruction
instruction_cb90 = stubInstruction "0xcb90"

-- 0xcb91 "RES 2,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb91 :: Instruction
instruction_cb91 = stubInstruction "0xcb91"

-- 0xcb92 "RES 2,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb92 :: Instruction
instruction_cb92 = stubInstruction "0xcb92"

-- 0xcb93 "RES 2,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb93 :: Instruction
instruction_cb93 = stubInstruction "0xcb93"

-- 0xcb94 "RES 2,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb94 :: Instruction
instruction_cb94 = stubInstruction "0xcb94"

-- 0xcb95 "RES 2,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb95 :: Instruction
instruction_cb95 = stubInstruction "0xcb95"

-- 0xcb96 "RES 2,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb96 :: Instruction
instruction_cb96 = stubInstruction "0xcb96"

-- 0xcb97 "RES 2,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb97 :: Instruction
instruction_cb97 = stubInstruction "0xcb97"

-- 0xcb98 "RES 3,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cb98 :: Instruction
instruction_cb98 = stubInstruction "0xcb98"

-- 0xcb99 "RES 3,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cb99 :: Instruction
instruction_cb99 = stubInstruction "0xcb99"

-- 0xcb9a "RES 3,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9a :: Instruction
instruction_cb9a = stubInstruction "0xcb9a"

-- 0xcb9b "RES 3,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9b :: Instruction
instruction_cb9b = stubInstruction "0xcb9b"

-- 0xcb9c "RES 3,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9c :: Instruction
instruction_cb9c = stubInstruction "0xcb9c"

-- 0xcb9d "RES 3,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9d :: Instruction
instruction_cb9d = stubInstruction "0xcb9d"

-- 0xcb9e "RES 3,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cb9e :: Instruction
instruction_cb9e = stubInstruction "0xcb9e"

-- 0xcb9f "RES 3,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cb9f :: Instruction
instruction_cb9f = stubInstruction "0xcb9f"

-- 0xcba0 "RES 4,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cba0 :: Instruction
instruction_cba0 = stubInstruction "0xcba0"

-- 0xcba1 "RES 4,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cba1 :: Instruction
instruction_cba1 = stubInstruction "0xcba1"

-- 0xcba2 "RES 4,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cba2 :: Instruction
instruction_cba2 = stubInstruction "0xcba2"

-- 0xcba3 "RES 4,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cba3 :: Instruction
instruction_cba3 = stubInstruction "0xcba3"

-- 0xcba4 "RES 4,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cba4 :: Instruction
instruction_cba4 = stubInstruction "0xcba4"

-- 0xcba5 "RES 4,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cba5 :: Instruction
instruction_cba5 = stubInstruction "0xcba5"

-- 0xcba6 "RES 4,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cba6 :: Instruction
instruction_cba6 = stubInstruction "0xcba6"

-- 0xcba7 "RES 4,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cba7 :: Instruction
instruction_cba7 = stubInstruction "0xcba7"

-- 0xcba8 "RES 5,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cba8 :: Instruction
instruction_cba8 = stubInstruction "0xcba8"

-- 0xcba9 "RES 5,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cba9 :: Instruction
instruction_cba9 = stubInstruction "0xcba9"

-- 0xcbaa "RES 5,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbaa :: Instruction
instruction_cbaa = stubInstruction "0xcbaa"

-- 0xcbab "RES 5,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbab :: Instruction
instruction_cbab = stubInstruction "0xcbab"

-- 0xcbac "RES 5,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbac :: Instruction
instruction_cbac = stubInstruction "0xcbac"

-- 0xcbad "RES 5,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbad :: Instruction
instruction_cbad = stubInstruction "0xcbad"

-- 0xcbae "RES 5,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbae :: Instruction
instruction_cbae = stubInstruction "0xcbae"

-- 0xcbaf "RES 5,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbaf :: Instruction
instruction_cbaf = stubInstruction "0xcbaf"

-- 0xcbb0 "RES 6,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb0 :: Instruction
instruction_cbb0 = stubInstruction "0xcbb0"

-- 0xcbb1 "RES 6,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb1 :: Instruction
instruction_cbb1 = stubInstruction "0xcbb1"

-- 0xcbb2 "RES 6,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb2 :: Instruction
instruction_cbb2 = stubInstruction "0xcbb2"

-- 0xcbb3 "RES 6,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb3 :: Instruction
instruction_cbb3 = stubInstruction "0xcbb3"

-- 0xcbb4 "RES 6,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb4 :: Instruction
instruction_cbb4 = stubInstruction "0xcbb4"

-- 0xcbb5 "RES 6,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb5 :: Instruction
instruction_cbb5 = stubInstruction "0xcbb5"

-- 0xcbb6 "RES 6,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbb6 :: Instruction
instruction_cbb6 = stubInstruction "0xcbb6"

-- 0xcbb7 "RES 6,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb7 :: Instruction
instruction_cbb7 = stubInstruction "0xcbb7"

-- 0xcbb8 "RES 7,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb8 :: Instruction
instruction_cbb8 = stubInstruction "0xcbb8"

-- 0xcbb9 "RES 7,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbb9 :: Instruction
instruction_cbb9 = stubInstruction "0xcbb9"

-- 0xcbba "RES 7,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbba :: Instruction
instruction_cbba = stubInstruction "0xcbba"

-- 0xcbbb "RES 7,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbb :: Instruction
instruction_cbbb = stubInstruction "0xcbbb"

-- 0xcbbc "RES 7,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbc :: Instruction
instruction_cbbc = stubInstruction "0xcbbc"

-- 0xcbbd "RES 7,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbd :: Instruction
instruction_cbbd = stubInstruction "0xcbbd"

-- 0xcbbe "RES 7,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbbe :: Instruction
instruction_cbbe = stubInstruction "0xcbbe"

-- 0xcbbf "RES 7,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbbf :: Instruction
instruction_cbbf = stubInstruction "0xcbbf"

-- 0xcbc0 "SET 0,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc0 :: Instruction
instruction_cbc0 = stubInstruction "0xcbc0"

-- 0xcbc1 "SET 0,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc1 :: Instruction
instruction_cbc1 = stubInstruction "0xcbc1"

-- 0xcbc2 "SET 0,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc2 :: Instruction
instruction_cbc2 = stubInstruction "0xcbc2"

-- 0xcbc3 "SET 0,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc3 :: Instruction
instruction_cbc3 = stubInstruction "0xcbc3"

-- 0xcbc4 "SET 0,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc4 :: Instruction
instruction_cbc4 = stubInstruction "0xcbc4"

-- 0xcbc5 "SET 0,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc5 :: Instruction
instruction_cbc5 = stubInstruction "0xcbc5"

-- 0xcbc6 "SET 0,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbc6 :: Instruction
instruction_cbc6 = stubInstruction "0xcbc6"

-- 0xcbc7 "SET 0,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc7 :: Instruction
instruction_cbc7 = stubInstruction "0xcbc7"

-- 0xcbc8 "SET 1,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc8 :: Instruction
instruction_cbc8 = stubInstruction "0xcbc8"

-- 0xcbc9 "SET 1,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbc9 :: Instruction
instruction_cbc9 = stubInstruction "0xcbc9"

-- 0xcbca "SET 1,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbca :: Instruction
instruction_cbca = stubInstruction "0xcbca"

-- 0xcbcb "SET 1,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcb :: Instruction
instruction_cbcb = stubInstruction "0xcbcb"

-- 0xcbcc "SET 1,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcc :: Instruction
instruction_cbcc = stubInstruction "0xcbcc"

-- 0xcbcd "SET 1,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcd :: Instruction
instruction_cbcd = stubInstruction "0xcbcd"

-- 0xcbce "SET 1,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbce :: Instruction
instruction_cbce = stubInstruction "0xcbce"

-- 0xcbcf "SET 1,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbcf :: Instruction
instruction_cbcf = stubInstruction "0xcbcf"

-- 0xcbd0 "SET 2,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd0 :: Instruction
instruction_cbd0 = stubInstruction "0xcbd0"

-- 0xcbd1 "SET 2,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd1 :: Instruction
instruction_cbd1 = stubInstruction "0xcbd1"

-- 0xcbd2 "SET 2,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd2 :: Instruction
instruction_cbd2 = stubInstruction "0xcbd2"

-- 0xcbd3 "SET 2,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd3 :: Instruction
instruction_cbd3 = stubInstruction "0xcbd3"

-- 0xcbd4 "SET 2,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd4 :: Instruction
instruction_cbd4 = stubInstruction "0xcbd4"

-- 0xcbd5 "SET 2,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd5 :: Instruction
instruction_cbd5 = stubInstruction "0xcbd5"

-- 0xcbd6 "SET 2,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbd6 :: Instruction
instruction_cbd6 = stubInstruction "0xcbd6"

-- 0xcbd7 "SET 2,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd7 :: Instruction
instruction_cbd7 = stubInstruction "0xcbd7"

-- 0xcbd8 "SET 3,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd8 :: Instruction
instruction_cbd8 = stubInstruction "0xcbd8"

-- 0xcbd9 "SET 3,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbd9 :: Instruction
instruction_cbd9 = stubInstruction "0xcbd9"

-- 0xcbda "SET 3,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbda :: Instruction
instruction_cbda = stubInstruction "0xcbda"

-- 0xcbdb "SET 3,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdb :: Instruction
instruction_cbdb = stubInstruction "0xcbdb"

-- 0xcbdc "SET 3,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdc :: Instruction
instruction_cbdc = stubInstruction "0xcbdc"

-- 0xcbdd "SET 3,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdd :: Instruction
instruction_cbdd = stubInstruction "0xcbdd"

-- 0xcbde "SET 3,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbde :: Instruction
instruction_cbde = stubInstruction "0xcbde"

-- 0xcbdf "SET 3,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbdf :: Instruction
instruction_cbdf = stubInstruction "0xcbdf"

-- 0xcbe0 "SET 4,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe0 :: Instruction
instruction_cbe0 = stubInstruction "0xcbe0"

-- 0xcbe1 "SET 4,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe1 :: Instruction
instruction_cbe1 = stubInstruction "0xcbe1"

-- 0xcbe2 "SET 4,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe2 :: Instruction
instruction_cbe2 = stubInstruction "0xcbe2"

-- 0xcbe3 "SET 4,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe3 :: Instruction
instruction_cbe3 = stubInstruction "0xcbe3"

-- 0xcbe4 "SET 4,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe4 :: Instruction
instruction_cbe4 = stubInstruction "0xcbe4"

-- 0xcbe5 "SET 4,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe5 :: Instruction
instruction_cbe5 = stubInstruction "0xcbe5"

-- 0xcbe6 "SET 4,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbe6 :: Instruction
instruction_cbe6 = stubInstruction "0xcbe6"

-- 0xcbe7 "SET 4,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe7 :: Instruction
instruction_cbe7 = stubInstruction "0xcbe7"

-- 0xcbe8 "SET 5,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe8 :: Instruction
instruction_cbe8 = stubInstruction "0xcbe8"

-- 0xcbe9 "SET 5,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbe9 :: Instruction
instruction_cbe9 = stubInstruction "0xcbe9"

-- 0xcbea "SET 5,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbea :: Instruction
instruction_cbea = stubInstruction "0xcbea"

-- 0xcbeb "SET 5,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbeb :: Instruction
instruction_cbeb = stubInstruction "0xcbeb"

-- 0xcbec "SET 5,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbec :: Instruction
instruction_cbec = stubInstruction "0xcbec"

-- 0xcbed "SET 5,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbed :: Instruction
instruction_cbed = stubInstruction "0xcbed"

-- 0xcbee "SET 5,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbee :: Instruction
instruction_cbee = stubInstruction "0xcbee"

-- 0xcbef "SET 5,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbef :: Instruction
instruction_cbef = stubInstruction "0xcbef"

-- 0xcbf0 "SET 6,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf0 :: Instruction
instruction_cbf0 = stubInstruction "0xcbf0"

-- 0xcbf1 "SET 6,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf1 :: Instruction
instruction_cbf1 = stubInstruction "0xcbf1"

-- 0xcbf2 "SET 6,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf2 :: Instruction
instruction_cbf2 = stubInstruction "0xcbf2"

-- 0xcbf3 "SET 6,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf3 :: Instruction
instruction_cbf3 = stubInstruction "0xcbf3"

-- 0xcbf4 "SET 6,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf4 :: Instruction
instruction_cbf4 = stubInstruction "0xcbf4"

-- 0xcbf5 "SET 6,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf5 :: Instruction
instruction_cbf5 = stubInstruction "0xcbf5"

-- 0xcbf6 "SET 6,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbf6 :: Instruction
instruction_cbf6 = stubInstruction "0xcbf6"

-- 0xcbf7 "SET 6,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf7 :: Instruction
instruction_cbf7 = stubInstruction "0xcbf7"

-- 0xcbf8 "SET 7,B", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf8 :: Instruction
instruction_cbf8 = stubInstruction "0xcbf8"

-- 0xcbf9 "SET 7,C", 2 byte operand, 8 cycles -,-,-,-
instruction_cbf9 :: Instruction
instruction_cbf9 = stubInstruction "0xcbf9"

-- 0xcbfa "SET 7,D", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfa :: Instruction
instruction_cbfa = stubInstruction "0xcbfa"

-- 0xcbfb "SET 7,E", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfb :: Instruction
instruction_cbfb = stubInstruction "0xcbfb"

-- 0xcbfc "SET 7,H", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfc :: Instruction
instruction_cbfc = stubInstruction "0xcbfc"

-- 0xcbfd "SET 7,L", 2 byte operand, 8 cycles -,-,-,-
instruction_cbfd :: Instruction
instruction_cbfd = stubInstruction "0xcbfd"

-- 0xcbfe "SET 7,(HL)", 2 byte operand, 16 cycles -,-,-,-
instruction_cbfe :: Instruction
instruction_cbfe = stubInstruction "0xcbfe"

-- 0xcbff "SET 7,A", 2 byte operand, 8 cycles -,-,-,-
instruction_cbff :: Instruction
instruction_cbff = stubInstruction "0xcbff"

instructionMap :: Map [Word8] Instruction
instructionMap = fromList [
    ([0x00], instruction_00),
    ([0x01], instruction_01),
    ([0x02], instruction_02),
    ([0x03], instruction_03),
    ([0x04], instruction_04),
    ([0x05], instruction_05),
    ([0x06], instruction_06),
    ([0x07], instruction_07),
    ([0x08], instruction_08),
    ([0x09], instruction_09),
    ([0x0a], instruction_0a),
    ([0x0b], instruction_0b),
    ([0x0c], instruction_0c),
    ([0x0d], instruction_0d),
    ([0x0e], instruction_0e),
    ([0x0f], instruction_0f),
    ([0x10], instruction_10),
    ([0x11], instruction_11),
    ([0x12], instruction_12),
    ([0x13], instruction_13),
    ([0x14], instruction_14),
    ([0x15], instruction_15),
    ([0x16], instruction_16),
    ([0x17], instruction_17),
    ([0x18], instruction_18),
    ([0x19], instruction_19),
    ([0x1a], instruction_1a),
    ([0x1b], instruction_1b),
    ([0x1c], instruction_1c),
    ([0x1d], instruction_1d),
    ([0x1e], instruction_1e),
    ([0x1f], instruction_1f),
    ([0x20], instruction_20),
    ([0x21], instruction_21),
    ([0x22], instruction_22),
    ([0x23], instruction_23),
    ([0x24], instruction_24),
    ([0x25], instruction_25),
    ([0x26], instruction_26),
    ([0x27], instruction_27),
    ([0x28], instruction_28),
    ([0x29], instruction_29),
    ([0x2a], instruction_2a),
    ([0x2b], instruction_2b),
    ([0x2c], instruction_2c),
    ([0x2d], instruction_2d),
    ([0x2e], instruction_2e),
    ([0x2f], instruction_2f),
    ([0x30], instruction_30),
    ([0x31], instruction_31),
    ([0x32], instruction_32),
    ([0x33], instruction_33),
    ([0x34], instruction_34),
    ([0x35], instruction_35),
    ([0x36], instruction_36),
    ([0x37], instruction_37),
    ([0x38], instruction_38),
    ([0x39], instruction_39),
    ([0x3a], instruction_3a),
    ([0x3b], instruction_3b),
    ([0x3c], instruction_3c),
    ([0x3d], instruction_3d),
    ([0x3e], instruction_3e),
    ([0x3f], instruction_3f),
    ([0x40], instruction_40),
    ([0x41], instruction_41),
    ([0x42], instruction_42),
    ([0x43], instruction_43),
    ([0x44], instruction_44),
    ([0x45], instruction_45),
    ([0x46], instruction_46),
    ([0x47], instruction_47),
    ([0x48], instruction_48),
    ([0x49], instruction_49),
    ([0x4a], instruction_4a),
    ([0x4b], instruction_4b),
    ([0x4c], instruction_4c),
    ([0x4d], instruction_4d),
    ([0x4e], instruction_4e),
    ([0x4f], instruction_4f),
    ([0x50], instruction_50),
    ([0x51], instruction_51),
    ([0x52], instruction_52),
    ([0x53], instruction_53),
    ([0x54], instruction_54),
    ([0x55], instruction_55),
    ([0x56], instruction_56),
    ([0x57], instruction_57),
    ([0x58], instruction_58),
    ([0x59], instruction_59),
    ([0x5a], instruction_5a),
    ([0x5b], instruction_5b),
    ([0x5c], instruction_5c),
    ([0x5d], instruction_5d),
    ([0x5e], instruction_5e),
    ([0x5f], instruction_5f),
    ([0x60], instruction_60),
    ([0x61], instruction_61),
    ([0x62], instruction_62),
    ([0x63], instruction_63),
    ([0x64], instruction_64),
    ([0x65], instruction_65),
    ([0x66], instruction_66),
    ([0x67], instruction_67),
    ([0x68], instruction_68),
    ([0x69], instruction_69),
    ([0x6a], instruction_6a),
    ([0x6b], instruction_6b),
    ([0x6c], instruction_6c),
    ([0x6d], instruction_6d),
    ([0x6e], instruction_6e),
    ([0x6f], instruction_6f),
    ([0x70], instruction_70),
    ([0x71], instruction_71),
    ([0x72], instruction_72),
    ([0x73], instruction_73),
    ([0x74], instruction_74),
    ([0x75], instruction_75),
    ([0x76], instruction_76),
    ([0x77], instruction_77),
    ([0x78], instruction_78),
    ([0x79], instruction_79),
    ([0x7a], instruction_7a),
    ([0x7b], instruction_7b),
    ([0x7c], instruction_7c),
    ([0x7d], instruction_7d),
    ([0x7e], instruction_7e),
    ([0x7f], instruction_7f),
    ([0x80], instruction_80),
    ([0x81], instruction_81),
    ([0x82], instruction_82),
    ([0x83], instruction_83),
    ([0x84], instruction_84),
    ([0x85], instruction_85),
    ([0x86], instruction_86),
    ([0x87], instruction_87),
    ([0x88], instruction_88),
    ([0x89], instruction_89),
    ([0x8a], instruction_8a),
    ([0x8b], instruction_8b),
    ([0x8c], instruction_8c),
    ([0x8d], instruction_8d),
    ([0x8e], instruction_8e),
    ([0x8f], instruction_8f),
    ([0x90], instruction_90),
    ([0x91], instruction_91),
    ([0x92], instruction_92),
    ([0x93], instruction_93),
    ([0x94], instruction_94),
    ([0x95], instruction_95),
    ([0x96], instruction_96),
    ([0x97], instruction_97),
    ([0x98], instruction_98),
    ([0x99], instruction_99),
    ([0x9a], instruction_9a),
    ([0x9b], instruction_9b),
    ([0x9c], instruction_9c),
    ([0x9d], instruction_9d),
    ([0x9e], instruction_9e),
    ([0x9f], instruction_9f),
    ([0xa0], instruction_a0),
    ([0xa1], instruction_a1),
    ([0xa2], instruction_a2),
    ([0xa3], instruction_a3),
    ([0xa4], instruction_a4),
    ([0xa5], instruction_a5),
    ([0xa6], instruction_a6),
    ([0xa7], instruction_a7),
    ([0xa8], instruction_a8),
    ([0xa9], instruction_a9),
    ([0xaa], instruction_aa),
    ([0xab], instruction_ab),
    ([0xac], instruction_ac),
    ([0xad], instruction_ad),
    ([0xae], instruction_ae),
    ([0xaf], instruction_af),
    ([0xb0], instruction_b0),
    ([0xb1], instruction_b1),
    ([0xb2], instruction_b2),
    ([0xb3], instruction_b3),
    ([0xb4], instruction_b4),
    ([0xb5], instruction_b5),
    ([0xb6], instruction_b6),
    ([0xb7], instruction_b7),
    ([0xb8], instruction_b8),
    ([0xb9], instruction_b9),
    ([0xba], instruction_ba),
    ([0xbb], instruction_bb),
    ([0xbc], instruction_bc),
    ([0xbd], instruction_bd),
    ([0xbe], instruction_be),
    ([0xbf], instruction_bf),
    ([0xc0], instruction_c0),
    ([0xc1], instruction_c1),
    ([0xc2], instruction_c2),
    ([0xc3], instruction_c3),
    ([0xc4], instruction_c4),
    ([0xc5], instruction_c5),
    ([0xc6], instruction_c6),
    ([0xc7], instruction_c7),
    ([0xc8], instruction_c8),
    ([0xc9], instruction_c9),
    ([0xca], instruction_ca),
    ([0xcb], instruction_cb),
    ([0xcc], instruction_cc),
    ([0xcd], instruction_cd),
    ([0xce], instruction_ce),
    ([0xcf], instruction_cf),
    ([0xd0], instruction_d0),
    ([0xd1], instruction_d1),
    ([0xd2], instruction_d2),
    ([0xd3], instruction_d3),
    ([0xd4], instruction_d4),
    ([0xd5], instruction_d5),
    ([0xd6], instruction_d6),
    ([0xd7], instruction_d7),
    ([0xd8], instruction_d8),
    ([0xd9], instruction_d9),
    ([0xda], instruction_da),
    ([0xdb], instruction_db),
    ([0xdc], instruction_dc),
    ([0xdd], instruction_dd),
    ([0xde], instruction_de),
    ([0xdf], instruction_df),
    ([0xe0], instruction_e0),
    ([0xe1], instruction_e1),
    ([0xe2], instruction_e2),
    ([0xe3], instruction_e3),
    ([0xe4], instruction_e4),
    ([0xe5], instruction_e5),
    ([0xe6], instruction_e6),
    ([0xe7], instruction_e7),
    ([0xe8], instruction_e8),
    ([0xe9], instruction_e9),
    ([0xea], instruction_ea),
    ([0xeb], instruction_eb),
    ([0xec], instruction_ec),
    ([0xed], instruction_ed),
    ([0xee], instruction_ee),
    ([0xef], instruction_ef),
    ([0xf0], instruction_f0),
    ([0xf1], instruction_f1),
    ([0xf2], instruction_f2),
    ([0xf3], instruction_f3),
    ([0xf4], instruction_f4),
    ([0xf5], instruction_f5),
    ([0xf6], instruction_f6),
    ([0xf7], instruction_f7),
    ([0xf8], instruction_f8),
    ([0xf9], instruction_f9),
    ([0xfa], instruction_fa),
    ([0xfb], instruction_fb),
    ([0xfc], instruction_fc),
    ([0xfd], instruction_fd),
    ([0xfe], instruction_fe),
    ([0xff], instruction_ff),
    ([0xcb, 0x00], instruction_cb00),
    ([0xcb, 0x01], instruction_cb01),
    ([0xcb, 0x02], instruction_cb02),
    ([0xcb, 0x03], instruction_cb03),
    ([0xcb, 0x04], instruction_cb04),
    ([0xcb, 0x05], instruction_cb05),
    ([0xcb, 0x06], instruction_cb06),
    ([0xcb, 0x07], instruction_cb07),
    ([0xcb, 0x08], instruction_cb08),
    ([0xcb, 0x09], instruction_cb09),
    ([0xcb, 0x0a], instruction_cb0a),
    ([0xcb, 0x0b], instruction_cb0b),
    ([0xcb, 0x0c], instruction_cb0c),
    ([0xcb, 0x0d], instruction_cb0d),
    ([0xcb, 0x0e], instruction_cb0e),
    ([0xcb, 0x0f], instruction_cb0f),
    ([0xcb, 0x10], instruction_cb10),
    ([0xcb, 0x11], instruction_cb11),
    ([0xcb, 0x12], instruction_cb12),
    ([0xcb, 0x13], instruction_cb13),
    ([0xcb, 0x14], instruction_cb14),
    ([0xcb, 0x15], instruction_cb15),
    ([0xcb, 0x16], instruction_cb16),
    ([0xcb, 0x17], instruction_cb17),
    ([0xcb, 0x18], instruction_cb18),
    ([0xcb, 0x19], instruction_cb19),
    ([0xcb, 0x1a], instruction_cb1a),
    ([0xcb, 0x1b], instruction_cb1b),
    ([0xcb, 0x1c], instruction_cb1c),
    ([0xcb, 0x1d], instruction_cb1d),
    ([0xcb, 0x1e], instruction_cb1e),
    ([0xcb, 0x1f], instruction_cb1f),
    ([0xcb, 0x20], instruction_cb20),
    ([0xcb, 0x21], instruction_cb21),
    ([0xcb, 0x22], instruction_cb22),
    ([0xcb, 0x23], instruction_cb23),
    ([0xcb, 0x24], instruction_cb24),
    ([0xcb, 0x25], instruction_cb25),
    ([0xcb, 0x26], instruction_cb26),
    ([0xcb, 0x27], instruction_cb27),
    ([0xcb, 0x28], instruction_cb28),
    ([0xcb, 0x29], instruction_cb29),
    ([0xcb, 0x2a], instruction_cb2a),
    ([0xcb, 0x2b], instruction_cb2b),
    ([0xcb, 0x2c], instruction_cb2c),
    ([0xcb, 0x2d], instruction_cb2d),
    ([0xcb, 0x2e], instruction_cb2e),
    ([0xcb, 0x2f], instruction_cb2f),
    ([0xcb, 0x30], instruction_cb30),
    ([0xcb, 0x31], instruction_cb31),
    ([0xcb, 0x32], instruction_cb32),
    ([0xcb, 0x33], instruction_cb33),
    ([0xcb, 0x34], instruction_cb34),
    ([0xcb, 0x35], instruction_cb35),
    ([0xcb, 0x36], instruction_cb36),
    ([0xcb, 0x37], instruction_cb37),
    ([0xcb, 0x38], instruction_cb38),
    ([0xcb, 0x39], instruction_cb39),
    ([0xcb, 0x3a], instruction_cb3a),
    ([0xcb, 0x3b], instruction_cb3b),
    ([0xcb, 0x3c], instruction_cb3c),
    ([0xcb, 0x3d], instruction_cb3d),
    ([0xcb, 0x3e], instruction_cb3e),
    ([0xcb, 0x3f], instruction_cb3f),
    ([0xcb, 0x40], instruction_cb40),
    ([0xcb, 0x41], instruction_cb41),
    ([0xcb, 0x42], instruction_cb42),
    ([0xcb, 0x43], instruction_cb43),
    ([0xcb, 0x44], instruction_cb44),
    ([0xcb, 0x45], instruction_cb45),
    ([0xcb, 0x46], instruction_cb46),
    ([0xcb, 0x47], instruction_cb47),
    ([0xcb, 0x48], instruction_cb48),
    ([0xcb, 0x49], instruction_cb49),
    ([0xcb, 0x4a], instruction_cb4a),
    ([0xcb, 0x4b], instruction_cb4b),
    ([0xcb, 0x4c], instruction_cb4c),
    ([0xcb, 0x4d], instruction_cb4d),
    ([0xcb, 0x4e], instruction_cb4e),
    ([0xcb, 0x4f], instruction_cb4f),
    ([0xcb, 0x50], instruction_cb50),
    ([0xcb, 0x51], instruction_cb51),
    ([0xcb, 0x52], instruction_cb52),
    ([0xcb, 0x53], instruction_cb53),
    ([0xcb, 0x54], instruction_cb54),
    ([0xcb, 0x55], instruction_cb55),
    ([0xcb, 0x56], instruction_cb56),
    ([0xcb, 0x57], instruction_cb57),
    ([0xcb, 0x58], instruction_cb58),
    ([0xcb, 0x59], instruction_cb59),
    ([0xcb, 0x5a], instruction_cb5a),
    ([0xcb, 0x5b], instruction_cb5b),
    ([0xcb, 0x5c], instruction_cb5c),
    ([0xcb, 0x5d], instruction_cb5d),
    ([0xcb, 0x5e], instruction_cb5e),
    ([0xcb, 0x5f], instruction_cb5f),
    ([0xcb, 0x60], instruction_cb60),
    ([0xcb, 0x61], instruction_cb61),
    ([0xcb, 0x62], instruction_cb62),
    ([0xcb, 0x63], instruction_cb63),
    ([0xcb, 0x64], instruction_cb64),
    ([0xcb, 0x65], instruction_cb65),
    ([0xcb, 0x66], instruction_cb66),
    ([0xcb, 0x67], instruction_cb67),
    ([0xcb, 0x68], instruction_cb68),
    ([0xcb, 0x69], instruction_cb69),
    ([0xcb, 0x6a], instruction_cb6a),
    ([0xcb, 0x6b], instruction_cb6b),
    ([0xcb, 0x6c], instruction_cb6c),
    ([0xcb, 0x6d], instruction_cb6d),
    ([0xcb, 0x6e], instruction_cb6e),
    ([0xcb, 0x6f], instruction_cb6f),
    ([0xcb, 0x70], instruction_cb70),
    ([0xcb, 0x71], instruction_cb71),
    ([0xcb, 0x72], instruction_cb72),
    ([0xcb, 0x73], instruction_cb73),
    ([0xcb, 0x74], instruction_cb74),
    ([0xcb, 0x75], instruction_cb75),
    ([0xcb, 0x76], instruction_cb76),
    ([0xcb, 0x77], instruction_cb77),
    ([0xcb, 0x78], instruction_cb78),
    ([0xcb, 0x79], instruction_cb79),
    ([0xcb, 0x7a], instruction_cb7a),
    ([0xcb, 0x7b], instruction_cb7b),
    ([0xcb, 0x7c], instruction_cb7c),
    ([0xcb, 0x7d], instruction_cb7d),
    ([0xcb, 0x7e], instruction_cb7e),
    ([0xcb, 0x7f], instruction_cb7f),
    ([0xcb, 0x80], instruction_cb80),
    ([0xcb, 0x81], instruction_cb81),
    ([0xcb, 0x82], instruction_cb82),
    ([0xcb, 0x83], instruction_cb83),
    ([0xcb, 0x84], instruction_cb84),
    ([0xcb, 0x85], instruction_cb85),
    ([0xcb, 0x86], instruction_cb86),
    ([0xcb, 0x87], instruction_cb87),
    ([0xcb, 0x88], instruction_cb88),
    ([0xcb, 0x89], instruction_cb89),
    ([0xcb, 0x8a], instruction_cb8a),
    ([0xcb, 0x8b], instruction_cb8b),
    ([0xcb, 0x8c], instruction_cb8c),
    ([0xcb, 0x8d], instruction_cb8d),
    ([0xcb, 0x8e], instruction_cb8e),
    ([0xcb, 0x8f], instruction_cb8f),
    ([0xcb, 0x90], instruction_cb90),
    ([0xcb, 0x91], instruction_cb91),
    ([0xcb, 0x92], instruction_cb92),
    ([0xcb, 0x93], instruction_cb93),
    ([0xcb, 0x94], instruction_cb94),
    ([0xcb, 0x95], instruction_cb95),
    ([0xcb, 0x96], instruction_cb96),
    ([0xcb, 0x97], instruction_cb97),
    ([0xcb, 0x98], instruction_cb98),
    ([0xcb, 0x99], instruction_cb99),
    ([0xcb, 0x9a], instruction_cb9a),
    ([0xcb, 0x9b], instruction_cb9b),
    ([0xcb, 0x9c], instruction_cb9c),
    ([0xcb, 0x9d], instruction_cb9d),
    ([0xcb, 0x9e], instruction_cb9e),
    ([0xcb, 0x9f], instruction_cb9f),
    ([0xcb, 0xa0], instruction_cba0),
    ([0xcb, 0xa1], instruction_cba1),
    ([0xcb, 0xa2], instruction_cba2),
    ([0xcb, 0xa3], instruction_cba3),
    ([0xcb, 0xa4], instruction_cba4),
    ([0xcb, 0xa5], instruction_cba5),
    ([0xcb, 0xa6], instruction_cba6),
    ([0xcb, 0xa7], instruction_cba7),
    ([0xcb, 0xa8], instruction_cba8),
    ([0xcb, 0xa9], instruction_cba9),
    ([0xcb, 0xaa], instruction_cbaa),
    ([0xcb, 0xab], instruction_cbab),
    ([0xcb, 0xac], instruction_cbac),
    ([0xcb, 0xad], instruction_cbad),
    ([0xcb, 0xae], instruction_cbae),
    ([0xcb, 0xaf], instruction_cbaf),
    ([0xcb, 0xb0], instruction_cbb0),
    ([0xcb, 0xb1], instruction_cbb1),
    ([0xcb, 0xb2], instruction_cbb2),
    ([0xcb, 0xb3], instruction_cbb3),
    ([0xcb, 0xb4], instruction_cbb4),
    ([0xcb, 0xb5], instruction_cbb5),
    ([0xcb, 0xb6], instruction_cbb6),
    ([0xcb, 0xb7], instruction_cbb7),
    ([0xcb, 0xb8], instruction_cbb8),
    ([0xcb, 0xb9], instruction_cbb9),
    ([0xcb, 0xba], instruction_cbba),
    ([0xcb, 0xbb], instruction_cbbb),
    ([0xcb, 0xbc], instruction_cbbc),
    ([0xcb, 0xbd], instruction_cbbd),
    ([0xcb, 0xbe], instruction_cbbe),
    ([0xcb, 0xbf], instruction_cbbf),
    ([0xcb, 0xc0], instruction_cbc0),
    ([0xcb, 0xc1], instruction_cbc1),
    ([0xcb, 0xc2], instruction_cbc2),
    ([0xcb, 0xc3], instruction_cbc3),
    ([0xcb, 0xc4], instruction_cbc4),
    ([0xcb, 0xc5], instruction_cbc5),
    ([0xcb, 0xc6], instruction_cbc6),
    ([0xcb, 0xc7], instruction_cbc7),
    ([0xcb, 0xc8], instruction_cbc8),
    ([0xcb, 0xc9], instruction_cbc9),
    ([0xcb, 0xca], instruction_cbca),
    ([0xcb, 0xcb], instruction_cbcb),
    ([0xcb, 0xcc], instruction_cbcc),
    ([0xcb, 0xcd], instruction_cbcd),
    ([0xcb, 0xce], instruction_cbce),
    ([0xcb, 0xcf], instruction_cbcf),
    ([0xcb, 0xd0], instruction_cbd0),
    ([0xcb, 0xd1], instruction_cbd1),
    ([0xcb, 0xd2], instruction_cbd2),
    ([0xcb, 0xd3], instruction_cbd3),
    ([0xcb, 0xd4], instruction_cbd4),
    ([0xcb, 0xd5], instruction_cbd5),
    ([0xcb, 0xd6], instruction_cbd6),
    ([0xcb, 0xd7], instruction_cbd7),
    ([0xcb, 0xd8], instruction_cbd8),
    ([0xcb, 0xd9], instruction_cbd9),
    ([0xcb, 0xda], instruction_cbda),
    ([0xcb, 0xdb], instruction_cbdb),
    ([0xcb, 0xdc], instruction_cbdc),
    ([0xcb, 0xdd], instruction_cbdd),
    ([0xcb, 0xde], instruction_cbde),
    ([0xcb, 0xdf], instruction_cbdf),
    ([0xcb, 0xe0], instruction_cbe0),
    ([0xcb, 0xe1], instruction_cbe1),
    ([0xcb, 0xe2], instruction_cbe2),
    ([0xcb, 0xe3], instruction_cbe3),
    ([0xcb, 0xe4], instruction_cbe4),
    ([0xcb, 0xe5], instruction_cbe5),
    ([0xcb, 0xe6], instruction_cbe6),
    ([0xcb, 0xe7], instruction_cbe7),
    ([0xcb, 0xe8], instruction_cbe8),
    ([0xcb, 0xe9], instruction_cbe9),
    ([0xcb, 0xea], instruction_cbea),
    ([0xcb, 0xeb], instruction_cbeb),
    ([0xcb, 0xec], instruction_cbec),
    ([0xcb, 0xed], instruction_cbed),
    ([0xcb, 0xee], instruction_cbee),
    ([0xcb, 0xef], instruction_cbef),
    ([0xcb, 0xf0], instruction_cbf0),
    ([0xcb, 0xf1], instruction_cbf1),
    ([0xcb, 0xf2], instruction_cbf2),
    ([0xcb, 0xf3], instruction_cbf3),
    ([0xcb, 0xf4], instruction_cbf4),
    ([0xcb, 0xf5], instruction_cbf5),
    ([0xcb, 0xf6], instruction_cbf6),
    ([0xcb, 0xf7], instruction_cbf7),
    ([0xcb, 0xf8], instruction_cbf8),
    ([0xcb, 0xf9], instruction_cbf9),
    ([0xcb, 0xfa], instruction_cbfa),
    ([0xcb, 0xfb], instruction_cbfb),
    ([0xcb, 0xfc], instruction_cbfc),
    ([0xcb, 0xfd], instruction_cbfd),
    ([0xcb, 0xfe], instruction_cbfe),
    ([0xcb, 0xff], instruction_cbff)
    ]