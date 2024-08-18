module [Instruction, instruction]

import Cpu.Condition exposing [Condition]
import Cpu.Register exposing [Type8, Type16]

AddressMode : [
    Immediate,
    Direct8 Type8,
    Direct16 Type16,
    Indirect [C, BC, DE, HL, HLPostDecrement, HLPostIncrement, Word8Operand, Word16Operand],
]

## https://gbdev.io/gb-opcodes/optables/

# TODO:
# 8-bit shift, rotate and bit instructions

Instruction : [
    # Other
    Unknown,
    Illegal,
    # Misc / control instructions
    Nop,
    Prefix,
    Halt,
    DisableInterrupts, # Di
    EnableInterrupts, # Ei
    # Jumps / calls
    Call Condition,
    JumpRelative Condition, # Jr
    JumpAbsolute Condition [Immediate], # Jp
    Return Condition, # Ret
    ReturnAndEnableInterrupts, # Reti
    Restart U8, # Rst
    RotateLeftCircularAccumulator, # Rlca
    RotateRightCircularAccumulator, # Rrca
    RotateLeftAccumulator, # Rla
    RotateRightAccumulator, # Rra
    DecimalAdjustAccumulator, # Daa
    ComplementAccumulator, # Cpl
    SetCarryFlag, # Scf
    ComplementCarryFlag, # Ccf
    JpIndirectHL,
    # 8-bit arithmetic / logical instructions
    Add AddressMode,
    Adc AddressMode, # Adc
    Sub AddressMode,
    Sbc AddressMode, # Sbc
    And AddressMode,
    Xor AddressMode,
    Or AddressMode,
    Compare AddressMode, # Cp
    Inc AddressMode AddressMode,
    Dec AddressMode AddressMode,
    # 16-bit arithmetic / logical instructions
    Add16 AddressMode,
    Dec16 AddressMode AddressMode,
    Inc16 AddressMode AddressMode,
    AddSPSignedImmediate,
    # 8-bit load instructions
    Load AddressMode AddressMode, # Ld
    # 16-bit load instructions
    Load16 AddressMode AddressMode, # Ld
    Push AddressMode,
    Pop AddressMode,
    LdHLSPPlusSignedImmediate,
    LdSPHL,
]

instruction : U16 -> Instruction
instruction = \byte ->
    when byte is
        0x00 -> Nop
        0x01 -> Load16 (Direct16 BC) Immediate
        0x02 -> Load (Indirect BC) (Direct8 A)
        0x03 -> Inc16 (Direct16 BC) (Direct16 BC)
        0x04 -> Inc (Direct8 B) (Direct8 B)
        0x05 -> Dec (Direct8 B) (Direct8 B)
        0x06 -> Load (Direct8 B) Immediate
        0x07 -> RotateLeftCircularAccumulator
        0x08 -> Load16 (Indirect Word16Operand) (Direct16 SP)
        0x09 -> Add16 (Direct16 BC)
        0x0A -> Load (Direct8 A) (Indirect BC)
        0x0B -> Dec16 (Direct16 BC) (Direct16 BC)
        0x0C -> Inc (Direct8 C) (Direct8 C)
        0x0D -> Dec (Direct8 C) (Direct8 C)
        0x0E -> Load (Direct8 C) Immediate
        0x0F -> RotateRightCircularAccumulator
        0x10 -> Nop # Stop
        0x11 -> Load16 (Direct16 DE) Immediate
        0x12 -> Load (Indirect DE) (Direct8 A)
        0x13 -> Inc16 (Direct16 DE) (Direct16 DE)
        0x14 -> Inc (Direct8 D) (Direct8 D)
        0x15 -> Dec (Direct8 D) (Direct8 D)
        0x16 -> Load (Direct8 D) Immediate
        0x17 -> RotateLeftAccumulator
        0x18 -> JumpRelative Always
        0x19 -> Add16 (Direct16 DE)
        0x1A -> Load (Direct8 A) (Indirect DE)
        0x1B -> Dec16 (Direct16 DE) (Direct16 DE)
        0x1C -> Inc (Direct8 E) (Direct8 E)
        0x1D -> Dec (Direct8 E) (Direct8 E)
        0x1E -> Load (Direct8 E) Immediate
        0x1F -> RotateRightAccumulator
        0x20 -> JumpRelative NotZero
        0x21 -> Load16 (Direct16 HL) Immediate
        0x22 -> Load (Indirect HLPostIncrement) (Direct8 A)
        0x23 -> Inc16 (Direct16 HL) (Direct16 HL)
        0x24 -> Inc (Direct8 H) (Direct8 H)
        0x25 -> Dec (Direct8 H) (Direct8 H)
        0x26 -> Load (Direct8 H) Immediate
        0x27 -> DecimalAdjustAccumulator
        0x28 -> JumpRelative Zero
        0x29 -> Add16 (Direct16 HL)
        0x2A -> Load (Direct8 A) (Indirect HLPostIncrement)
        0x2B -> Dec16 (Direct16 HL) (Direct16 HL)
        0x2C -> Inc (Direct8 L) (Direct8 L)
        0x2D -> Dec (Direct8 L) (Direct8 L)
        0x2E -> Load (Direct8 L) Immediate
        0x2F -> ComplementAccumulator
        0x30 -> JumpRelative NotCarry
        0x31 -> Load16 (Direct16 SP) Immediate
        0x32 -> Load (Indirect HLPostDecrement) (Direct8 A)
        0x33 -> Inc16 (Direct16 SP) (Direct16 SP)
        0x34 -> Inc (Indirect HL) (Indirect HL)
        0x35 -> Dec (Indirect HL) (Indirect HL)
        0x36 -> Load (Indirect HL) Immediate
        0x37 -> SetCarryFlag
        0x38 -> JumpRelative Carry
        0x39 -> Add16 (Direct16 SP)
        0x3A -> Load (Direct8 A) (Indirect HLPostDecrement)
        0x3B -> Dec16 (Direct16 SP) (Direct16 SP)
        0x3C -> Inc (Direct8 A) (Direct8 A)
        0x3D -> Dec (Direct8 A) (Direct8 A)
        0x3E -> Load (Direct8 A) Immediate
        0x3F -> ComplementCarryFlag
        0x40 -> Load (Direct8 B) (Direct8 B)
        0x41 -> Load (Direct8 B) (Direct8 C)
        0x42 -> Load (Direct8 B) (Direct8 D)
        0x43 -> Load (Direct8 B) (Direct8 E)
        0x44 -> Load (Direct8 B) (Direct8 H)
        0x45 -> Load (Direct8 B) (Direct8 L)
        0x46 -> Load (Direct8 B) (Indirect HL)
        0x47 -> Load (Direct8 B) (Direct8 A)
        0x48 -> Load (Direct8 C) (Direct8 B)
        0x49 -> Load (Direct8 C) (Direct8 C)
        0x4A -> Load (Direct8 C) (Direct8 D)
        0x4B -> Load (Direct8 C) (Direct8 E)
        0x4C -> Load (Direct8 C) (Direct8 H)
        0x4D -> Load (Direct8 C) (Direct8 L)
        0x4E -> Load (Direct8 C) (Indirect HL)
        0x4F -> Load (Direct8 C) (Direct8 A)
        0x50 -> Load (Direct8 D) (Direct8 B)
        0x51 -> Load (Direct8 D) (Direct8 C)
        0x52 -> Load (Direct8 D) (Direct8 D)
        0x53 -> Load (Direct8 D) (Direct8 E)
        0x54 -> Load (Direct8 D) (Direct8 H)
        0x55 -> Load (Direct8 D) (Direct8 L)
        0x56 -> Load (Direct8 D) (Indirect HL)
        0x57 -> Load (Direct8 D) (Direct8 A)
        0x58 -> Load (Direct8 E) (Direct8 B)
        0x59 -> Load (Direct8 E) (Direct8 C)
        0x5A -> Load (Direct8 E) (Direct8 D)
        0x5B -> Load (Direct8 E) (Direct8 E)
        0x5C -> Load (Direct8 E) (Direct8 H)
        0x5D -> Load (Direct8 E) (Direct8 L)
        0x5E -> Load (Direct8 E) (Indirect HL)
        0x5F -> Load (Direct8 E) (Direct8 A)
        0x60 -> Load (Direct8 H) (Direct8 B)
        0x61 -> Load (Direct8 H) (Direct8 C)
        0x62 -> Load (Direct8 H) (Direct8 D)
        0x63 -> Load (Direct8 H) (Direct8 E)
        0x64 -> Load (Direct8 H) (Direct8 H)
        0x65 -> Load (Direct8 H) (Direct8 L)
        0x66 -> Load (Direct8 H) (Indirect HL)
        0x67 -> Load (Direct8 H) (Direct8 A)
        0x68 -> Load (Direct8 L) (Direct8 B)
        0x69 -> Load (Direct8 L) (Direct8 C)
        0x6A -> Load (Direct8 L) (Direct8 D)
        0x6B -> Load (Direct8 L) (Direct8 E)
        0x6C -> Load (Direct8 L) (Direct8 H)
        0x6D -> Load (Direct8 L) (Direct8 L)
        0x6E -> Load (Direct8 L) (Indirect HL)
        0x6F -> Load (Direct8 L) (Direct8 A)
        0x70 -> Load (Indirect HL) (Direct8 B)
        0x71 -> Load (Indirect HL) (Direct8 C)
        0x72 -> Load (Indirect HL) (Direct8 D)
        0x73 -> Load (Indirect HL) (Direct8 E)
        0x74 -> Load (Indirect HL) (Direct8 H)
        0x75 -> Load (Indirect HL) (Direct8 L)
        0x76 -> Halt
        0x77 -> Load (Indirect HL) (Direct8 A)
        0x78 -> Load (Direct8 A) (Direct8 B)
        0x79 -> Load (Direct8 A) (Direct8 C)
        0x7A -> Load (Direct8 A) (Direct8 D)
        0x7B -> Load (Direct8 A) (Direct8 E)
        0x7C -> Load (Direct8 A) (Direct8 H)
        0x7D -> Load (Direct8 A) (Direct8 L)
        0x7E -> Load (Direct8 A) (Indirect HL)
        0x7F -> Load (Direct8 A) (Direct8 A)
        0x80 -> Add (Direct8 B)
        0x81 -> Add (Direct8 C)
        0x82 -> Add (Direct8 D)
        0x83 -> Add (Direct8 E)
        0x84 -> Add (Direct8 H)
        0x85 -> Add (Direct8 L)
        0x86 -> Add (Indirect HL)
        0x87 -> Add (Direct8 A)
        0x88 -> Adc (Direct8 B)
        0x89 -> Adc (Direct8 C)
        0x8A -> Adc (Direct8 D)
        0x8B -> Adc (Direct8 E)
        0x8C -> Adc (Direct8 H)
        0x8D -> Adc (Direct8 L)
        0x8E -> Adc (Indirect HL)
        0x8F -> Adc (Direct8 A)
        0x90 -> Sub (Direct8 B)
        0x91 -> Sub (Direct8 C)
        0x92 -> Sub (Direct8 D)
        0x93 -> Sub (Direct8 E)
        0x94 -> Sub (Direct8 H)
        0x95 -> Sub (Direct8 L)
        0x96 -> Sub (Indirect HL)
        0x97 -> Sub (Direct8 A)
        0x98 -> Sbc (Direct8 B)
        0x99 -> Sbc (Direct8 C)
        0x9A -> Sbc (Direct8 D)
        0x9B -> Sbc (Direct8 E)
        0x9C -> Sbc (Direct8 H)
        0x9D -> Sbc (Direct8 L)
        0x9E -> Sbc (Indirect HL)
        0x9F -> Sbc (Direct8 A)
        0xA0 -> And (Direct8 B)
        0xA1 -> And (Direct8 C)
        0xA2 -> And (Direct8 D)
        0xA3 -> And (Direct8 E)
        0xA4 -> And (Direct8 H)
        0xA5 -> And (Direct8 L)
        0xA6 -> And (Indirect HL)
        0xA7 -> And (Direct8 A)
        0xA8 -> Xor (Direct8 B)
        0xA9 -> Xor (Direct8 C)
        0xAA -> Xor (Direct8 D)
        0xAB -> Xor (Direct8 E)
        0xAC -> Xor (Direct8 H)
        0xAD -> Xor (Direct8 L)
        0xAE -> Xor (Indirect HL)
        0xAF -> Xor (Direct8 A)
        0xB0 -> Or (Direct8 B)
        0xB1 -> Or (Direct8 C)
        0xB2 -> Or (Direct8 D)
        0xB3 -> Or (Direct8 E)
        0xB4 -> Or (Direct8 H)
        0xB5 -> Or (Direct8 L)
        0xB6 -> Or (Indirect HL)
        0xB7 -> Or (Direct8 A)
        0xB8 -> Compare (Direct8 B)
        0xB9 -> Compare (Direct8 C)
        0xBA -> Compare (Direct8 D)
        0xBB -> Compare (Direct8 E)
        0xBC -> Compare (Direct8 H)
        0xBD -> Compare (Direct8 L)
        0xBE -> Compare (Indirect HL)
        0xBF -> Compare (Direct8 A)
        0xC0 -> Return NotZero
        0xC1 -> Pop (Direct16 BC)
        0xC2 -> JumpAbsolute NotZero Immediate
        0xC3 -> JumpAbsolute Always Immediate
        0xC4 -> Call NotZero
        0xC5 -> Push (Direct16 BC)
        0xC6 -> Add Immediate
        0xC7 -> Restart 0x00
        0xC8 -> Return Zero
        0xC9 -> Return Always
        0xCA -> JumpAbsolute Zero Immediate
        0xCB -> Prefix
        0xCC -> Call Zero
        0xCD -> Call Always
        0xCE -> Adc Immediate
        0xCF -> Restart 0x08
        0xD0 -> Return NotCarry
        0xD1 -> Pop (Direct16 DE)
        0xD2 -> JumpAbsolute NotCarry Immediate
        0xD3 -> Illegal
        0xD4 -> Call NotCarry
        0xD5 -> Push (Direct16 DE)
        0xD6 -> Sub Immediate
        0xD7 -> Restart 0x10
        0xD8 -> Return Carry
        0xD9 -> ReturnAndEnableInterrupts
        0xDA -> JumpAbsolute Carry Immediate
        0xDB -> Illegal
        0xDC -> Call Carry
        0xDD -> Illegal
        0xDE -> Sbc Immediate
        0xDF -> Restart 0x18
        0xE0 -> Load (Indirect Word8Operand) (Direct8 A)
        0xE1 -> Pop (Direct16 HL)
        0xE2 -> Load (Indirect C) (Direct8 A)
        0xE3 -> Illegal
        0xE4 -> Illegal
        0xE5 -> Push (Direct16 HL)
        0xE6 -> And Immediate
        0xE7 -> Restart 0x20
        0xE8 -> AddSPSignedImmediate
        0xE9 -> JpIndirectHL
        0xEA -> Load (Indirect Word16Operand) (Direct8 A)
        0xEB -> Illegal
        0xEC -> Illegal
        0xED -> Illegal
        0xEE -> Xor Immediate
        0xEF -> Restart 0x28
        0xF0 -> Load (Direct8 A) (Indirect Word8Operand)
        0xF1 -> Pop (Direct16 AF)
        0xF2 -> Load (Direct8 A) (Indirect C)
        0xF3 -> DisableInterrupts
        0xF4 -> Illegal
        0xF5 -> Push (Direct16 AF)
        0xF6 -> Or Immediate
        0xF7 -> Restart 0x30
        0xF8 -> LdHLSPPlusSignedImmediate
        0xF9 -> LdSPHL
        0xFA -> Load (Direct8 A) (Indirect Word16Operand)
        0xFB -> EnableInterrupts
        0xFC -> Illegal
        0xFD -> Illegal
        0xFE -> Compare Immediate
        0xFF -> Restart 0x38
        _ -> Unknown
