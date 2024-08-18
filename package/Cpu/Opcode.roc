module [Instruction, instruction]

import Cpu.Condition exposing [Condition]

T8 : [A, B, C, D, E, H, L]
T16 : [AF, BC, DE, HL, SP]

# Address modes
RW8 : [Immediate, Indirect [HL], Register T8]
RW16 : [Register T16]
RWLd : [Immediate, Indirect [BC, C, DE, HL, HLPostDecrement, HLPostIncrement, Word16Operand, Word8Operand], Register T8]
RWLd16 : [Immediate, Indirect [Word16Operand], Register T16]

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
    Add RW8,
    Adc RW8, # Adc
    Sub RW8,
    Sbc RW8, # Sbc
    And RW8,
    Xor RW8,
    Or RW8,
    Compare RW8, # Cp
    Inc RW8 RW8,
    Dec RW8 RW8,
    # 16-bit arithmetic / logical instructions
    Add16 RW16,
    Dec16 RW16 RW16,
    Inc16 RW16 RW16,
    AddSPSignedImmediate,
    # 8-bit load instructions
    Load RWLd RWLd, # Ld
    # 16-bit load instructions
    Load16 RWLd16 RWLd16, # Ld
    Push RW16,
    Pop RW16,
    LdHLSPPlusSignedImmediate,
    LdSPHL,
]

instruction : U16 -> Instruction
instruction = \byte ->
    when byte is
        0x00 -> Nop
        0x01 -> Load16 (Register BC) Immediate
        0x02 -> Load (Indirect BC) (Register A)
        0x03 -> Inc16 (Register BC) (Register BC)
        0x04 -> Inc (Register B) (Register B)
        0x05 -> Dec (Register B) (Register B)
        0x06 -> Load (Register B) Immediate
        0x07 -> RotateLeftCircularAccumulator
        0x08 -> Load16 (Indirect Word16Operand) (Register SP)
        0x09 -> Add16 (Register BC)
        0x0A -> Load (Register A) (Indirect BC)
        0x0B -> Dec16 (Register BC) (Register BC)
        0x0C -> Inc (Register C) (Register C)
        0x0D -> Dec (Register C) (Register C)
        0x0E -> Load (Register C) Immediate
        0x0F -> RotateRightCircularAccumulator
        0x10 -> Nop # Stop
        0x11 -> Load16 (Register DE) Immediate
        0x12 -> Load (Indirect DE) (Register A)
        0x13 -> Inc16 (Register DE) (Register DE)
        0x14 -> Inc (Register D) (Register D)
        0x15 -> Dec (Register D) (Register D)
        0x16 -> Load (Register D) Immediate
        0x17 -> RotateLeftAccumulator
        0x18 -> JumpRelative Always
        0x19 -> Add16 (Register DE)
        0x1A -> Load (Register A) (Indirect DE)
        0x1B -> Dec16 (Register DE) (Register DE)
        0x1C -> Inc (Register E) (Register E)
        0x1D -> Dec (Register E) (Register E)
        0x1E -> Load (Register E) Immediate
        0x1F -> RotateRightAccumulator
        0x20 -> JumpRelative NotZero
        0x21 -> Load16 (Register HL) Immediate
        0x22 -> Load (Indirect HLPostIncrement) (Register A)
        0x23 -> Inc16 (Register HL) (Register HL)
        0x24 -> Inc (Register H) (Register H)
        0x25 -> Dec (Register H) (Register H)
        0x26 -> Load (Register H) Immediate
        0x27 -> DecimalAdjustAccumulator
        0x28 -> JumpRelative Zero
        0x29 -> Add16 (Register HL)
        0x2A -> Load (Register A) (Indirect HLPostIncrement)
        0x2B -> Dec16 (Register HL) (Register HL)
        0x2C -> Inc (Register L) (Register L)
        0x2D -> Dec (Register L) (Register L)
        0x2E -> Load (Register L) Immediate
        0x2F -> ComplementAccumulator
        0x30 -> JumpRelative NotCarry
        0x31 -> Load16 (Register SP) Immediate
        0x32 -> Load (Indirect HLPostDecrement) (Register A)
        0x33 -> Inc16 (Register SP) (Register SP)
        0x34 -> Inc (Indirect HL) (Indirect HL)
        0x35 -> Dec (Indirect HL) (Indirect HL)
        0x36 -> Load (Indirect HL) Immediate
        0x37 -> SetCarryFlag
        0x38 -> JumpRelative Carry
        0x39 -> Add16 (Register SP)
        0x3A -> Load (Register A) (Indirect HLPostDecrement)
        0x3B -> Dec16 (Register SP) (Register SP)
        0x3C -> Inc (Register A) (Register A)
        0x3D -> Dec (Register A) (Register A)
        0x3E -> Load (Register A) Immediate
        0x3F -> ComplementCarryFlag
        0x40 -> Load (Register B) (Register B)
        0x41 -> Load (Register B) (Register C)
        0x42 -> Load (Register B) (Register D)
        0x43 -> Load (Register B) (Register E)
        0x44 -> Load (Register B) (Register H)
        0x45 -> Load (Register B) (Register L)
        0x46 -> Load (Register B) (Indirect HL)
        0x47 -> Load (Register B) (Register A)
        0x48 -> Load (Register C) (Register B)
        0x49 -> Load (Register C) (Register C)
        0x4A -> Load (Register C) (Register D)
        0x4B -> Load (Register C) (Register E)
        0x4C -> Load (Register C) (Register H)
        0x4D -> Load (Register C) (Register L)
        0x4E -> Load (Register C) (Indirect HL)
        0x4F -> Load (Register C) (Register A)
        0x50 -> Load (Register D) (Register B)
        0x51 -> Load (Register D) (Register C)
        0x52 -> Load (Register D) (Register D)
        0x53 -> Load (Register D) (Register E)
        0x54 -> Load (Register D) (Register H)
        0x55 -> Load (Register D) (Register L)
        0x56 -> Load (Register D) (Indirect HL)
        0x57 -> Load (Register D) (Register A)
        0x58 -> Load (Register E) (Register B)
        0x59 -> Load (Register E) (Register C)
        0x5A -> Load (Register E) (Register D)
        0x5B -> Load (Register E) (Register E)
        0x5C -> Load (Register E) (Register H)
        0x5D -> Load (Register E) (Register L)
        0x5E -> Load (Register E) (Indirect HL)
        0x5F -> Load (Register E) (Register A)
        0x60 -> Load (Register H) (Register B)
        0x61 -> Load (Register H) (Register C)
        0x62 -> Load (Register H) (Register D)
        0x63 -> Load (Register H) (Register E)
        0x64 -> Load (Register H) (Register H)
        0x65 -> Load (Register H) (Register L)
        0x66 -> Load (Register H) (Indirect HL)
        0x67 -> Load (Register H) (Register A)
        0x68 -> Load (Register L) (Register B)
        0x69 -> Load (Register L) (Register C)
        0x6A -> Load (Register L) (Register D)
        0x6B -> Load (Register L) (Register E)
        0x6C -> Load (Register L) (Register H)
        0x6D -> Load (Register L) (Register L)
        0x6E -> Load (Register L) (Indirect HL)
        0x6F -> Load (Register L) (Register A)
        0x70 -> Load (Indirect HL) (Register B)
        0x71 -> Load (Indirect HL) (Register C)
        0x72 -> Load (Indirect HL) (Register D)
        0x73 -> Load (Indirect HL) (Register E)
        0x74 -> Load (Indirect HL) (Register H)
        0x75 -> Load (Indirect HL) (Register L)
        0x76 -> Halt
        0x77 -> Load (Indirect HL) (Register A)
        0x78 -> Load (Register A) (Register B)
        0x79 -> Load (Register A) (Register C)
        0x7A -> Load (Register A) (Register D)
        0x7B -> Load (Register A) (Register E)
        0x7C -> Load (Register A) (Register H)
        0x7D -> Load (Register A) (Register L)
        0x7E -> Load (Register A) (Indirect HL)
        0x7F -> Load (Register A) (Register A)
        0x80 -> Add (Register B)
        0x81 -> Add (Register C)
        0x82 -> Add (Register D)
        0x83 -> Add (Register E)
        0x84 -> Add (Register H)
        0x85 -> Add (Register L)
        0x86 -> Add (Indirect HL)
        0x87 -> Add (Register A)
        0x88 -> Adc (Register B)
        0x89 -> Adc (Register C)
        0x8A -> Adc (Register D)
        0x8B -> Adc (Register E)
        0x8C -> Adc (Register H)
        0x8D -> Adc (Register L)
        0x8E -> Adc (Indirect HL)
        0x8F -> Adc (Register A)
        0x90 -> Sub (Register B)
        0x91 -> Sub (Register C)
        0x92 -> Sub (Register D)
        0x93 -> Sub (Register E)
        0x94 -> Sub (Register H)
        0x95 -> Sub (Register L)
        0x96 -> Sub (Indirect HL)
        0x97 -> Sub (Register A)
        0x98 -> Sbc (Register B)
        0x99 -> Sbc (Register C)
        0x9A -> Sbc (Register D)
        0x9B -> Sbc (Register E)
        0x9C -> Sbc (Register H)
        0x9D -> Sbc (Register L)
        0x9E -> Sbc (Indirect HL)
        0x9F -> Sbc (Register A)
        0xA0 -> And (Register B)
        0xA1 -> And (Register C)
        0xA2 -> And (Register D)
        0xA3 -> And (Register E)
        0xA4 -> And (Register H)
        0xA5 -> And (Register L)
        0xA6 -> And (Indirect HL)
        0xA7 -> And (Register A)
        0xA8 -> Xor (Register B)
        0xA9 -> Xor (Register C)
        0xAA -> Xor (Register D)
        0xAB -> Xor (Register E)
        0xAC -> Xor (Register H)
        0xAD -> Xor (Register L)
        0xAE -> Xor (Indirect HL)
        0xAF -> Xor (Register A)
        0xB0 -> Or (Register B)
        0xB1 -> Or (Register C)
        0xB2 -> Or (Register D)
        0xB3 -> Or (Register E)
        0xB4 -> Or (Register H)
        0xB5 -> Or (Register L)
        0xB6 -> Or (Indirect HL)
        0xB7 -> Or (Register A)
        0xB8 -> Compare (Register B)
        0xB9 -> Compare (Register C)
        0xBA -> Compare (Register D)
        0xBB -> Compare (Register E)
        0xBC -> Compare (Register H)
        0xBD -> Compare (Register L)
        0xBE -> Compare (Indirect HL)
        0xBF -> Compare (Register A)
        0xC0 -> Return NotZero
        0xC1 -> Pop (Register BC)
        0xC2 -> JumpAbsolute NotZero Immediate
        0xC3 -> JumpAbsolute Always Immediate
        0xC4 -> Call NotZero
        0xC5 -> Push (Register BC)
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
        0xD1 -> Pop (Register DE)
        0xD2 -> JumpAbsolute NotCarry Immediate
        0xD3 -> Illegal
        0xD4 -> Call NotCarry
        0xD5 -> Push (Register DE)
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
        0xE0 -> Load (Indirect Word8Operand) (Register A)
        0xE1 -> Pop (Register HL)
        0xE2 -> Load (Indirect C) (Register A)
        0xE3 -> Illegal
        0xE4 -> Illegal
        0xE5 -> Push (Register HL)
        0xE6 -> And Immediate
        0xE7 -> Restart 0x20
        0xE8 -> AddSPSignedImmediate
        0xE9 -> JpIndirectHL
        0xEA -> Load (Indirect Word16Operand) (Register A)
        0xEB -> Illegal
        0xEC -> Illegal
        0xED -> Illegal
        0xEE -> Xor Immediate
        0xEF -> Restart 0x28
        0xF0 -> Load (Register A) (Indirect Word8Operand)
        0xF1 -> Pop (Register AF)
        0xF2 -> Load (Register A) (Indirect C)
        0xF3 -> DisableInterrupts
        0xF4 -> Illegal
        0xF5 -> Push (Register AF)
        0xF6 -> Or Immediate
        0xF7 -> Restart 0x30
        0xF8 -> LdHLSPPlusSignedImmediate
        0xF9 -> LdSPHL
        0xFA -> Load (Register A) (Indirect Word16Operand)
        0xFB -> EnableInterrupts
        0xFC -> Illegal
        0xFD -> Illegal
        0xFE -> Compare Immediate
        0xFF -> Restart 0x38
        _ -> Unknown
