## Gameboy CPU (LR35902) instruction set
# https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
# https://gbdev.io/gb-opcodes/optables/
module [Instruction, lookup]

import Cpu.Register exposing [Type8, Type16]
import Cpu.Register.Status exposing [Member]

Condition : [Always, Status Member Bool]

checkCondition : Condition, U8 -> Bool
checkCondition = \condition, flag ->
    when condition is
        Always -> Bool.true
        Status member value -> Cpu.Register.Status.check member flag == value

AddressingMode : [
    Immediate,
    Direct8 Type8,
    Direct16 Type16,
    Indirect [C, BC, DE, HL, HLPostDecrement, HLPostIncrement, Word8Operand, Word16Operand],
]

Instruction : [
    # Other
    Prefix,
    Illegal,
    Unknown,
    # Misc / control instructions
    Nop,
    Halt,
    Interrupts [Enable, Disable], # (Ei, Di)
    CarryFlag [Set, Complement], # (Scf, Ccf) # Set or toggle the carry flag
    DecimalAdjustAccumulator, # Daa
    ComplementAccumulator, # Cpl # Flip all bits from the flags register
    # Jumps / calls
    Jump Condition [Immediate], # Jp
    Branch Condition, # Jr (I renamed jump relative to branch)
    Call Condition,
    Return Condition, # Ret
    ReturnAndEnableInterrupts, # Reti
    Restart U8, # Rst
    # 8-bit shift, rotate and bit
    RotateCircularAccumulator [Left, Right], # Rlca & Rrca
    RotateAccumulator [Left, Right], # Rla & Rra
    # 8-bit arithmetic / logical
    Or AddressingMode,
    And AddressingMode,
    Xor AddressingMode,
    Add AddressingMode,
    Adc AddressingMode,
    Sub AddressingMode,
    Sbc AddressingMode,
    Compare AddressingMode, # Cp
    Inc AddressingMode AddressingMode,
    Dec AddressingMode AddressingMode,
    # 16-bit arithmetic / logical
    Add16 AddressingMode,
    Dec16 AddressingMode AddressingMode,
    Inc16 AddressingMode AddressingMode,
    # 8-bit load
    Load AddressingMode AddressingMode, # Ld
    # 16-bit load
    Load16 AddressingMode AddressingMode, # Ld
    # Stack
    Push AddressingMode,
    Pop AddressingMode,
    # TODO
    AddSPSignedImmediate,
    LdHLSPPlusSignedImmediate,
    LdSPHL,
    JpIndirectHL,
]

lookup : U8 -> Instruction
lookup = \byte ->
    when byte is
        0x00 -> Nop
        0x10 -> Nop # Stop
        0x76 -> Halt
        0xE6 -> And Immediate
        0xA0 -> And (Direct8 B)
        0xA1 -> And (Direct8 C)
        0xA2 -> And (Direct8 D)
        0xA3 -> And (Direct8 E)
        0xA4 -> And (Direct8 H)
        0xA5 -> And (Direct8 L)
        0xA6 -> And (Indirect HL)
        0xA7 -> And (Direct8 A)
        0xEE -> Xor Immediate
        0xA8 -> Xor (Direct8 B)
        0xA9 -> Xor (Direct8 C)
        0xAA -> Xor (Direct8 D)
        0xAB -> Xor (Direct8 E)
        0xAC -> Xor (Direct8 H)
        0xAD -> Xor (Direct8 L)
        0xAE -> Xor (Indirect HL)
        0xAF -> Xor (Direct8 A)
        0xF6 -> Or Immediate
        0xB0 -> Or (Direct8 B)
        0xB1 -> Or (Direct8 C)
        0xB2 -> Or (Direct8 D)
        0xB3 -> Or (Direct8 E)
        0xB4 -> Or (Direct8 H)
        0xB5 -> Or (Direct8 L)
        0xB6 -> Or (Indirect HL)
        0xB7 -> Or (Direct8 A)
        0xC6 -> Add Immediate
        0x80 -> Add (Direct8 B)
        0x81 -> Add (Direct8 C)
        0x82 -> Add (Direct8 D)
        0x83 -> Add (Direct8 E)
        0x84 -> Add (Direct8 H)
        0x85 -> Add (Direct8 L)
        0x86 -> Add (Indirect HL)
        0x87 -> Add (Direct8 A)
        0xCE -> Adc Immediate
        0x88 -> Adc (Direct8 B)
        0x89 -> Adc (Direct8 C)
        0x8A -> Adc (Direct8 D)
        0x8B -> Adc (Direct8 E)
        0x8C -> Adc (Direct8 H)
        0x8D -> Adc (Direct8 L)
        0x8E -> Adc (Indirect HL)
        0x8F -> Adc (Direct8 A)
        0xD6 -> Sub Immediate
        0x90 -> Sub (Direct8 B)
        0x91 -> Sub (Direct8 C)
        0x92 -> Sub (Direct8 D)
        0x93 -> Sub (Direct8 E)
        0x94 -> Sub (Direct8 H)
        0x95 -> Sub (Direct8 L)
        0x96 -> Sub (Indirect HL)
        0x97 -> Sub (Direct8 A)
        0xDE -> Sbc Immediate
        0x98 -> Sbc (Direct8 B)
        0x99 -> Sbc (Direct8 C)
        0x9A -> Sbc (Direct8 D)
        0x9B -> Sbc (Direct8 E)
        0x9C -> Sbc (Direct8 H)
        0x9D -> Sbc (Direct8 L)
        0x9E -> Sbc (Indirect HL)
        0x9F -> Sbc (Direct8 A)
        0xFE -> Compare Immediate
        0xB8 -> Compare (Direct8 B)
        0xB9 -> Compare (Direct8 C)
        0xBA -> Compare (Direct8 D)
        0xBB -> Compare (Direct8 E)
        0xBC -> Compare (Direct8 H)
        0xBD -> Compare (Direct8 L)
        0xBE -> Compare (Indirect HL)
        0xBF -> Compare (Direct8 A)
        0x04 -> Inc (Direct8 B) (Direct8 B)
        0x0C -> Inc (Direct8 C) (Direct8 C)
        0x14 -> Inc (Direct8 D) (Direct8 D)
        0x1C -> Inc (Direct8 E) (Direct8 E)
        0x24 -> Inc (Direct8 H) (Direct8 H)
        0x2C -> Inc (Direct8 L) (Direct8 L)
        0x34 -> Inc (Indirect HL) (Indirect HL)
        0x3C -> Inc (Direct8 A) (Direct8 A)
        0x05 -> Dec (Direct8 B) (Direct8 B)
        0x0D -> Dec (Direct8 C) (Direct8 C)
        0x15 -> Dec (Direct8 D) (Direct8 D)
        0x1D -> Dec (Direct8 E) (Direct8 E)
        0x25 -> Dec (Direct8 H) (Direct8 H)
        0x2D -> Dec (Direct8 L) (Direct8 L)
        0x35 -> Dec (Indirect HL) (Indirect HL)
        0x3D -> Dec (Direct8 A) (Direct8 A)
        0x09 -> Add16 (Direct16 BC)
        0x19 -> Add16 (Direct16 DE)
        0x29 -> Add16 (Direct16 HL)
        0x39 -> Add16 (Direct16 SP)
        0x03 -> Inc16 (Direct16 BC) (Direct16 BC)
        0x13 -> Inc16 (Direct16 DE) (Direct16 DE)
        0x23 -> Inc16 (Direct16 HL) (Direct16 HL)
        0x33 -> Inc16 (Direct16 SP) (Direct16 SP)
        0x0B -> Dec16 (Direct16 BC) (Direct16 BC)
        0x1B -> Dec16 (Direct16 DE) (Direct16 DE)
        0x2B -> Dec16 (Direct16 HL) (Direct16 HL)
        0x3B -> Dec16 (Direct16 SP) (Direct16 SP)
        0x07 -> RotateCircularAccumulator Left
        0x0F -> RotateCircularAccumulator Right
        0x17 -> RotateAccumulator Left
        0x1F -> RotateAccumulator Right
        0x02 -> Load (Indirect BC) (Direct8 A)
        0x06 -> Load (Direct8 B) Immediate
        0x0A -> Load (Direct8 A) (Indirect BC)
        0x0E -> Load (Direct8 C) Immediate
        0x12 -> Load (Indirect DE) (Direct8 A)
        0x16 -> Load (Direct8 D) Immediate
        0x1A -> Load (Direct8 A) (Indirect DE)
        0x1E -> Load (Direct8 E) Immediate
        0x22 -> Load (Indirect HLPostIncrement) (Direct8 A)
        0x26 -> Load (Direct8 H) Immediate
        0x2A -> Load (Direct8 A) (Indirect HLPostIncrement)
        0x2E -> Load (Direct8 L) Immediate
        0x32 -> Load (Indirect HLPostDecrement) (Direct8 A)
        0x36 -> Load (Indirect HL) Immediate
        0x3A -> Load (Direct8 A) (Indirect HLPostDecrement)
        0x3E -> Load (Direct8 A) Immediate
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
        0x77 -> Load (Indirect HL) (Direct8 A)
        0x78 -> Load (Direct8 A) (Direct8 B)
        0x79 -> Load (Direct8 A) (Direct8 C)
        0x7A -> Load (Direct8 A) (Direct8 D)
        0x7B -> Load (Direct8 A) (Direct8 E)
        0x7C -> Load (Direct8 A) (Direct8 H)
        0x7D -> Load (Direct8 A) (Direct8 L)
        0x7E -> Load (Direct8 A) (Indirect HL)
        0x7F -> Load (Direct8 A) (Direct8 A)
        0xE2 -> Load (Indirect C) (Direct8 A)
        0xEA -> Load (Indirect Word16Operand) (Direct8 A)
        0xF0 -> Load (Direct8 A) (Indirect Word8Operand)
        0xF2 -> Load (Direct8 A) (Indirect C)
        0xFA -> Load (Direct8 A) (Indirect Word16Operand)
        0xE0 -> Load (Indirect Word8Operand) (Direct8 A)
        0x01 -> Load16 (Direct16 BC) Immediate
        0x08 -> Load16 (Indirect Word16Operand) (Direct16 SP)
        0x11 -> Load16 (Direct16 DE) Immediate
        0x21 -> Load16 (Direct16 HL) Immediate
        0x31 -> Load16 (Direct16 SP) Immediate
        0xC3 -> Jump Always Immediate
        0xC2 -> Jump (Status Zero Bool.false) Immediate
        0xCA -> Jump (Status Zero Bool.true) Immediate
        0xD2 -> Jump (Status Carry Bool.false) Immediate
        0xDA -> Jump (Status Carry Bool.true) Immediate
        0x18 -> Branch Always
        0x20 -> Branch (Status Zero Bool.false)
        0x28 -> Branch (Status Zero Bool.true)
        0x30 -> Branch (Status Carry Bool.false)
        0x38 -> Branch (Status Carry Bool.true)
        0xCD -> Call Always
        0xC4 -> Call (Status Zero Bool.false)
        0xCC -> Call (Status Zero Bool.true)
        0xD4 -> Call (Status Carry Bool.false)
        0xDC -> Call (Status Carry Bool.true)
        0xC9 -> Return Always
        0xC0 -> Return (Status Zero Bool.false)
        0xC8 -> Return (Status Zero Bool.true)
        0xD0 -> Return (Status Carry Bool.false)
        0xD8 -> Return (Status Carry Bool.true)
        0xD9 -> ReturnAndEnableInterrupts
        0xC1 -> Pop (Direct16 BC)
        0xD1 -> Pop (Direct16 DE)
        0xE1 -> Pop (Direct16 HL)
        0xF1 -> Pop (Direct16 AF)
        0xC5 -> Push (Direct16 BC)
        0xD5 -> Push (Direct16 DE)
        0xE5 -> Push (Direct16 HL)
        0xF5 -> Push (Direct16 AF)
        0xCB -> Prefix
        0xF3 -> Interrupts Disable
        0xFB -> Interrupts Enable
        0x27 -> DecimalAdjustAccumulator
        0x2F -> ComplementAccumulator
        0x37 -> CarryFlag Set
        0x3F -> CarryFlag Complement
        0xC7 -> Restart 0x00
        0xCF -> Restart 0x08
        0xD7 -> Restart 0x10
        0xDF -> Restart 0x18
        0xE7 -> Restart 0x20
        0xEF -> Restart 0x28
        0xF7 -> Restart 0x30
        0xFF -> Restart 0x38
        # TODO
        0xE8 -> AddSPSignedImmediate
        0xF8 -> LdHLSPPlusSignedImmediate
        0xF9 -> LdSPHL
        0xE9 -> JpIndirectHL
        0xD3 -> Illegal
        0xDB -> Illegal
        0xDD -> Illegal
        0xE3 -> Illegal
        0xE4 -> Illegal
        0xEB -> Illegal
        0xEC -> Illegal
        0xED -> Illegal
        0xF4 -> Illegal
        0xFC -> Illegal
        0xFD -> Illegal
        _ -> Unknown

# 8-bit shift, rotate and bit instructions
Prefixed : [
    Bit U8 AddressingMode,
    Set U8 AddressingMode AddressingMode,
    Reset U8 AddressingMode AddressingMode, # Res
    Rotate [Left, Right] AddressingMode AddressingMode, # Rl & Rr
    RotateCircular [Left, Right] AddressingMode AddressingMode, # Rlc & Rrc
    ShiftArithmetic [Left, Right] AddressingMode AddressingMode, # Sla & Sra
    ShiftLogical [Right] AddressingMode AddressingMode, # Srl
    Swap AddressingMode AddressingMode,
    Unknown,
]

prefixed : U8 -> Prefixed
prefixed = \byte ->
    when byte is
        0x00 -> RotateCircular Left (Direct8 B) (Direct8 B)
        0x01 -> RotateCircular Left (Direct8 C) (Direct8 C)
        0x02 -> RotateCircular Left (Direct8 D) (Direct8 D)
        0x03 -> RotateCircular Left (Direct8 E) (Direct8 E)
        0x04 -> RotateCircular Left (Direct8 H) (Direct8 H)
        0x05 -> RotateCircular Left (Direct8 L) (Direct8 L)
        0x06 -> RotateCircular Left (Indirect HL) (Indirect HL)
        0x07 -> RotateCircular Left (Direct8 A) (Direct8 A)
        0x08 -> RotateCircular Right (Direct8 B) (Direct8 B)
        0x09 -> RotateCircular Right (Direct8 C) (Direct8 C)
        0x0A -> RotateCircular Right (Direct8 D) (Direct8 D)
        0x0B -> RotateCircular Right (Direct8 E) (Direct8 E)
        0x0C -> RotateCircular Right (Direct8 H) (Direct8 H)
        0x0D -> RotateCircular Right (Direct8 L) (Direct8 L)
        0x0E -> RotateCircular Right (Indirect HL) (Indirect HL)
        0x0F -> RotateCircular Right (Direct8 A) (Direct8 A)
        0x10 -> Rotate Left (Direct8 B) (Direct8 B)
        0x11 -> Rotate Left (Direct8 C) (Direct8 C)
        0x12 -> Rotate Left (Direct8 D) (Direct8 D)
        0x13 -> Rotate Left (Direct8 E) (Direct8 E)
        0x14 -> Rotate Left (Direct8 H) (Direct8 H)
        0x15 -> Rotate Left (Direct8 L) (Direct8 L)
        0x16 -> Rotate Left (Indirect HL) (Indirect HL)
        0x17 -> Rotate Left (Direct8 A) (Direct8 A)
        0x18 -> Rotate Right (Direct8 B) (Direct8 B)
        0x19 -> Rotate Right (Direct8 C) (Direct8 C)
        0x1A -> Rotate Right (Direct8 D) (Direct8 D)
        0x1B -> Rotate Right (Direct8 E) (Direct8 E)
        0x1C -> Rotate Right (Direct8 H) (Direct8 H)
        0x1D -> Rotate Right (Direct8 L) (Direct8 L)
        0x1E -> Rotate Right (Indirect HL) (Indirect HL)
        0x1F -> Rotate Right (Direct8 A) (Direct8 A)
        0x20 -> ShiftArithmetic Left (Direct8 B) (Direct8 B)
        0x21 -> ShiftArithmetic Left (Direct8 C) (Direct8 C)
        0x22 -> ShiftArithmetic Left (Direct8 D) (Direct8 D)
        0x23 -> ShiftArithmetic Left (Direct8 E) (Direct8 E)
        0x24 -> ShiftArithmetic Left (Direct8 H) (Direct8 H)
        0x25 -> ShiftArithmetic Left (Direct8 L) (Direct8 L)
        0x26 -> ShiftArithmetic Left (Indirect HL) (Indirect HL)
        0x27 -> ShiftArithmetic Left (Direct8 A) (Direct8 A)
        0x28 -> ShiftArithmetic Right (Direct8 B) (Direct8 B)
        0x29 -> ShiftArithmetic Right (Direct8 C) (Direct8 C)
        0x2A -> ShiftArithmetic Right (Direct8 D) (Direct8 D)
        0x2B -> ShiftArithmetic Right (Direct8 E) (Direct8 E)
        0x2C -> ShiftArithmetic Right (Direct8 H) (Direct8 H)
        0x2D -> ShiftArithmetic Right (Direct8 L) (Direct8 L)
        0x2E -> ShiftArithmetic Right (Indirect HL) (Indirect HL)
        0x2F -> ShiftArithmetic Right (Direct8 A) (Direct8 A)
        0x30 -> Swap (Direct8 B) (Direct8 B)
        0x31 -> Swap (Direct8 C) (Direct8 C)
        0x32 -> Swap (Direct8 D) (Direct8 D)
        0x33 -> Swap (Direct8 E) (Direct8 E)
        0x34 -> Swap (Direct8 H) (Direct8 H)
        0x35 -> Swap (Direct8 L) (Direct8 L)
        0x36 -> Swap (Indirect HL) (Indirect HL)
        0x37 -> Swap (Direct8 A) (Direct8 A)
        0x38 -> ShiftLogical Right (Direct8 B) (Direct8 B)
        0x39 -> ShiftLogical Right (Direct8 C) (Direct8 C)
        0x3A -> ShiftLogical Right (Direct8 D) (Direct8 D)
        0x3B -> ShiftLogical Right (Direct8 E) (Direct8 E)
        0x3C -> ShiftLogical Right (Direct8 H) (Direct8 H)
        0x3D -> ShiftLogical Right (Direct8 L) (Direct8 L)
        0x3E -> ShiftLogical Right (Indirect HL) (Indirect HL)
        0x3F -> ShiftLogical Right (Direct8 A) (Direct8 A)
        0x40 -> Bit 0 (Direct8 B)
        0x41 -> Bit 0 (Direct8 C)
        0x42 -> Bit 0 (Direct8 D)
        0x43 -> Bit 0 (Direct8 E)
        0x44 -> Bit 0 (Direct8 H)
        0x45 -> Bit 0 (Direct8 L)
        0x46 -> Bit 0 (Indirect HL)
        0x47 -> Bit 0 (Direct8 A)
        0x48 -> Bit 1 (Direct8 B)
        0x49 -> Bit 1 (Direct8 C)
        0x4A -> Bit 1 (Direct8 D)
        0x4B -> Bit 1 (Direct8 E)
        0x4C -> Bit 1 (Direct8 H)
        0x4D -> Bit 1 (Direct8 L)
        0x4E -> Bit 1 (Indirect HL)
        0x4F -> Bit 1 (Direct8 A)
        0x50 -> Bit 2 (Direct8 B)
        0x51 -> Bit 2 (Direct8 C)
        0x52 -> Bit 2 (Direct8 D)
        0x53 -> Bit 2 (Direct8 E)
        0x54 -> Bit 2 (Direct8 H)
        0x55 -> Bit 2 (Direct8 L)
        0x56 -> Bit 2 (Indirect HL)
        0x57 -> Bit 2 (Direct8 A)
        0x58 -> Bit 3 (Direct8 B)
        0x59 -> Bit 3 (Direct8 C)
        0x5A -> Bit 3 (Direct8 D)
        0x5B -> Bit 3 (Direct8 E)
        0x5C -> Bit 3 (Direct8 H)
        0x5D -> Bit 3 (Direct8 L)
        0x5E -> Bit 3 (Indirect HL)
        0x5F -> Bit 3 (Direct8 A)
        0x60 -> Bit 4 (Direct8 B)
        0x61 -> Bit 4 (Direct8 C)
        0x62 -> Bit 4 (Direct8 D)
        0x63 -> Bit 4 (Direct8 E)
        0x64 -> Bit 4 (Direct8 H)
        0x65 -> Bit 4 (Direct8 L)
        0x66 -> Bit 4 (Indirect HL)
        0x67 -> Bit 4 (Direct8 A)
        0x68 -> Bit 5 (Direct8 B)
        0x69 -> Bit 5 (Direct8 C)
        0x6A -> Bit 5 (Direct8 D)
        0x6B -> Bit 5 (Direct8 E)
        0x6C -> Bit 5 (Direct8 H)
        0x6D -> Bit 5 (Direct8 L)
        0x6E -> Bit 5 (Indirect HL)
        0x6F -> Bit 5 (Direct8 A)
        0x70 -> Bit 6 (Direct8 B)
        0x71 -> Bit 6 (Direct8 C)
        0x72 -> Bit 6 (Direct8 D)
        0x73 -> Bit 6 (Direct8 E)
        0x74 -> Bit 6 (Direct8 H)
        0x75 -> Bit 6 (Direct8 L)
        0x76 -> Bit 6 (Indirect HL)
        0x77 -> Bit 6 (Direct8 A)
        0x78 -> Bit 7 (Direct8 B)
        0x79 -> Bit 7 (Direct8 C)
        0x7A -> Bit 7 (Direct8 D)
        0x7B -> Bit 7 (Direct8 E)
        0x7C -> Bit 7 (Direct8 H)
        0x7D -> Bit 7 (Direct8 L)
        0x7E -> Bit 7 (Indirect HL)
        0x7F -> Bit 7 (Direct8 A)
        0x80 -> Reset 0 (Direct8 B) (Direct8 B)
        0x81 -> Reset 0 (Direct8 C) (Direct8 C)
        0x82 -> Reset 0 (Direct8 D) (Direct8 D)
        0x83 -> Reset 0 (Direct8 E) (Direct8 E)
        0x84 -> Reset 0 (Direct8 H) (Direct8 H)
        0x85 -> Reset 0 (Direct8 L) (Direct8 L)
        0x86 -> Reset 0 (Indirect HL) (Indirect HL)
        0x87 -> Reset 0 (Direct8 A) (Direct8 A)
        0x88 -> Reset 1 (Direct8 B) (Direct8 B)
        0x89 -> Reset 1 (Direct8 C) (Direct8 C)
        0x8A -> Reset 1 (Direct8 D) (Direct8 D)
        0x8B -> Reset 1 (Direct8 E) (Direct8 E)
        0x8C -> Reset 1 (Direct8 H) (Direct8 H)
        0x8D -> Reset 1 (Direct8 L) (Direct8 L)
        0x8E -> Reset 1 (Indirect HL) (Indirect HL)
        0x8F -> Reset 1 (Direct8 A) (Direct8 A)
        0x90 -> Reset 2 (Direct8 B) (Direct8 B)
        0x91 -> Reset 2 (Direct8 C) (Direct8 C)
        0x92 -> Reset 2 (Direct8 D) (Direct8 D)
        0x93 -> Reset 2 (Direct8 E) (Direct8 E)
        0x94 -> Reset 2 (Direct8 H) (Direct8 H)
        0x95 -> Reset 2 (Direct8 L) (Direct8 L)
        0x96 -> Reset 2 (Indirect HL) (Indirect HL)
        0x97 -> Reset 2 (Direct8 A) (Direct8 A)
        0x98 -> Reset 3 (Direct8 B) (Direct8 B)
        0x99 -> Reset 3 (Direct8 C) (Direct8 C)
        0x9A -> Reset 3 (Direct8 D) (Direct8 D)
        0x9B -> Reset 3 (Direct8 E) (Direct8 E)
        0x9C -> Reset 3 (Direct8 H) (Direct8 H)
        0x9D -> Reset 3 (Direct8 L) (Direct8 L)
        0x9E -> Reset 3 (Indirect HL) (Indirect HL)
        0x9F -> Reset 3 (Direct8 A) (Direct8 A)
        0xA0 -> Reset 4 (Direct8 B) (Direct8 B)
        0xA1 -> Reset 4 (Direct8 C) (Direct8 C)
        0xA2 -> Reset 4 (Direct8 D) (Direct8 D)
        0xA3 -> Reset 4 (Direct8 E) (Direct8 E)
        0xA4 -> Reset 4 (Direct8 H) (Direct8 H)
        0xA5 -> Reset 4 (Direct8 L) (Direct8 L)
        0xA6 -> Reset 4 (Indirect HL) (Indirect HL)
        0xA7 -> Reset 4 (Direct8 A) (Direct8 A)
        0xA8 -> Reset 5 (Direct8 B) (Direct8 B)
        0xA9 -> Reset 5 (Direct8 C) (Direct8 C)
        0xAA -> Reset 5 (Direct8 D) (Direct8 D)
        0xAB -> Reset 5 (Direct8 E) (Direct8 E)
        0xAC -> Reset 5 (Direct8 H) (Direct8 H)
        0xAD -> Reset 5 (Direct8 L) (Direct8 L)
        0xAE -> Reset 5 (Indirect HL) (Indirect HL)
        0xAF -> Reset 5 (Direct8 A) (Direct8 A)
        0xB0 -> Reset 6 (Direct8 B) (Direct8 B)
        0xB1 -> Reset 6 (Direct8 C) (Direct8 C)
        0xB2 -> Reset 6 (Direct8 D) (Direct8 D)
        0xB3 -> Reset 6 (Direct8 E) (Direct8 E)
        0xB4 -> Reset 6 (Direct8 H) (Direct8 H)
        0xB5 -> Reset 6 (Direct8 L) (Direct8 L)
        0xB6 -> Reset 6 (Indirect HL) (Indirect HL)
        0xB7 -> Reset 6 (Direct8 A) (Direct8 A)
        0xB8 -> Reset 7 (Direct8 B) (Direct8 B)
        0xB9 -> Reset 7 (Direct8 C) (Direct8 C)
        0xBA -> Reset 7 (Direct8 D) (Direct8 D)
        0xBB -> Reset 7 (Direct8 E) (Direct8 E)
        0xBC -> Reset 7 (Direct8 H) (Direct8 H)
        0xBD -> Reset 7 (Direct8 L) (Direct8 L)
        0xBE -> Reset 7 (Indirect HL) (Indirect HL)
        0xBF -> Reset 7 (Direct8 A) (Direct8 A)
        0xC0 -> Set 0 (Direct8 B) (Direct8 B)
        0xC1 -> Set 0 (Direct8 C) (Direct8 C)
        0xC2 -> Set 0 (Direct8 D) (Direct8 D)
        0xC3 -> Set 0 (Direct8 E) (Direct8 E)
        0xC4 -> Set 0 (Direct8 H) (Direct8 H)
        0xC5 -> Set 0 (Direct8 L) (Direct8 L)
        0xC6 -> Set 0 (Indirect HL) (Indirect HL)
        0xC7 -> Set 0 (Direct8 A) (Direct8 A)
        0xC8 -> Set 1 (Direct8 B) (Direct8 B)
        0xC9 -> Set 1 (Direct8 C) (Direct8 C)
        0xCA -> Set 1 (Direct8 D) (Direct8 D)
        0xCB -> Set 1 (Direct8 E) (Direct8 E)
        0xCC -> Set 1 (Direct8 H) (Direct8 H)
        0xCD -> Set 1 (Direct8 L) (Direct8 L)
        0xCE -> Set 1 (Indirect HL) (Indirect HL)
        0xCF -> Set 1 (Direct8 A) (Direct8 A)
        0xD0 -> Set 2 (Direct8 B) (Direct8 B)
        0xD1 -> Set 2 (Direct8 C) (Direct8 C)
        0xD2 -> Set 2 (Direct8 D) (Direct8 D)
        0xD3 -> Set 2 (Direct8 E) (Direct8 E)
        0xD4 -> Set 2 (Direct8 H) (Direct8 H)
        0xD5 -> Set 2 (Direct8 L) (Direct8 L)
        0xD6 -> Set 2 (Indirect HL) (Indirect HL)
        0xD7 -> Set 2 (Direct8 A) (Direct8 A)
        0xD8 -> Set 3 (Direct8 B) (Direct8 B)
        0xD9 -> Set 3 (Direct8 C) (Direct8 C)
        0xDA -> Set 3 (Direct8 D) (Direct8 D)
        0xDB -> Set 3 (Direct8 E) (Direct8 E)
        0xDC -> Set 3 (Direct8 H) (Direct8 H)
        0xDD -> Set 3 (Direct8 L) (Direct8 L)
        0xDE -> Set 3 (Indirect HL) (Indirect HL)
        0xDF -> Set 3 (Direct8 A) (Direct8 A)
        0xE0 -> Set 4 (Direct8 B) (Direct8 B)
        0xE1 -> Set 4 (Direct8 C) (Direct8 C)
        0xE2 -> Set 4 (Direct8 D) (Direct8 D)
        0xE3 -> Set 4 (Direct8 E) (Direct8 E)
        0xE4 -> Set 4 (Direct8 H) (Direct8 H)
        0xE5 -> Set 4 (Direct8 L) (Direct8 L)
        0xE6 -> Set 4 (Indirect HL) (Indirect HL)
        0xE7 -> Set 4 (Direct8 A) (Direct8 A)
        0xE8 -> Set 5 (Direct8 B) (Direct8 B)
        0xE9 -> Set 5 (Direct8 C) (Direct8 C)
        0xEA -> Set 5 (Direct8 D) (Direct8 D)
        0xEB -> Set 5 (Direct8 E) (Direct8 E)
        0xEC -> Set 5 (Direct8 H) (Direct8 H)
        0xED -> Set 5 (Direct8 L) (Direct8 L)
        0xEE -> Set 5 (Indirect HL) (Indirect HL)
        0xEF -> Set 5 (Direct8 A) (Direct8 A)
        0xF0 -> Set 6 (Direct8 B) (Direct8 B)
        0xF1 -> Set 6 (Direct8 C) (Direct8 C)
        0xF2 -> Set 6 (Direct8 D) (Direct8 D)
        0xF3 -> Set 6 (Direct8 E) (Direct8 E)
        0xF4 -> Set 6 (Direct8 H) (Direct8 H)
        0xF5 -> Set 6 (Direct8 L) (Direct8 L)
        0xF6 -> Set 6 (Indirect HL) (Indirect HL)
        0xF7 -> Set 6 (Direct8 A) (Direct8 A)
        0xF8 -> Set 7 (Direct8 B) (Direct8 B)
        0xF9 -> Set 7 (Direct8 C) (Direct8 C)
        0xFA -> Set 7 (Direct8 D) (Direct8 D)
        0xFB -> Set 7 (Direct8 E) (Direct8 E)
        0xFC -> Set 7 (Direct8 H) (Direct8 H)
        0xFD -> Set 7 (Direct8 L) (Direct8 L)
        0xFE -> Set 7 (Indirect HL) (Indirect HL)
        0xFF -> Set 7 (Direct8 A) (Direct8 A)
        _ -> Unknown
