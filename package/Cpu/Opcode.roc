module [Instruction, instruction]

import Cpu.Condition exposing [Condition]

Read8 : [Immediate, IndirectHL, RegisterA, RegisterB, RegisterC, RegisterD, RegisterE, RegisterH, RegisterL]
Read8IncDec : [IndirectHL, RegisterA, RegisterB, RegisterC, RegisterD, RegisterE, RegisterH, RegisterL]
Write8IncDec : [IndirectHL, RegisterA, RegisterB, RegisterC, RegisterD, RegisterE, RegisterH, RegisterL]
Read16 : [RegisterBC, RegisterDE, RegisterHL, RegisterSP]
Write16 : [RegisterBC, RegisterDE, RegisterHL, RegisterSP]

WriteLd16 : [
    IndirectWord16Operand,
    RegisterBC,
    RegisterDE,
    RegisterHL,
    RegisterSP,
]
ReadLd16 : [
    Immediate,
    RegisterSP,
]
WriteLd : [
    IndirectBC,
    IndirectC,
    IndirectDE,
    IndirectHL,
    IndirectHLPostDecrement,
    IndirectHLPostIncrement,
    IndirectWord16Operand,
    IndirectWord8Operand,
    RegisterA,
    RegisterB,
    RegisterC,
    RegisterD,
    RegisterE,
    RegisterH,
    RegisterL,
]
ReadLd : [
    Immediate,
    IndirectBC,
    IndirectC,
    IndirectDE,
    IndirectHL,
    IndirectHLPostDecrement,
    IndirectHLPostIncrement,
    IndirectWord16Operand,
    IndirectWord8Operand,
    RegisterA,
    RegisterB,
    RegisterC,
    RegisterD,
    RegisterE,
    RegisterH,
    RegisterL,
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
    Add Read8,
    Adc Read8, # Adc
    Sub Read8,
    Sbc Read8, # Sbc
    And Read8,
    Xor Read8,
    Or Read8,
    Cp Read8, # Cp
    Inc Read8IncDec Write8IncDec,
    Dec Read8IncDec Write8IncDec,
    # 16-bit arithmetic / logical instructions
    Add16 Read16,
    Dec16 Read16 Write16,
    Inc16 Read16 Write16,
    AddSPSignedImmediate,
    # 8-bit load instructions
    Load WriteLd ReadLd, # Ld
    # 16-bit load instructions
    Load16 WriteLd16 ReadLd16, # Ld
    Push [RegisterAF, RegisterBC, RegisterDE, RegisterHL],
    Pop [RegisterAF, RegisterBC, RegisterDE, RegisterHL],
    LdHLSPPlusSignedImmediate,
    LdSPHL,
]

instruction : U16 -> Instruction
instruction = \byte ->
    when byte is
        0x00 -> Nop
        0x01 -> Load16 RegisterBC Immediate
        0x02 -> Load IndirectBC RegisterA
        0x03 -> Inc16 RegisterBC RegisterBC
        0x04 -> Inc RegisterB RegisterB
        0x05 -> Dec RegisterB RegisterB
        0x06 -> Load RegisterB Immediate
        0x07 -> RotateLeftCircularAccumulator
        0x08 -> Load16 IndirectWord16Operand RegisterSP
        0x09 -> Add16 RegisterBC
        0x0A -> Load RegisterA IndirectBC
        0x0B -> Dec16 RegisterBC RegisterBC
        0x0C -> Inc RegisterC RegisterC
        0x0D -> Dec RegisterC RegisterC
        0x0E -> Load RegisterC Immediate
        0x0F -> RotateRightCircularAccumulator
        0x10 -> Nop # Stop
        0x11 -> Load16 RegisterDE Immediate
        0x12 -> Load IndirectDE RegisterA
        0x13 -> Inc16 RegisterDE RegisterDE
        0x14 -> Inc RegisterD RegisterD
        0x15 -> Dec RegisterD RegisterD
        0x16 -> Load RegisterD Immediate
        0x17 -> RotateLeftAccumulator
        0x18 -> JumpRelative Always
        0x19 -> Add16 RegisterDE
        0x1A -> Load RegisterA IndirectDE
        0x1B -> Dec16 RegisterDE RegisterDE
        0x1C -> Inc RegisterE RegisterE
        0x1D -> Dec RegisterE RegisterE
        0x1E -> Load RegisterE Immediate
        0x1F -> RotateRightAccumulator
        0x20 -> JumpRelative NotZero
        0x21 -> Load16 RegisterHL Immediate
        0x22 -> Load IndirectHLPostIncrement RegisterA
        0x23 -> Inc16 RegisterHL RegisterHL
        0x24 -> Inc RegisterH RegisterH
        0x25 -> Dec RegisterH RegisterH
        0x26 -> Load RegisterH Immediate
        0x27 -> DecimalAdjustAccumulator
        0x28 -> JumpRelative Zero
        0x29 -> Add16 RegisterHL
        0x2A -> Load RegisterA IndirectHLPostIncrement
        0x2B -> Dec16 RegisterHL RegisterHL
        0x2C -> Inc RegisterL RegisterL
        0x2D -> Dec RegisterL RegisterL
        0x2E -> Load RegisterL Immediate
        0x2F -> ComplementAccumulator
        0x30 -> JumpRelative NotCarry
        0x31 -> Load16 RegisterSP Immediate
        0x32 -> Load IndirectHLPostDecrement RegisterA
        0x33 -> Inc16 RegisterSP RegisterSP
        0x34 -> Inc IndirectHL IndirectHL
        0x35 -> Dec IndirectHL IndirectHL
        0x36 -> Load IndirectHL Immediate
        0x37 -> SetCarryFlag
        0x38 -> JumpRelative Carry
        0x39 -> Add16 RegisterSP
        0x3A -> Load RegisterA IndirectHLPostDecrement
        0x3B -> Dec16 RegisterSP RegisterSP
        0x3C -> Inc RegisterA RegisterA
        0x3D -> Dec RegisterA RegisterA
        0x3E -> Load RegisterA Immediate
        0x3F -> ComplementCarryFlag
        0x40 -> Load RegisterB RegisterB
        0x41 -> Load RegisterB RegisterC
        0x42 -> Load RegisterB RegisterD
        0x43 -> Load RegisterB RegisterE
        0x44 -> Load RegisterB RegisterH
        0x45 -> Load RegisterB RegisterL
        0x46 -> Load RegisterB IndirectHL
        0x47 -> Load RegisterB RegisterA
        0x48 -> Load RegisterC RegisterB
        0x49 -> Load RegisterC RegisterC
        0x4A -> Load RegisterC RegisterD
        0x4B -> Load RegisterC RegisterE
        0x4C -> Load RegisterC RegisterH
        0x4D -> Load RegisterC RegisterL
        0x4E -> Load RegisterC IndirectHL
        0x4F -> Load RegisterC RegisterA
        0x50 -> Load RegisterD RegisterB
        0x51 -> Load RegisterD RegisterC
        0x52 -> Load RegisterD RegisterD
        0x53 -> Load RegisterD RegisterE
        0x54 -> Load RegisterD RegisterH
        0x55 -> Load RegisterD RegisterL
        0x56 -> Load RegisterD IndirectHL
        0x57 -> Load RegisterD RegisterA
        0x58 -> Load RegisterE RegisterB
        0x59 -> Load RegisterE RegisterC
        0x5A -> Load RegisterE RegisterD
        0x5B -> Load RegisterE RegisterE
        0x5C -> Load RegisterE RegisterH
        0x5D -> Load RegisterE RegisterL
        0x5E -> Load RegisterE IndirectHL
        0x5F -> Load RegisterE RegisterA
        0x60 -> Load RegisterH RegisterB
        0x61 -> Load RegisterH RegisterC
        0x62 -> Load RegisterH RegisterD
        0x63 -> Load RegisterH RegisterE
        0x64 -> Load RegisterH RegisterH
        0x65 -> Load RegisterH RegisterL
        0x66 -> Load RegisterH IndirectHL
        0x67 -> Load RegisterH RegisterA
        0x68 -> Load RegisterL RegisterB
        0x69 -> Load RegisterL RegisterC
        0x6A -> Load RegisterL RegisterD
        0x6B -> Load RegisterL RegisterE
        0x6C -> Load RegisterL RegisterH
        0x6D -> Load RegisterL RegisterL
        0x6E -> Load RegisterL IndirectHL
        0x6F -> Load RegisterL RegisterA
        0x70 -> Load IndirectHL RegisterB
        0x71 -> Load IndirectHL RegisterC
        0x72 -> Load IndirectHL RegisterD
        0x73 -> Load IndirectHL RegisterE
        0x74 -> Load IndirectHL RegisterH
        0x75 -> Load IndirectHL RegisterL
        0x76 -> Halt
        0x77 -> Load IndirectHL RegisterA
        0x78 -> Load RegisterA RegisterB
        0x79 -> Load RegisterA RegisterC
        0x7A -> Load RegisterA RegisterD
        0x7B -> Load RegisterA RegisterE
        0x7C -> Load RegisterA RegisterH
        0x7D -> Load RegisterA RegisterL
        0x7E -> Load RegisterA IndirectHL
        0x7F -> Load RegisterA RegisterA
        0x80 -> Add RegisterB
        0x81 -> Add RegisterC
        0x82 -> Add RegisterD
        0x83 -> Add RegisterE
        0x84 -> Add RegisterH
        0x85 -> Add RegisterL
        0x86 -> Add IndirectHL
        0x87 -> Add RegisterA
        0x88 -> Adc RegisterB
        0x89 -> Adc RegisterC
        0x8A -> Adc RegisterD
        0x8B -> Adc RegisterE
        0x8C -> Adc RegisterH
        0x8D -> Adc RegisterL
        0x8E -> Adc IndirectHL
        0x8F -> Adc RegisterA
        0x90 -> Sub RegisterB
        0x91 -> Sub RegisterC
        0x92 -> Sub RegisterD
        0x93 -> Sub RegisterE
        0x94 -> Sub RegisterH
        0x95 -> Sub RegisterL
        0x96 -> Sub IndirectHL
        0x97 -> Sub RegisterA
        0x98 -> Sbc RegisterB
        0x99 -> Sbc RegisterC
        0x9A -> Sbc RegisterD
        0x9B -> Sbc RegisterE
        0x9C -> Sbc RegisterH
        0x9D -> Sbc RegisterL
        0x9E -> Sbc IndirectHL
        0x9F -> Sbc RegisterA
        0xA0 -> And RegisterB
        0xA1 -> And RegisterC
        0xA2 -> And RegisterD
        0xA3 -> And RegisterE
        0xA4 -> And RegisterH
        0xA5 -> And RegisterL
        0xA6 -> And IndirectHL
        0xA7 -> And RegisterA
        0xA8 -> Xor RegisterB
        0xA9 -> Xor RegisterC
        0xAA -> Xor RegisterD
        0xAB -> Xor RegisterE
        0xAC -> Xor RegisterH
        0xAD -> Xor RegisterL
        0xAE -> Xor IndirectHL
        0xAF -> Xor RegisterA
        0xB0 -> Or RegisterB
        0xB1 -> Or RegisterC
        0xB2 -> Or RegisterD
        0xB3 -> Or RegisterE
        0xB4 -> Or RegisterH
        0xB5 -> Or RegisterL
        0xB6 -> Or IndirectHL
        0xB7 -> Or RegisterA
        0xB8 -> Cp RegisterB
        0xB9 -> Cp RegisterC
        0xBA -> Cp RegisterD
        0xBB -> Cp RegisterE
        0xBC -> Cp RegisterH
        0xBD -> Cp RegisterL
        0xBE -> Cp IndirectHL
        0xBF -> Cp RegisterA
        0xC0 -> Return NotZero
        0xC1 -> Pop RegisterBC
        0xC2 -> JumpAbsolute NotZero Immediate
        0xC3 -> JumpAbsolute Always Immediate
        0xC4 -> Call NotZero
        0xC5 -> Push RegisterBC
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
        0xD1 -> Pop RegisterDE
        0xD2 -> JumpAbsolute NotCarry Immediate
        0xD3 -> Illegal
        0xD4 -> Call NotCarry
        0xD5 -> Push RegisterDE
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
        0xE0 -> Load IndirectWord8Operand RegisterA
        0xE1 -> Pop RegisterHL
        0xE2 -> Load IndirectC RegisterA
        0xE3 -> Illegal
        0xE4 -> Illegal
        0xE5 -> Push RegisterHL
        0xE6 -> And Immediate
        0xE7 -> Restart 0x20
        0xE8 -> AddSPSignedImmediate
        0xE9 -> JpIndirectHL
        0xEA -> Load IndirectWord16Operand RegisterA
        0xEB -> Illegal
        0xEC -> Illegal
        0xED -> Illegal
        0xEE -> Xor Immediate
        0xEF -> Restart 0x28
        0xF0 -> Load RegisterA IndirectWord8Operand
        0xF1 -> Pop RegisterAF
        0xF2 -> Load RegisterA IndirectC
        0xF3 -> DisableInterrupts
        0xF4 -> Illegal
        0xF5 -> Push RegisterAF
        0xF6 -> Or Immediate
        0xF7 -> Restart 0x30
        0xF8 -> LdHLSPPlusSignedImmediate
        0xF9 -> LdSPHL
        0xFA -> Load RegisterA IndirectWord16Operand
        0xFB -> EnableInterrupts
        0xFC -> Illegal
        0xFD -> Illegal
        0xFE -> Cp Immediate
        0xFF -> Restart 0x38
        _ -> Unknown
