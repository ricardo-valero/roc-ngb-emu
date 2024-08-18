module [Instruction, instruction]

import Cpu.Condition exposing [Condition]

Read8 : [Read8Immediate, Read8IndirectHL, Read8RegisterA, Read8RegisterB, Read8RegisterC, Read8RegisterD, Read8RegisterE, Read8RegisterH, Read8RegisterL]
Read8IncDec : [Read8IndirectHL, Read8RegisterA, Read8RegisterB, Read8RegisterC, Read8RegisterD, Read8RegisterE, Read8RegisterH, Read8RegisterL]
Write8IncDec : [Write8IndirectHL, Write8RegisterA, Write8RegisterB, Write8RegisterC, Write8RegisterD, Write8RegisterE, Write8RegisterH, Write8RegisterL]
Read16 : [Read16RegisterBC, Read16RegisterDE, Read16RegisterHL, Read16RegisterSP]
Write16 : [Write16RegisterBC, Write16RegisterDE, Write16RegisterHL, Write16RegisterSP]

WriteLd : [
    Write16IndirectWord16Operand,
    Write16RegisterBC,
    Write16RegisterDE,
    Write16RegisterHL,
    Write16RegisterSP,
    Write8IndirectBC,
    Write8IndirectC,
    Write8IndirectDE,
    Write8IndirectHL,
    Write8IndirectHLPostDecrement,
    Write8IndirectHLPostIncrement,
    Write8IndirectWord16Operand,
    Write8IndirectWord8Operand,
    Write8RegisterA,
    Write8RegisterB,
    Write8RegisterC,
    Write8RegisterD,
    Write8RegisterE,
    Write8RegisterH,
    Write8RegisterL,
]
ReadLd : [
    Read16Immediate,
    Read16RegisterSP,
    Read8Immediate,
    Read8IndirectBC,
    Read8IndirectC,
    Read8IndirectDE,
    Read8IndirectHL,
    Read8IndirectHLPostDecrement,
    Read8IndirectHLPostIncrement,
    Read8IndirectWord16Operand,
    Read8IndirectWord8Operand,
    Read8RegisterA,
    Read8RegisterB,
    Read8RegisterC,
    Read8RegisterD,
    Read8RegisterE,
    Read8RegisterH,
    Read8RegisterL,
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
    JumpAbsolute Condition [Read16Immediate], # Jp
    JpIndirectHL,
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
    # 8-bit arithmetic / logical instructions
    Add Read8,
    Adc Read8,
    Sub Read8,
    Sbc Read8,
    And Read8,
    Xor Read8,
    Or Read8,
    Cp Read8,
    Inc Read8IncDec Write8IncDec,
    Dec Read8IncDec Write8IncDec,
    # 16-bit arithmetic / logical instructions
    Add16 Read16,
    Dec16 Read16 Write16,
    Inc16 Read16 Write16,
    AddSPSignedImmediate,
    # 8-bit load instructions
    # 16-bit load instructions
    Ld WriteLd ReadLd,
    Push [Read16RegisterAF, Read16RegisterBC, Read16RegisterDE, Read16RegisterHL],
    Pop [Write16RegisterAF, Write16RegisterBC, Write16RegisterDE, Write16RegisterHL],
    LdHLSPPlusSignedImmediate,
    LdSPHL,
]

instruction : U16 -> Instruction
instruction = \byte ->
    when byte is
        0x00 -> Nop
        0x01 -> Ld Write16RegisterBC Read16Immediate
        0x02 -> Ld Write8IndirectBC Read8RegisterA
        0x03 -> Inc16 Read16RegisterBC Write16RegisterBC
        0x04 -> Inc Read8RegisterB Write8RegisterB
        0x05 -> Dec Read8RegisterB Write8RegisterB
        0x06 -> Ld Write8RegisterB Read8Immediate
        0x07 -> RotateLeftCircularAccumulator
        0x08 -> Ld Write16IndirectWord16Operand Read16RegisterSP
        0x09 -> Add16 Read16RegisterBC
        0x0A -> Ld Write8RegisterA Read8IndirectBC
        0x0B -> Dec16 Read16RegisterBC Write16RegisterBC
        0x0C -> Inc Read8RegisterC Write8RegisterC
        0x0D -> Dec Read8RegisterC Write8RegisterC
        0x0E -> Ld Write8RegisterC Read8Immediate
        0x0F -> RotateRightCircularAccumulator
        0x10 -> Nop # Stop
        0x11 -> Ld Write16RegisterDE Read16Immediate
        0x12 -> Ld Write8IndirectDE Read8RegisterA
        0x13 -> Inc16 Read16RegisterDE Write16RegisterDE
        0x14 -> Inc Read8RegisterD Write8RegisterD
        0x15 -> Dec Read8RegisterD Write8RegisterD
        0x16 -> Ld Write8RegisterD Read8Immediate
        0x17 -> RotateLeftAccumulator
        0x18 -> JumpRelative Always
        0x19 -> Add16 Read16RegisterDE
        0x1A -> Ld Write8RegisterA Read8IndirectDE
        0x1B -> Dec16 Read16RegisterDE Write16RegisterDE
        0x1C -> Inc Read8RegisterE Write8RegisterE
        0x1D -> Dec Read8RegisterE Write8RegisterE
        0x1E -> Ld Write8RegisterE Read8Immediate
        0x1F -> RotateRightAccumulator
        0x20 -> JumpRelative NotZero
        0x21 -> Ld Write16RegisterHL Read16Immediate
        0x22 -> Ld Write8IndirectHLPostIncrement Read8RegisterA
        0x23 -> Inc16 Read16RegisterHL Write16RegisterHL
        0x24 -> Inc Read8RegisterH Write8RegisterH
        0x25 -> Dec Read8RegisterH Write8RegisterH
        0x26 -> Ld Write8RegisterH Read8Immediate
        0x27 -> DecimalAdjustAccumulator
        0x28 -> JumpRelative Zero
        0x29 -> Add16 Read16RegisterHL
        0x2A -> Ld Write8RegisterA Read8IndirectHLPostIncrement
        0x2B -> Dec16 Read16RegisterHL Write16RegisterHL
        0x2C -> Inc Read8RegisterL Write8RegisterL
        0x2D -> Dec Read8RegisterL Write8RegisterL
        0x2E -> Ld Write8RegisterL Read8Immediate
        0x2F -> ComplementAccumulator
        0x30 -> JumpRelative NotCarry
        0x31 -> Ld Write16RegisterSP Read16Immediate
        0x32 -> Ld Write8IndirectHLPostDecrement Read8RegisterA
        0x33 -> Inc16 Read16RegisterSP Write16RegisterSP
        0x34 -> Inc Read8IndirectHL Write8IndirectHL
        0x35 -> Dec Read8IndirectHL Write8IndirectHL
        0x36 -> Ld Write8IndirectHL Read8Immediate
        0x37 -> SetCarryFlag
        0x38 -> JumpRelative Carry
        0x39 -> Add16 Read16RegisterSP
        0x3A -> Ld Write8RegisterA Read8IndirectHLPostDecrement
        0x3B -> Dec16 Read16RegisterSP Write16RegisterSP
        0x3C -> Inc Read8RegisterA Write8RegisterA
        0x3D -> Dec Read8RegisterA Write8RegisterA
        0x3E -> Ld Write8RegisterA Read8Immediate
        0x3F -> ComplementCarryFlag
        0x40 -> Ld Write8RegisterB Read8RegisterB
        0x41 -> Ld Write8RegisterB Read8RegisterC
        0x42 -> Ld Write8RegisterB Read8RegisterD
        0x43 -> Ld Write8RegisterB Read8RegisterE
        0x44 -> Ld Write8RegisterB Read8RegisterH
        0x45 -> Ld Write8RegisterB Read8RegisterL
        0x46 -> Ld Write8RegisterB Read8IndirectHL
        0x47 -> Ld Write8RegisterB Read8RegisterA
        0x48 -> Ld Write8RegisterC Read8RegisterB
        0x49 -> Ld Write8RegisterC Read8RegisterC
        0x4A -> Ld Write8RegisterC Read8RegisterD
        0x4B -> Ld Write8RegisterC Read8RegisterE
        0x4C -> Ld Write8RegisterC Read8RegisterH
        0x4D -> Ld Write8RegisterC Read8RegisterL
        0x4E -> Ld Write8RegisterC Read8IndirectHL
        0x4F -> Ld Write8RegisterC Read8RegisterA
        0x50 -> Ld Write8RegisterD Read8RegisterB
        0x51 -> Ld Write8RegisterD Read8RegisterC
        0x52 -> Ld Write8RegisterD Read8RegisterD
        0x53 -> Ld Write8RegisterD Read8RegisterE
        0x54 -> Ld Write8RegisterD Read8RegisterH
        0x55 -> Ld Write8RegisterD Read8RegisterL
        0x56 -> Ld Write8RegisterD Read8IndirectHL
        0x57 -> Ld Write8RegisterD Read8RegisterA
        0x58 -> Ld Write8RegisterE Read8RegisterB
        0x59 -> Ld Write8RegisterE Read8RegisterC
        0x5A -> Ld Write8RegisterE Read8RegisterD
        0x5B -> Ld Write8RegisterE Read8RegisterE
        0x5C -> Ld Write8RegisterE Read8RegisterH
        0x5D -> Ld Write8RegisterE Read8RegisterL
        0x5E -> Ld Write8RegisterE Read8IndirectHL
        0x5F -> Ld Write8RegisterE Read8RegisterA
        0x60 -> Ld Write8RegisterH Read8RegisterB
        0x61 -> Ld Write8RegisterH Read8RegisterC
        0x62 -> Ld Write8RegisterH Read8RegisterD
        0x63 -> Ld Write8RegisterH Read8RegisterE
        0x64 -> Ld Write8RegisterH Read8RegisterH
        0x65 -> Ld Write8RegisterH Read8RegisterL
        0x66 -> Ld Write8RegisterH Read8IndirectHL
        0x67 -> Ld Write8RegisterH Read8RegisterA
        0x68 -> Ld Write8RegisterL Read8RegisterB
        0x69 -> Ld Write8RegisterL Read8RegisterC
        0x6A -> Ld Write8RegisterL Read8RegisterD
        0x6B -> Ld Write8RegisterL Read8RegisterE
        0x6C -> Ld Write8RegisterL Read8RegisterH
        0x6D -> Ld Write8RegisterL Read8RegisterL
        0x6E -> Ld Write8RegisterL Read8IndirectHL
        0x6F -> Ld Write8RegisterL Read8RegisterA
        0x70 -> Ld Write8IndirectHL Read8RegisterB
        0x71 -> Ld Write8IndirectHL Read8RegisterC
        0x72 -> Ld Write8IndirectHL Read8RegisterD
        0x73 -> Ld Write8IndirectHL Read8RegisterE
        0x74 -> Ld Write8IndirectHL Read8RegisterH
        0x75 -> Ld Write8IndirectHL Read8RegisterL
        0x76 -> Halt
        0x77 -> Ld Write8IndirectHL Read8RegisterA
        0x78 -> Ld Write8RegisterA Read8RegisterB
        0x79 -> Ld Write8RegisterA Read8RegisterC
        0x7A -> Ld Write8RegisterA Read8RegisterD
        0x7B -> Ld Write8RegisterA Read8RegisterE
        0x7C -> Ld Write8RegisterA Read8RegisterH
        0x7D -> Ld Write8RegisterA Read8RegisterL
        0x7E -> Ld Write8RegisterA Read8IndirectHL
        0x7F -> Ld Write8RegisterA Read8RegisterA
        0x80 -> Add Read8RegisterB
        0x81 -> Add Read8RegisterC
        0x82 -> Add Read8RegisterD
        0x83 -> Add Read8RegisterE
        0x84 -> Add Read8RegisterH
        0x85 -> Add Read8RegisterL
        0x86 -> Add Read8IndirectHL
        0x87 -> Add Read8RegisterA
        0x88 -> Adc Read8RegisterB
        0x89 -> Adc Read8RegisterC
        0x8A -> Adc Read8RegisterD
        0x8B -> Adc Read8RegisterE
        0x8C -> Adc Read8RegisterH
        0x8D -> Adc Read8RegisterL
        0x8E -> Adc Read8IndirectHL
        0x8F -> Adc Read8RegisterA
        0x90 -> Sub Read8RegisterB
        0x91 -> Sub Read8RegisterC
        0x92 -> Sub Read8RegisterD
        0x93 -> Sub Read8RegisterE
        0x94 -> Sub Read8RegisterH
        0x95 -> Sub Read8RegisterL
        0x96 -> Sub Read8IndirectHL
        0x97 -> Sub Read8RegisterA
        0x98 -> Sbc Read8RegisterB
        0x99 -> Sbc Read8RegisterC
        0x9A -> Sbc Read8RegisterD
        0x9B -> Sbc Read8RegisterE
        0x9C -> Sbc Read8RegisterH
        0x9D -> Sbc Read8RegisterL
        0x9E -> Sbc Read8IndirectHL
        0x9F -> Sbc Read8RegisterA
        0xA0 -> And Read8RegisterB
        0xA1 -> And Read8RegisterC
        0xA2 -> And Read8RegisterD
        0xA3 -> And Read8RegisterE
        0xA4 -> And Read8RegisterH
        0xA5 -> And Read8RegisterL
        0xA6 -> And Read8IndirectHL
        0xA7 -> And Read8RegisterA
        0xA8 -> Xor Read8RegisterB
        0xA9 -> Xor Read8RegisterC
        0xAA -> Xor Read8RegisterD
        0xAB -> Xor Read8RegisterE
        0xAC -> Xor Read8RegisterH
        0xAD -> Xor Read8RegisterL
        0xAE -> Xor Read8IndirectHL
        0xAF -> Xor Read8RegisterA
        0xB0 -> Or Read8RegisterB
        0xB1 -> Or Read8RegisterC
        0xB2 -> Or Read8RegisterD
        0xB3 -> Or Read8RegisterE
        0xB4 -> Or Read8RegisterH
        0xB5 -> Or Read8RegisterL
        0xB6 -> Or Read8IndirectHL
        0xB7 -> Or Read8RegisterA
        0xB8 -> Cp Read8RegisterB
        0xB9 -> Cp Read8RegisterC
        0xBA -> Cp Read8RegisterD
        0xBB -> Cp Read8RegisterE
        0xBC -> Cp Read8RegisterH
        0xBD -> Cp Read8RegisterL
        0xBE -> Cp Read8IndirectHL
        0xBF -> Cp Read8RegisterA
        0xC0 -> Return NotZero
        0xC1 -> Pop Write16RegisterBC
        0xC2 -> JumpAbsolute NotZero Read16Immediate
        0xC3 -> JumpAbsolute Always Read16Immediate
        0xC4 -> Call NotZero
        0xC5 -> Push Read16RegisterBC
        0xC6 -> Add Read8Immediate
        0xC7 -> Restart 0x00
        0xC8 -> Return Zero
        0xC9 -> Return Always
        0xCA -> JumpAbsolute Zero Read16Immediate
        0xCB -> Prefix
        0xCC -> Call Zero
        0xCD -> Call Always
        0xCE -> Adc Read8Immediate
        0xCF -> Restart 0x08
        0xD0 -> Return NotCarry
        0xD1 -> Pop Write16RegisterDE
        0xD2 -> JumpAbsolute NotCarry Read16Immediate
        0xD3 -> Illegal
        0xD4 -> Call NotCarry
        0xD5 -> Push Read16RegisterDE
        0xD6 -> Sub Read8Immediate
        0xD7 -> Restart 0x10
        0xD8 -> Return Carry
        0xD9 -> ReturnAndEnableInterrupts
        0xDA -> JumpAbsolute Carry Read16Immediate
        0xDB -> Illegal
        0xDC -> Call Carry
        0xDD -> Illegal
        0xDE -> Sbc Read8Immediate
        0xDF -> Restart 0x18
        0xE0 -> Ld Write8IndirectWord8Operand Read8RegisterA
        0xE1 -> Pop Write16RegisterHL
        0xE2 -> Ld Write8IndirectC Read8RegisterA
        0xE3 -> Illegal
        0xE4 -> Illegal
        0xE5 -> Push Read16RegisterHL
        0xE6 -> And Read8Immediate
        0xE7 -> Restart 0x20
        0xE8 -> AddSPSignedImmediate
        0xE9 -> JpIndirectHL
        0xEA -> Ld Write8IndirectWord16Operand Read8RegisterA
        0xEB -> Illegal
        0xEC -> Illegal
        0xED -> Illegal
        0xEE -> Xor Read8Immediate
        0xEF -> Restart 0x28
        0xF0 -> Ld Write8RegisterA Read8IndirectWord8Operand
        0xF1 -> Pop Write16RegisterAF
        0xF2 -> Ld Write8RegisterA Read8IndirectC
        0xF3 -> DisableInterrupts
        0xF4 -> Illegal
        0xF5 -> Push Read16RegisterAF
        0xF6 -> Or Read8Immediate
        0xF7 -> Restart 0x30
        0xF8 -> LdHLSPPlusSignedImmediate
        0xF9 -> LdSPHL
        0xFA -> Ld Write8RegisterA Read8IndirectWord16Operand
        0xFB -> EnableInterrupts
        0xFC -> Illegal
        0xFD -> Illegal
        0xFE -> Cp Read8Immediate
        0xFF -> Restart 0x38
        _ -> Unknown
