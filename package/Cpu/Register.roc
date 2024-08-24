module [Register, Type8, Type16, init, read8, read16, write8, write16]

import Constant exposing [MemoryAddress]
# 8-Bit and 16-Bit
# A: Accumulator
# F: Flags
# SP: Stack Pointer
# PC: Program Counter
Type8 : [A, F, B, C, D, E, H, L]
Type16 : [AF, BC, DE, HL, SP, PC]

Register : {
    af : MemoryAddress,
    bc : MemoryAddress,
    de : MemoryAddress,
    hl : MemoryAddress,
    programCounter : MemoryAddress,
    stackPointer : MemoryAddress,
    interruptMasterEnable : Bool, # IME
    halted : Bool,
    interruptFlag : U8,
    interruptEnable : U8,
}

init : Register
init = {
    af: 0x01B0,
    bc: 0x13,
    de: 0xD8,
    hl: 0x014D,
    programCounter: 0x0100,
    stackPointer: 0xFFFE,
    interruptMasterEnable: Bool.true,
    halted: Bool.false,
    interruptFlag: 0xE1,
    interruptEnable: 0x00,
}

read8 : Type8, Register -> U8
read8 = \type, reg ->
    (
        when type is
            A -> Num.shiftRightZfBy reg.af 8
            F -> Num.bitwiseAnd reg.af 0xFF
            B -> Num.shiftRightZfBy reg.bc 8
            C -> Num.bitwiseAnd reg.bc 0xFF
            D -> Num.shiftRightZfBy reg.de 8
            E -> Num.bitwiseAnd reg.de 0xFF
            H -> Num.shiftRightZfBy reg.hl 8
            L -> Num.bitwiseAnd reg.hl 0xFF
    )
    |> Num.toU8

read16 : Type16, Register -> U16
read16 = \type, reg ->
    when type is
        AF -> reg.af
        BC -> reg.bc
        DE -> reg.de
        HL -> reg.hl
        SP -> reg.stackPointer
        PC -> reg.programCounter

write8 : Type8, U16 -> (Register -> Register)
write8 = \type, value ->
    \reg ->
        when type is
            A -> { reg & af: Num.bitwiseAnd reg.af 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
            F -> { reg & af: Num.bitwiseAnd reg.af 0xFF00 |> Num.bitwiseOr value }
            B -> { reg & bc: Num.bitwiseAnd reg.bc 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
            C -> { reg & bc: Num.bitwiseAnd reg.bc 0xFF00 |> Num.bitwiseOr value }
            D -> { reg & de: Num.bitwiseAnd reg.de 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
            E -> { reg & de: Num.bitwiseAnd reg.de 0xFF00 |> Num.bitwiseOr value }
            H -> { reg & hl: Num.bitwiseAnd reg.hl 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
            L -> { reg & hl: Num.bitwiseAnd reg.hl 0xFF00 |> Num.bitwiseOr value }

write16 : Type16, U16 -> (Register -> Register)
write16 = \type, value ->
    \reg ->
        when type is
            AF -> { reg & af: Num.bitwiseAnd value 0xFFF0 } # The lowest 4 bits are always discarded for the F register as per spec
            BC -> { reg & bc: Num.bitwiseAnd value 0xFFFF }
            DE -> { reg & de: Num.bitwiseAnd value 0xFFFF }
            HL -> { reg & hl: Num.bitwiseAnd value 0xFFFF }
            SP -> { reg & stackPointer: Num.bitwiseAnd value 0xFFFF }
            PC -> { reg & programCounter: Num.bitwiseAnd value 0xFFFF }
