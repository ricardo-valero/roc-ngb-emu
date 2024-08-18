module [Register, Type8, Type16, init, read8, read16, write8, write16]

# 8-Bit and 16-Bit
Type8 : [A, F, B, C, D, E, H, L]
Type16 : [AF, BC, DE, HL, SP, PC]

Register : {
    af : U16,
    bc : U16,
    de : U16,
    hl : U16,
    pc : U16,
    sp : U16,
    halted : Bool,
    interruptFlag : U16,
    interruptEnable : U16,
    interruptMasterEnable : Bool,
}

init : Register
init = {
    af: 0x01B0,
    bc: 0x13,
    de: 0xD8,
    hl: 0x014D,
    pc: 0x0100,
    sp: 0xFFFE,
    halted: Bool.false,
    interruptFlag: 0xE1,
    interruptEnable: 0x00,
    interruptMasterEnable: Bool.true,
}

read8 : Register, Type8 -> U16
read8 = \cpu, type ->
    when type is
        A -> Num.shiftRightZfBy cpu.af 8
        F -> Num.bitwiseAnd cpu.af 0xFF
        B -> Num.shiftRightZfBy cpu.bc 8
        C -> Num.bitwiseAnd cpu.bc 0xFF
        D -> Num.shiftRightZfBy cpu.de 8
        E -> Num.bitwiseAnd cpu.de 0xFF
        H -> Num.shiftRightZfBy cpu.hl 8
        L -> Num.bitwiseAnd cpu.hl 0xFF

read16 : Register, Type16 -> U16
read16 = \cpu, type ->
    when type is
        AF -> cpu.af
        BC -> cpu.bc
        DE -> cpu.de
        HL -> cpu.hl
        SP -> cpu.sp
        PC -> cpu.pc

write8 : U16, Register, Type8 -> Register
write8 = \value, cpu, type ->
    when type is
        A -> { cpu & af: Num.bitwiseAnd cpu.af 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
        F -> { cpu & af: Num.bitwiseAnd cpu.af 0xFF00 |> Num.bitwiseOr value }
        B -> { cpu & bc: Num.bitwiseAnd cpu.bc 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
        C -> { cpu & bc: Num.bitwiseAnd cpu.bc 0xFF00 |> Num.bitwiseOr value }
        D -> { cpu & de: Num.bitwiseAnd cpu.de 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
        E -> { cpu & de: Num.bitwiseAnd cpu.de 0xFF00 |> Num.bitwiseOr value }
        H -> { cpu & hl: Num.bitwiseAnd cpu.hl 0xFF |> Num.bitwiseOr (Num.shiftLeftBy value 8) }
        L -> { cpu & hl: Num.bitwiseAnd cpu.hl 0xFF00 |> Num.bitwiseOr value }

write16 : U16, Register, Type16 -> Register
write16 = \value, cpu, type ->
    when type is
        AF -> { cpu & af: Num.bitwiseAnd value 0xFFF0 } # The lowest 4 bits are always discarded for the F register as per spec
        BC -> { cpu & bc: Num.bitwiseAnd value 0xFFFF }
        DE -> { cpu & de: Num.bitwiseAnd value 0xFFFF }
        HL -> { cpu & hl: Num.bitwiseAnd value 0xFFFF }
        SP -> { cpu & sp: Num.bitwiseAnd value 0xFFFF }
        PC -> { cpu & pc: Num.bitwiseAnd value 0xFFFF }
