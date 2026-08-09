# 8-Bit and 16-Bit registers
# A: Accumulator
# F: Status (Commonly known as Flags)
# SP: Stack Pointer
# PC: Program Counter
Register := {
    program_counter : U16,
    stack_pointer : U16,
    accumulator : U8,
    status : U8,
    b : U8,
    c : U8,
    d : U8,
    e : U8,
    h : U8,
    l : U8,
}.{
    Type8 := [Accumulator, Status, B, C, D, E, H, L]
    Type16 := [ProgramCounter, StackPointer, AccumulatorStatus, BC, DE, HL]

    # DMG post-boot values (no boot ROM)
    init : {} -> Register
    init = |_| {
        program_counter: 0x0100,
        stack_pointer: 0xFFFE,
        accumulator: 0x01,
        status: 0xB0,
        b: 0x00,
        c: 0x13,
        d: 0x00,
        e: 0xD8,
        h: 0x01,
        l: 0x4D,
    }

    read16 : Register, Type16 -> U16
    read16 = |reg, type|
        match type {
            ProgramCounter => reg.program_counter
            StackPointer => reg.stack_pointer
            AccumulatorStatus => reg.accumulator.to_u16().shl_wrap(8).bitwise_or(reg.status.to_u16())
            BC => reg.b.to_u16().shl_wrap(8).bitwise_or(reg.c.to_u16())
            DE => reg.d.to_u16().shl_wrap(8).bitwise_or(reg.e.to_u16())
            HL => reg.h.to_u16().shl_wrap(8).bitwise_or(reg.l.to_u16())
        }

    write16 : Register, Type16, U16 -> Register
    write16 = |reg, type, value|
        match type {
            ProgramCounter => { ..reg, program_counter: value }
            StackPointer => { ..reg, stack_pointer: value }
            AccumulatorStatus => { ..reg, accumulator: value.shr_zf_wrap(8).to_u8_wrap(), status: value.bitwise_and(0xF0).to_u8_wrap() } # Discard the lowest 4 bits as per spec
            BC => { ..reg, b: value.shr_zf_wrap(8).to_u8_wrap(), c: value.to_u8_wrap() }
            DE => { ..reg, d: value.shr_zf_wrap(8).to_u8_wrap(), e: value.to_u8_wrap() }
            HL => { ..reg, h: value.shr_zf_wrap(8).to_u8_wrap(), l: value.to_u8_wrap() }
        }

    read8 : Register, Type8 -> U8
    read8 = |reg, type|
        match type {
            Accumulator => reg.accumulator
            Status => reg.status
            B => reg.b
            C => reg.c
            D => reg.d
            E => reg.e
            H => reg.h
            L => reg.l
        }

    write8 : Register, Type8, U8 -> Register
    write8 = |reg, type, value|
        match type {
            Accumulator => { ..reg, accumulator: value }
            Status => { ..reg, status: value.bitwise_and(0xF0) } # Discard the lowest 4 bits as per spec
            B => { ..reg, b: value }
            C => { ..reg, c: value }
            D => { ..reg, d: value }
            E => { ..reg, e: value }
            H => { ..reg, h: value }
            L => { ..reg, l: value }
        }
}

expect Register.init({}).read16(AccumulatorStatus) == 0x01B0
expect Register.init({}).write16(BC, 0x1234).read8(C) == 0x34
expect Register.init({}).write16(AccumulatorStatus, 0xABCD).read16(AccumulatorStatus) == 0xABC0
expect Register.init({}).write8(H, 0x12).write8(L, 0x34).read16(HL) == 0x1234
