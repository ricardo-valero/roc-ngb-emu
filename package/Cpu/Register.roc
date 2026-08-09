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
    interrupt_master_enable : Bool, # IME
    halted : Bool,
    interrupt_flag : U8,
    interrupt_enable : U8,
}.{
    Type8 := [Accumulator, Status, B, C, D, E, H, L]
    Type16 := [ProgramCounter, StackPointer, AccumulatorStatus, BC, DE, HL]

    read16 : Type16 -> (Register -> U16)
    read16 = |type| |reg|
        match type {
            ProgramCounter => reg.program_counter
            StackPointer => reg.stack_pointer
            AccumulatorStatus => reg.accumulator.to_u16().shl_wrap(8).bitwise_or(reg.status.to_u16())
            # I don't know if this is a good approach or I'm exaggerating
            BC => read8(B)(reg).to_u16().shl_wrap(8).bitwise_or(read8(C)(reg).to_u16())
            DE => reg.d.to_u16().shl_wrap(8).bitwise_or(reg.e.to_u16())
            HL => reg.h.to_u16().shl_wrap(8).bitwise_or(reg.l.to_u16())
        }

    write16 : Type16, U16 -> (Register -> Register)
    write16 = |type, value| |reg|
        match type {
            ProgramCounter => { ..reg, program_counter: value.bitwise_and(0xFFFF) }
            StackPointer => { ..reg, stack_pointer: value.bitwise_and(0xFFFF) }
            AccumulatorStatus => { ..reg, accumulator: value.shr_zf_wrap(8).to_u8_wrap(), status: value.bitwise_and(0xF0).to_u8_wrap() } # Discard the lowest 4 bits as per spec
            # Same here, I don't know if this is a good approach
            BC => write8(C, value.bitwise_and(0xFF).to_u8_wrap())(write8(B, value.shr_zf_wrap(8).to_u8_wrap())(reg))
            DE => { ..reg, d: value.shr_zf_wrap(8).to_u8_wrap(), e: value.bitwise_and(0xFF).to_u8_wrap() }
            HL => { ..reg, h: value.shr_zf_wrap(8).to_u8_wrap(), l: value.bitwise_and(0xFF).to_u8_wrap() }
        }

    read8 : Type8 -> (Register -> U8)
    read8 = |type| |reg|
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

    write8 : Type8, U8 -> (Register -> Register)
    write8 = |type, value| |reg|
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
