module [Register, Type8, Type16, read8, read16, write8, write16]

# 8-Bit and 16-Bit registers
# A: Accumulator
# F: Status (Commonly known as Flags)
# SP: Stack Pointer
# PC: Program Counter
Type8 : [Accumulator, Status, B, C, D, E, H, L]
Type16 : [ProgramCounter, StackPointer, AccumulatorStatus, BC, DE, HL]

Register : {
    programCounter : U16,
    stackPointer : U16,
    accumulator : U8,
    status : U8,
    b : U8,
    c : U8,
    d : U8,
    e : U8,
    h : U8,
    l : U8,
    interruptMasterEnable : Bool, # IME
    halted : Bool,
    interruptFlag : U8,
    interruptEnable : U8,
}

read16 : Type16 -> (Register -> U16)
read16 = \type -> \reg ->
        when type is
            ProgramCounter -> reg.programCounter
            StackPointer -> reg.stackPointer
            AccumulatorStatus -> (Num.shiftLeftBy (Num.toU16 reg.accumulator) 8) |> Num.bitwiseOr (Num.toU16 reg.status)
            # I don't know if this is a good approach or I'm exaggerating
            BC -> (Num.shiftLeftBy (reg |> (read8 B) |> Num.toU16) 8) |> Num.bitwiseOr (reg |> (read8 C) |> Num.toU16)
            DE -> (Num.shiftLeftBy (Num.toU16 reg.d) 8) |> Num.bitwiseOr (Num.toU16 reg.e)
            HL -> (Num.shiftLeftBy (Num.toU16 reg.h) 8) |> Num.bitwiseOr (Num.toU16 reg.l)

write16 : Type16, U16 -> (Register -> Register)
write16 = \type, value -> \reg ->
        when type is
            ProgramCounter -> { reg & programCounter: Num.bitwiseAnd value 0xFFFF }
            StackPointer -> { reg & stackPointer: Num.bitwiseAnd value 0xFFFF }
            AccumulatorStatus -> { reg & accumulator: Num.shiftRightZfBy value 8 |> Num.toU8, status: Num.bitwiseAnd value 0xF0 |> Num.toU8 } # Discard the lowest 4 bits as per spec
            # Same here, I don't know if this is a good approach
            BC -> reg |> (write8 B (Num.shiftRightZfBy value 8 |> Num.toU8)) |> (write8 C (Num.bitwiseAnd value 0xFF |> Num.toU8))
            DE -> { reg & d: Num.shiftRightZfBy value 8 |> Num.toU8, e: Num.bitwiseAnd value 0xFF |> Num.toU8 }
            HL -> { reg & h: Num.shiftRightZfBy value 8 |> Num.toU8, l: Num.bitwiseAnd value 0xFF |> Num.toU8 }

read8 : Type8 -> (Register -> U8)
read8 = \type -> \reg ->
        when type is
            Accumulator -> reg.accumulator
            Status -> reg.status
            B -> reg.b
            C -> reg.c
            D -> reg.d
            E -> reg.e
            H -> reg.h
            L -> reg.l

write8 : Type8, U8 -> (Register -> Register)
write8 = \type, value -> \reg ->
        when type is
            Accumulator -> { reg & accumulator: value }
            Status -> { reg & status: Num.bitwiseAnd value 0xF0 } # Discard the lowest 4 bits as per spec
            B -> { reg & b: value }
            C -> { reg & c: value }
            D -> { reg & d: value }
            E -> { reg & e: value }
            H -> { reg & h: value }
            L -> { reg & l: value }
