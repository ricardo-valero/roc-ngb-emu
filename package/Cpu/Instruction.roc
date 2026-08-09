## Gameboy CPU (LR35902) instruction set
# https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
# https://gbdev.io/gb-opcodes/optables/

import /Cpu/Register/Status

# Structural copies of Status.Member / Register.Type8 / Register.Type16: the
# nightly compiler cannot yet reference nested types through subdirectory
# imports (works for root-level modules only). Values still flow into the
# nominal types at call sites. Swap back to the qualified names once fixed.
Condition : [Always, Status([Zero, Subtract, HalfCarry, Carry], Bool)]

check_condition : Condition, U8 -> Bool
check_condition = |condition, flag|
    match condition {
        Always => Bool.True
        Status(member, value) => Status.check(member, flag) == value
    }

AddressingMode : [
    Immediate,
    Direct8([Accumulator, Status, B, C, D, E, H, L]),
    Direct16([ProgramCounter, StackPointer, AccumulatorStatus, BC, DE, HL]),
    Indirect([C, BC, DE, HL, HLPostDecrement, HLPostIncrement, Word8Operand, Word16Operand]),
]

Instruction := [
    # Other
    Prefix,
    Illegal,
    Unknown,
    # Misc / control instructions
    Nop,
    Stop,
    Halt,
    Interrupts([Enable, Disable]), # (Ei, Di)
    CarryFlag([Set, Complement]), # (Scf, Ccf) # Set or toggle the carry flag
    DecimalAdjustAccumulator, # Daa
    ComplementAccumulator, # Cpl # Flip all bits from the flags register
    # Jumps / calls
    Jump(Condition, [Immediate, HL]), # Jp (JP HL jumps to the HL value, no memory read)
    Branch(Condition), # Jr (I renamed jump relative to branch)
    Call(Condition),
    Return(Condition), # Ret
    ReturnAndEnableInterrupts, # Reti
    Restart(U8), # Rst
    # 8-bit shift, rotate and bit
    RotateCircularAccumulator([Left, Right]), # Rlca & Rrca
    RotateAccumulator([Left, Right]), # Rla & Rra
    # 8-bit arithmetic / logical
    Or(AddressingMode),
    And(AddressingMode),
    Xor(AddressingMode),
    Add(AddressingMode),
    Adc(AddressingMode),
    Sub(AddressingMode),
    Sbc(AddressingMode),
    Compare(AddressingMode), # Cp
    Inc(AddressingMode, AddressingMode),
    Dec(AddressingMode, AddressingMode),
    # 16-bit arithmetic / logical
    Add16(AddressingMode),
    Dec16(AddressingMode, AddressingMode),
    Inc16(AddressingMode, AddressingMode),
    AddStackPointerImmediate, # Add SP, e8
    # 8-bit load
    Load(AddressingMode, AddressingMode), # Ld
    # 16-bit load
    Load16(AddressingMode, AddressingMode), # Ld
    LoadHLStackPointerImmediate, # Ld HL, SP+e8
    # Stack
    Push(AddressingMode),
    Pop(AddressingMode),
].{
    # Operand order shared by the regular decode blocks and the CB table:
    # bits 2-0 (or 5-3 for destinations) index B,C,D,E,H,L,(HL),A
    operand_mode : U8 -> AddressingMode
    operand_mode = |index|
        match index {
            0 => Direct8(B)
            1 => Direct8(C)
            2 => Direct8(D)
            3 => Direct8(E)
            4 => Direct8(H)
            5 => Direct8(L)
            6 => Indirect(HL)
            _ => Direct8(Accumulator)
        }

    # CB-prefixed decode, exposed as a method so other modules can reach it
    lookup_prefixed : U8 -> Prefixed
    lookup_prefixed = |byte| prefixed(byte)

    # Branch/jump/call/return condition against the flags register
    condition_met : Condition, U8 -> Bool
    condition_met = |condition, flag| check_condition(condition, flag)

    lookup : U8 -> Instruction
    lookup = |byte|
        if byte >= 0x40 and byte <= 0x7F {
            # Load quadrant: 0b01_ddd_sss (0x76 would be LD (HL),(HL) and is HALT)
            if byte == 0x76 {
                Halt
            } else {
                Load(operand_mode(byte.shr_zf_wrap(3).bitwise_and(0x07)), operand_mode(byte.bitwise_and(0x07)))
            }
        } else if byte >= 0x80 and byte <= 0xBF {
            # ALU block: 0b10_ooo_sss
            mode = operand_mode(byte.bitwise_and(0x07))
            match byte.shr_zf_wrap(3).bitwise_and(0x07) {
                0 => Add(mode)
                1 => Adc(mode)
                2 => Sub(mode)
                3 => Sbc(mode)
                4 => And(mode)
                5 => Xor(mode)
                6 => Or(mode)
                _ => Compare(mode)
            }
        } else {
            match byte {
                0x00 => Nop
                0x10 => Stop
                0xE6 => And(Immediate)
                0xEE => Xor(Immediate)
                0xF6 => Or(Immediate)
                0xC6 => Add(Immediate)
                0xCE => Adc(Immediate)
                0xD6 => Sub(Immediate)
                0xDE => Sbc(Immediate)
                0xFE => Compare(Immediate)
                0x04 => Inc(Direct8(B), Direct8(B))
                0x0C => Inc(Direct8(C), Direct8(C))
                0x14 => Inc(Direct8(D), Direct8(D))
                0x1C => Inc(Direct8(E), Direct8(E))
                0x24 => Inc(Direct8(H), Direct8(H))
                0x2C => Inc(Direct8(L), Direct8(L))
                0x34 => Inc(Indirect(HL), Indirect(HL))
                0x3C => Inc(Direct8(Accumulator), Direct8(Accumulator))
                0x05 => Dec(Direct8(B), Direct8(B))
                0x0D => Dec(Direct8(C), Direct8(C))
                0x15 => Dec(Direct8(D), Direct8(D))
                0x1D => Dec(Direct8(E), Direct8(E))
                0x25 => Dec(Direct8(H), Direct8(H))
                0x2D => Dec(Direct8(L), Direct8(L))
                0x35 => Dec(Indirect(HL), Indirect(HL))
                0x3D => Dec(Direct8(Accumulator), Direct8(Accumulator))
                0x09 => Add16(Direct16(BC))
                0x19 => Add16(Direct16(DE))
                0x29 => Add16(Direct16(HL))
                0x39 => Add16(Direct16(StackPointer))
                0x03 => Inc16(Direct16(BC), Direct16(BC))
                0x13 => Inc16(Direct16(DE), Direct16(DE))
                0x23 => Inc16(Direct16(HL), Direct16(HL))
                0x33 => Inc16(Direct16(StackPointer), Direct16(StackPointer))
                0x0B => Dec16(Direct16(BC), Direct16(BC))
                0x1B => Dec16(Direct16(DE), Direct16(DE))
                0x2B => Dec16(Direct16(HL), Direct16(HL))
                0x3B => Dec16(Direct16(StackPointer), Direct16(StackPointer))
                0x07 => RotateCircularAccumulator(Left)
                0x0F => RotateCircularAccumulator(Right)
                0x17 => RotateAccumulator(Left)
                0x1F => RotateAccumulator(Right)
                0x02 => Load(Indirect(BC), Direct8(Accumulator))
                0x06 => Load(Direct8(B), Immediate)
                0x0A => Load(Direct8(Accumulator), Indirect(BC))
                0x0E => Load(Direct8(C), Immediate)
                0x12 => Load(Indirect(DE), Direct8(Accumulator))
                0x16 => Load(Direct8(D), Immediate)
                0x1A => Load(Direct8(Accumulator), Indirect(DE))
                0x1E => Load(Direct8(E), Immediate)
                0x22 => Load(Indirect(HLPostIncrement), Direct8(Accumulator))
                0x26 => Load(Direct8(H), Immediate)
                0x2A => Load(Direct8(Accumulator), Indirect(HLPostIncrement))
                0x2E => Load(Direct8(L), Immediate)
                0x32 => Load(Indirect(HLPostDecrement), Direct8(Accumulator))
                0x36 => Load(Indirect(HL), Immediate)
                0x3A => Load(Direct8(Accumulator), Indirect(HLPostDecrement))
                0x3E => Load(Direct8(Accumulator), Immediate)
                0xE2 => Load(Indirect(C), Direct8(Accumulator))
                0xEA => Load(Indirect(Word16Operand), Direct8(Accumulator))
                0xF0 => Load(Direct8(Accumulator), Indirect(Word8Operand))
                0xF2 => Load(Direct8(Accumulator), Indirect(C))
                0xFA => Load(Direct8(Accumulator), Indirect(Word16Operand))
                0xE0 => Load(Indirect(Word8Operand), Direct8(Accumulator))
                0x01 => Load16(Direct16(BC), Immediate)
                0x08 => Load16(Indirect(Word16Operand), Direct16(StackPointer))
                0x11 => Load16(Direct16(DE), Immediate)
                0x21 => Load16(Direct16(HL), Immediate)
                0x31 => Load16(Direct16(StackPointer), Immediate)
                0xF9 => Load16(Direct16(StackPointer), Direct16(HL))
                0xC3 => Jump(Always, Immediate)
                0xC2 => Jump(Status(Zero, Bool.False), Immediate)
                0xCA => Jump(Status(Zero, Bool.True), Immediate)
                0xD2 => Jump(Status(Carry, Bool.False), Immediate)
                0xDA => Jump(Status(Carry, Bool.True), Immediate)
                0xE9 => Jump(Always, HL)
                0x18 => Branch(Always)
                0x20 => Branch(Status(Zero, Bool.False))
                0x28 => Branch(Status(Zero, Bool.True))
                0x30 => Branch(Status(Carry, Bool.False))
                0x38 => Branch(Status(Carry, Bool.True))
                0xCD => Call(Always)
                0xC4 => Call(Status(Zero, Bool.False))
                0xCC => Call(Status(Zero, Bool.True))
                0xD4 => Call(Status(Carry, Bool.False))
                0xDC => Call(Status(Carry, Bool.True))
                0xC9 => Return(Always)
                0xC0 => Return(Status(Zero, Bool.False))
                0xC8 => Return(Status(Zero, Bool.True))
                0xD0 => Return(Status(Carry, Bool.False))
                0xD8 => Return(Status(Carry, Bool.True))
                0xD9 => ReturnAndEnableInterrupts
                0xC1 => Pop(Direct16(BC))
                0xD1 => Pop(Direct16(DE))
                0xE1 => Pop(Direct16(HL))
                0xF1 => Pop(Direct16(AccumulatorStatus))
                0xC5 => Push(Direct16(BC))
                0xD5 => Push(Direct16(DE))
                0xE5 => Push(Direct16(HL))
                0xF5 => Push(Direct16(AccumulatorStatus))
                0xCB => Prefix
                0xF3 => Interrupts(Disable)
                0xFB => Interrupts(Enable)
                0x27 => DecimalAdjustAccumulator
                0x2F => ComplementAccumulator
                0x37 => CarryFlag(Set)
                0x3F => CarryFlag(Complement)
                0xC7 => Restart(0x00)
                0xCF => Restart(0x08)
                0xD7 => Restart(0x10)
                0xDF => Restart(0x18)
                0xE7 => Restart(0x20)
                0xEF => Restart(0x28)
                0xF7 => Restart(0x30)
                0xFF => Restart(0x38)
                0xE8 => AddStackPointerImmediate
                0xF8 => LoadHLStackPointerImmediate
                0xD3 => Illegal
                0xDB => Illegal
                0xDD => Illegal
                0xE3 => Illegal
                0xE4 => Illegal
                0xEB => Illegal
                0xEC => Illegal
                0xED => Illegal
                0xF4 => Illegal
                0xFC => Illegal
                0xFD => Illegal
                _ => Unknown
            }
        }
}

# 8-bit shift, rotate and bit instructions (CB-prefixed)
Prefixed : [
    Bit(U8, AddressingMode),
    Set(U8, AddressingMode),
    Reset(U8, AddressingMode), # Res
    Rotate([Left, Right], AddressingMode), # Rl & Rr
    RotateCircular([Left, Right], AddressingMode), # Rlc & Rrc
    ShiftArithmetic([Left, Right], AddressingMode), # Sla & Sra
    ShiftLogical([Right], AddressingMode), # Srl
    Swap(AddressingMode),
]

# The CB table is pure structure: 0b_ff_iii_sss
# ff = op family, iii = bit index / sub-op, sss = operand
prefixed : U8 -> Prefixed
prefixed = |byte| {
    index = byte.shr_zf_wrap(3).bitwise_and(0x07)
    mode = Instruction.operand_mode(byte.bitwise_and(0x07))
    match byte.shr_zf_wrap(6) {
        0 =>
            match index {
                0 => RotateCircular(Left, mode)
                1 => RotateCircular(Right, mode)
                2 => Rotate(Left, mode)
                3 => Rotate(Right, mode)
                4 => ShiftArithmetic(Left, mode)
                5 => ShiftArithmetic(Right, mode)
                6 => Swap(mode)
                _ => ShiftLogical(Right, mode)
            }

        1 => Bit(index, mode)
        2 => Reset(index, mode)
        _ => Set(index, mode)
    }
}

# Fold a predicate over all 256 byte values (no stdlib range dependency)
count_bytes : U16, (U8 -> Bool) -> U16
count_bytes = |from, pred|
    if from > 0xFF {
        0
    } else {
        rest = count_bytes(from.plus(1), pred)
        if pred(from.to_u8_wrap()) { rest.plus(1) } else { rest }
    }

# Exhaustive decode: no opcode is Unknown, exactly the 11 documented illegals
expect count_bytes(0, |b| match Instruction.lookup(b) { Unknown => Bool.True, _ => Bool.False }) == 0
expect count_bytes(0, |b| match Instruction.lookup(b) { Illegal => Bool.True, _ => Bool.False }) == 11

# Spot checks across the computed regions (match: the nominal type has no derived ==)
decodes_to : U8, (Instruction -> Bool) -> Bool
decodes_to = |byte, pred| pred(Instruction.lookup(byte))

expect decodes_to(0x76, |i| match i { Halt => Bool.True, _ => Bool.False })
expect decodes_to(0x41, |i| match i { Load(Direct8(B), Direct8(C)) => Bool.True, _ => Bool.False })
expect decodes_to(0x6E, |i| match i { Load(Direct8(L), Indirect(HL)) => Bool.True, _ => Bool.False })
expect decodes_to(0x77, |i| match i { Load(Indirect(HL), Direct8(Accumulator)) => Bool.True, _ => Bool.False })
expect decodes_to(0x87, |i| match i { Add(Direct8(Accumulator)) => Bool.True, _ => Bool.False })
expect decodes_to(0x9E, |i| match i { Sbc(Indirect(HL)) => Bool.True, _ => Bool.False })
expect decodes_to(0xB8, |i| match i { Compare(Direct8(B)) => Bool.True, _ => Bool.False })
expect decodes_to(0xE8, |i| match i { AddStackPointerImmediate => Bool.True, _ => Bool.False })
expect decodes_to(0xF8, |i| match i { LoadHLStackPointerImmediate => Bool.True, _ => Bool.False })
expect decodes_to(0xF9, |i| match i { Load16(Direct16(StackPointer), Direct16(HL)) => Bool.True, _ => Bool.False })
expect decodes_to(0xE9, |i| match i { Jump(Always, HL) => Bool.True, _ => Bool.False })

# CB structural decode: every family lands on the documented opcodes
expect prefixed(0x00) == RotateCircular(Left, Direct8(B))
expect prefixed(0x0F) == RotateCircular(Right, Direct8(Accumulator))
expect prefixed(0x16) == Rotate(Left, Indirect(HL))
expect prefixed(0x27) == ShiftArithmetic(Left, Direct8(Accumulator))
expect prefixed(0x36) == Swap(Indirect(HL))
expect prefixed(0x3F) == ShiftLogical(Right, Direct8(Accumulator))
expect prefixed(0x7E) == Bit(7, Indirect(HL))
expect prefixed(0x80) == Reset(0, Direct8(B))
expect prefixed(0xFF) == Set(7, Direct8(Accumulator))
expect count_bytes(0, |b| match prefixed(b) { Bit(_, _) => Bool.True, _ => Bool.False }) == 64
expect count_bytes(0, |b| match prefixed(b) { Set(_, _) => Bool.True, _ => Bool.False }) == 64
expect count_bytes(0, |b| match prefixed(b) { Reset(_, _) => Bool.True, _ => Bool.False }) == 64
expect count_bytes(0, |b| match prefixed(b) { Swap(_) => Bool.True, _ => Bool.False }) == 8
