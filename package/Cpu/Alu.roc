import /Cpu/Register/Status as Status

FlagDelta : U8 -> U8

# Result byte and a delta to apply to the flags register
Output : (U8, FlagDelta)
Output16 : (U16, FlagDelta)

test_flags = |(a, b)| (a, b(0))

## Arithmetic Logic Unit
Alu :: [].{
    inc : U8 -> Output
    inc = |operand| {
        result = operand.plus_wrap(1)
        zero_flag = result == 0x00
        half_carry_flag = operand.bitwise_and(0xF) == 0xF
        (result, Status.modify(Value(zero_flag), Value(Bool.False), Value(half_carry_flag), Unchanged))
    }

    dec : U8 -> Output
    dec = |operand| {
        result = operand.minus_wrap(1)
        zero_flag = result == 0x00
        half_carry_flag = operand.bitwise_and(0x0F) == 0x00
        (result, Status.modify(Value(zero_flag), Value(Bool.True), Value(half_carry_flag), Unchanged))
    }

    add : U8, U8 -> Output
    add = |a, b| adc(a, b, Bool.False)

    # Half-carry comes from the 3-way nibble sum: carry-in participates,
    # so it cannot be recovered from a pre-summed operand.
    adc : U8, U8, Bool -> Output
    adc = |a, b, carry_in| {
        cin = if carry_in { 1 } else { 0 }
        sum = a.to_u16().plus(b.to_u16()).plus(cin)
        result = sum.to_u8_wrap()
        zero_flag = result == 0x00
        half_carry_flag = a.bitwise_and(0xF).to_u16().plus(b.bitwise_and(0xF).to_u16()).plus(cin) > 0xF
        carry_flag = sum > 0xFF
        (result, Status.modify(Value(zero_flag), Value(Bool.False), Value(half_carry_flag), Value(carry_flag)))
    }

    sub : U8, U8 -> Output
    sub = |a, b| sbc(a, b, Bool.False)

    sbc : U8, U8, Bool -> Output
    sbc = |a, b, carry_in| {
        cin = if carry_in { 1 } else { 0 }
        subtrahend = b.to_u16().plus(cin)
        result = a.to_u16().minus_wrap(subtrahend).to_u8_wrap()
        zero_flag = result == 0x00
        half_carry_flag = a.bitwise_and(0xF).to_u16() < b.bitwise_and(0xF).to_u16().plus(cin)
        carry_flag = a.to_u16() < subtrahend
        (result, Status.modify(Value(zero_flag), Value(Bool.True), Value(half_carry_flag), Value(carry_flag)))
    }

    # CP: same flags as SUB, accumulator untouched (caller discards the result byte)
    compare : U8, U8 -> Output
    compare = |a, b| sub(a, b)

    and_a : U8, U8 -> Output
    and_a = |a, b| {
        result = a.bitwise_and(b)
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.True), Value(Bool.False)))
    }

    or_a : U8, U8 -> Output
    or_a = |a, b| {
        result = a.bitwise_or(b)
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(Bool.False)))
    }

    xor_a : U8, U8 -> Output
    xor_a = |a, b| {
        result = a.bitwise_xor(b)
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(Bool.False)))
    }

    complement : U8 -> Output
    complement = |a|
        (a.bitwise_not(), Status.modify(Unchanged, Value(Bool.True), Value(Bool.True), Unchanged))

    # DAA is driven by the flags of the preceding ADD/SUB, not the value alone
    daa : U8, Bool, Bool, Bool -> Output
    daa = |a, subtract, half, carry|
        if subtract {
            low = if half { 0x06 } else { 0x00 }
            adjust = if carry { low.bitwise_or(0x60) } else { low }
            result = a.minus_wrap(adjust)
            (result, Status.modify(Value(result == 0x00), Unchanged, Value(Bool.False), Value(carry)))
        } else {
            carry_out = carry or a > 0x99
            high = if carry_out { 0x60 } else { 0x00 }
            adjust = if half or a.bitwise_and(0x0F) > 0x09 { high.bitwise_or(0x06) } else { high }
            result = a.plus_wrap(adjust)
            (result, Status.modify(Value(result == 0x00), Unchanged, Value(Bool.False), Value(carry_out)))
        }

    add16 : U16, U16 -> Output16
    add16 = |operand1, operand2| {
        result = operand1.plus_wrap(operand2)
        half_carry_flag = operand1.bitwise_and(0x0FFF).plus(operand2.bitwise_and(0x0FFF)) > 0x0FFF
        carry_flag = operand1 > U16.minus(0xFFFF, operand2)
        (result, Status.modify(Unchanged, Value(Bool.False), Value(half_carry_flag), Value(carry_flag)))
    }

    # ADD SP, e8 / LD HL, SP+e8: operand is sign-extended, but H/C come from
    # unsigned low-byte arithmetic and Z is always clear
    add_sp : U16, U8 -> Output16
    add_sp = |sp, e8| {
        offset = e8.to_u16()
        signed = if e8 >= 0x80 { offset.bitwise_or(0xFF00) } else { offset }
        result = sp.plus_wrap(signed)
        half_carry_flag = sp.bitwise_and(0xF).plus(offset.bitwise_and(0xF)) > 0xF
        carry_flag = sp.bitwise_and(0xFF).plus(offset.bitwise_and(0xFF)) > 0xFF
        (result, Status.modify(Value(Bool.False), Value(Bool.False), Value(half_carry_flag), Value(carry_flag)))
    }

    # Rotates and shifts: Z from the result (the RLCA/RLA-family accumulator
    # variants force Z=0 at the execute layer)
    rlc : U8 -> Output
    rlc = |v| {
        carry = v.bitwise_and(0x80) != 0x00
        result = v.shl_wrap(1).bitwise_or(if carry { 0x01 } else { 0x00 })
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    rrc : U8 -> Output
    rrc = |v| {
        carry = v.bitwise_and(0x01) != 0x00
        result = v.shr_zf_wrap(1).bitwise_or(if carry { 0x80 } else { 0x00 })
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    rl : U8, Bool -> Output
    rl = |v, carry_in| {
        carry = v.bitwise_and(0x80) != 0x00
        result = v.shl_wrap(1).bitwise_or(if carry_in { 0x01 } else { 0x00 })
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    rr : U8, Bool -> Output
    rr = |v, carry_in| {
        carry = v.bitwise_and(0x01) != 0x00
        result = v.shr_zf_wrap(1).bitwise_or(if carry_in { 0x80 } else { 0x00 })
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    sla : U8 -> Output
    sla = |v| {
        carry = v.bitwise_and(0x80) != 0x00
        result = v.shl_wrap(1)
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    sra : U8 -> Output
    sra = |v| {
        carry = v.bitwise_and(0x01) != 0x00
        result = v.shr_zf_wrap(1).bitwise_or(v.bitwise_and(0x80))
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    srl : U8 -> Output
    srl = |v| {
        carry = v.bitwise_and(0x01) != 0x00
        result = v.shr_zf_wrap(1)
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(carry)))
    }

    swap : U8 -> Output
    swap = |v| {
        result = v.shr_zf_wrap(4).bitwise_or(v.shl_wrap(4))
        (result, Status.modify(Value(result == 0x00), Value(Bool.False), Value(Bool.False), Value(Bool.False)))
    }

    bit_test : U8, U8 -> FlagDelta
    bit_test = |index, v| {
        zero_flag = v.bitwise_and(U8.shl_wrap(1, index)) == 0x00
        Status.modify(Value(zero_flag), Value(Bool.False), Value(Bool.True), Unchanged)
    }

    set_bit : U8, U8 -> U8
    set_bit = |index, v| v.bitwise_or(U8.shl_wrap(1, index))

    clear_bit : U8, U8 -> U8
    clear_bit = |index, v| v.bitwise_and(U8.shl_wrap(1, index).bitwise_not())
}

expect test_flags(Alu.inc(0)) == (1, 0b00000000)
expect test_flags(Alu.inc(15)) == (16, 0b00100000)
expect test_flags(Alu.inc(128)) == (129, 0b00000000)
expect test_flags(Alu.inc(255)) == (0, 0b10100000)

expect test_flags(Alu.dec(1)) == (0, 0b11000000)
expect test_flags(Alu.dec(16)) == (15, 0b01100000)
expect test_flags(Alu.dec(129)) == (128, 0b01000000)
expect test_flags(Alu.dec(0)) == (255, 0b01100000)

expect test_flags(Alu.add(1, 1)) == (2, 0b00000000)
expect test_flags(Alu.add(15, 1)) == (16, 0b00100000)
expect test_flags(Alu.add(128, 128)) == (0, 0b10010000)
expect test_flags(Alu.add(255, 1)) == (0, 0b10110000)

# Carry-in participates in the nibble sum
expect test_flags(Alu.adc(0x0F, 0x00, Bool.True)) == (0x10, 0b00100000)
expect test_flags(Alu.adc(0xFF, 0xFF, Bool.True)) == (0xFF, 0b00110000)
expect test_flags(Alu.adc(0xFF, 0x00, Bool.True)) == (0x00, 0b10110000)
expect test_flags(Alu.adc(0x00, 0x00, Bool.False)) == (0x00, 0b10000000)

expect test_flags(Alu.sub(255, 1)) == (254, 0b01000000)
expect test_flags(Alu.sub(1, 1)) == (0, 0b11000000)
expect test_flags(Alu.sub(1, 255)) == (2, 0b01110000)
expect test_flags(Alu.sub(16, 1)) == (15, 0b01100000)

expect test_flags(Alu.sbc(0x00, 0x00, Bool.True)) == (0xFF, 0b01110000)
expect test_flags(Alu.sbc(0x10, 0x0F, Bool.True)) == (0x00, 0b11100000)
expect test_flags(Alu.sbc(0xFF, 0xFF, Bool.True)) == (0xFF, 0b01110000)

expect test_flags(Alu.compare(0x3C, 0x3C)) == (0x00, 0b11000000)

expect test_flags(Alu.and_a(0xF0, 0x0F)) == (0x00, 0b10100000)
expect test_flags(Alu.and_a(0xFC, 0x0F)) == (0x0C, 0b00100000)
expect test_flags(Alu.or_a(0xF0, 0x0F)) == (0xFF, 0b00000000)
expect test_flags(Alu.or_a(0x00, 0x00)) == (0x00, 0b10000000)
expect test_flags(Alu.xor_a(0xFF, 0xFF)) == (0x00, 0b10000000)
expect test_flags(Alu.xor_a(0xF0, 0x0F)) == (0xFF, 0b00000000)

expect test_flags(Alu.complement(0b10100101)) == (0b01011010, 0b01100000)

# DAA: 0x15 + 0x27 = 0x3C (no flags) then DAA -> 0x42
expect test_flags(Alu.daa(0x3C, Bool.False, Bool.False, Bool.False)) == (0x42, 0b00000000)
# DAA after BCD subtraction: 0x20 - 0x13 = 0x0D (N, H set) -> 0x07
expect test_flags(Alu.daa(0x0D, Bool.True, Bool.True, Bool.False)) == (0x07, 0b00000000)
# DAA after 0x99 + 0x99 = 0x32 (H, C set) -> 0x98 with carry
expect test_flags(Alu.daa(0x32, Bool.False, Bool.True, Bool.True)) == (0x98, 0b00010000)
# DAA result of zero sets Z
expect test_flags(Alu.daa(0xA0, Bool.False, Bool.False, Bool.False)) == (0x00, 0b10010000)

expect test_flags(Alu.add16(1, 1)) == (2, 0b00000000)
expect test_flags(Alu.add16(32768, 32768)) == (0, 0b00010000)
expect test_flags(Alu.add16(65535, 1)) == (0, 0b00110000)
expect test_flags(Alu.add16(0x0FFF, 0x0001)) == (0x1000, 0b00100000)

# ADD SP, e8: Z=0, N=0, H/C from unsigned low-byte arithmetic
expect test_flags(Alu.add_sp(0xFFF8, 0x08)) == (0x0000, 0b00110000)
expect test_flags(Alu.add_sp(0xFFF8, 0xFE)) == (0xFFF6, 0b00110000)
expect test_flags(Alu.add_sp(0x0000, 0x01)) == (0x0001, 0b00000000)

expect test_flags(Alu.rlc(0x85)) == (0x0B, 0b00010000)
expect test_flags(Alu.rlc(0x00)) == (0x00, 0b10000000)
expect test_flags(Alu.rrc(0x01)) == (0x80, 0b00010000)
expect test_flags(Alu.rl(0x80, Bool.False)) == (0x00, 0b10010000)
expect test_flags(Alu.rl(0x80, Bool.True)) == (0x01, 0b00010000)
expect test_flags(Alu.rr(0x01, Bool.True)) == (0x80, 0b00010000)
expect test_flags(Alu.sla(0xFF)) == (0xFE, 0b00010000)
expect test_flags(Alu.sra(0x81)) == (0xC0, 0b00010000)
expect test_flags(Alu.srl(0x81)) == (0x40, 0b00010000)
expect test_flags(Alu.srl(0x01)) == (0x00, 0b10010000)
expect test_flags(Alu.swap(0xF0)) == (0x0F, 0b00000000)
expect test_flags(Alu.swap(0x00)) == (0x00, 0b10000000)

expect Alu.bit_test(7, 0x7F)(0) == 0b10100000
expect Alu.bit_test(0, 0x01)(0) == 0b00100000
# BIT leaves carry unchanged
expect Alu.bit_test(7, 0x80)(0b00010000) == 0b00110000

expect Alu.set_bit(3, 0x00) == 0x08
expect Alu.clear_bit(3, 0xFF) == 0xF7
