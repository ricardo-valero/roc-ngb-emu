import /Cpu/Register/Status as Status

FlagDelta : U8 -> U8
# Dir : [Left, Right]

# Accumulator and Flag delta
Output : (U8, FlagDelta)
Output16 : (U16, FlagDelta)

test_flags = |(a, b)| (a, b(0))

## Arithmetic Logic Unit
Alu :: [].{
    inc : U8 -> Output
    inc = |operand| {
        result = operand.plus_wrap(1)
        masked = result.bitwise_and(0xFF)
        zero_flag = masked == 0x00
        half_carry_flag = operand.bitwise_and(0xF).plus(1) > 0xF
        (masked, Status.modify(Value(zero_flag), Value(Bool.False), Value(half_carry_flag), Unchanged))
    }

    dec : U8 -> Output
    dec = |operand| {
        result = operand.minus_wrap(1)
        masked = result.bitwise_and(0xFF)
        zero_flag = masked == 0x00
        half_carry_flag = operand.bitwise_and(0x0F) == 0x00
        (masked, Status.modify(Value(zero_flag), Value(Bool.True), Value(half_carry_flag), Unchanged))
    }

    # TODO: Unsure if this is the right approach
    # - Should I use Num.addChecked?
    # - How to calculate the carry flag without converting to U16?
    # - This also works for the adc op, but is it worth it?
    add : U8, List(U8) -> Output
    add = |operand, operands| {
        result = operands.fold(operand, U8.plus_wrap)
        masked = result.bitwise_and(0xFF)
        zero_flag = masked == 0x00
        half_carry_flag = operands.prepend(operand).map(|x| x.bitwise_and(0xF)).fold(0, U8.plus) > 0xF
        carry_flag = operand > operands.fold(0xFF, U8.minus)
        (masked, Status.modify(Value(zero_flag), Value(Bool.False), Value(half_carry_flag), Value(carry_flag)))
    }

    sub : U8, List(U8) -> (U8, U8)
    sub = |operand, operands| {
        result = operands.fold(operand, U8.minus_wrap)
        masked = result.bitwise_and(0xFF)
        zero_flag = masked == 0x00
        half_carry_flag = operand.bitwise_and(0x0F) < operands.map(|x| x.bitwise_and(0x0F)).fold(0, U8.plus)
        carry_flag = operand < operands.fold(0, U8.plus)
        (masked, Status.set_all(zero_flag, Bool.True, half_carry_flag, carry_flag))
    }

    add16 : U16, U16 -> Output16
    add16 = |operand1, operand2| {
        result = operand1.plus_wrap(operand2)
        masked = result.bitwise_and(0xFFFF)
        half_carry_flag = operand1.bitwise_and(0x0FFF).plus(operand2.bitwise_and(0x0FFF)) > 0x0FFF
        carry_flag = operand1 > U16.minus(0xFFFF, operand2)
        (masked, Status.modify(Unchanged, Value(Bool.False), Value(half_carry_flag), Value(carry_flag)))
    }
}

expect test_flags(Alu.inc(0)) == (1, 0b00000000)
expect test_flags(Alu.inc(15)) == (16, 0b00100000)
expect test_flags(Alu.inc(128)) == (129, 0b00000000)
expect test_flags(Alu.inc(255)) == (0, 0b10100000)

expect test_flags(Alu.dec(1)) == (0, 0b11000000)
expect test_flags(Alu.dec(16)) == (15, 0b01100000)
expect test_flags(Alu.dec(129)) == (128, 0b01000000)
expect test_flags(Alu.dec(0)) == (255, 0b01100000)

expect test_flags(Alu.add(1, [1])) == (2, 0b00000000)
expect test_flags(Alu.add(15, [1])) == (16, 0b00100000)
expect test_flags(Alu.add(128, [128])) == (0, 0b10010000)
expect test_flags(Alu.add(255, [1])) == (0, 0b10110000)

expect Alu.sub(255, [1]) == (254, 0b01000000)
expect Alu.sub(1, [1]) == (0, 0b11000000)
expect Alu.sub(1, [255]) == (2, 0b01110000)
expect Alu.sub(16, [1]) == (15, 0b01100000)

expect test_flags(Alu.add16(1, 1)) == (2, 0b00000000)
expect test_flags(Alu.add16(32768, 32768)) == (0, 0b00010000)
expect test_flags(Alu.add16(65535, 1)) == (0, 0b00110000)
# expect test_flags(Alu.add16(128, 128)) == (0, 0b10010000)
# expect test_flags(Alu.add16(255, 1)) == (0, 0b10110000)
