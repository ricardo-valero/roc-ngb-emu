## Arithmetic Logic Unit
module [
    inc,
    dec,
]

import Cpu.Register.Flag

FlagDelta : U8 -> U8
# Dir : [Left, Right]

# Accumulator and Flag delta
Output : (U8, FlagDelta)
Output16 : (U16, FlagDelta)

testFlags = \(a, b) -> (a, b 0)

inc : U8 -> Output
inc = \operand ->
    result = Num.addWrap operand 1
    masked = Num.bitwiseAnd 0xFF result
    zeroFlag = masked == 0x00
    halfCarryFlag = Num.bitwiseAnd 0xF operand |> Num.add 1 > 0xF
    (masked, Cpu.Register.Flag.modify (Value zeroFlag) (Value Bool.false) (Value halfCarryFlag) Unchanged)

expect inc 0 |> testFlags == (1, 0b00000000)
expect inc 15 |> testFlags == (16, 0b00100000)
expect inc 128 |> testFlags == (129, 0b00000000)
expect inc 255 |> testFlags == (0, 0b10100000)

dec : U8 -> Output
dec = \operand ->
    result = Num.subWrap operand 1
    masked = Num.bitwiseAnd 0xFF result
    zeroFlag = masked == 0x00
    halfCarryFlag = Num.bitwiseAnd 0x0F operand == 0x00
    (masked, Cpu.Register.Flag.modify (Value zeroFlag) (Value Bool.true) (Value halfCarryFlag) Unchanged)

expect dec 1 |> testFlags == (0, 0b11000000)
expect dec 16 |> testFlags == (15, 0b01100000)
expect dec 129 |> testFlags == (128, 0b01000000)
expect dec 0 |> testFlags == (255, 0b01100000)

# TODO: Unsure if this is the right approach
# - Should I use Num.addChecked?
# - How to calculate the carry flag without converting to U16?
# - This also works for the adc op, but is it worth it?
add : U8, List U8 -> Output
add = \operand, operands ->
    result = operands |> List.walk operand Num.addWrap
    masked = Num.bitwiseAnd 0xFF result
    zeroFlag = masked == 0x00
    halfCarryFlag = operands |> List.prepend operand |> List.map (\x -> Num.bitwiseAnd 0xF x) |> List.walk 0 Num.add > 0xF
    carryFlag = operand > operands |> List.walk 0xFF Num.sub
    (masked, Cpu.Register.Flag.modify (Value zeroFlag) (Value Bool.false) (Value halfCarryFlag) (Value carryFlag))

sub : U8, List U8 -> (U8, U8)
sub = \operand, operands ->
    result = operands |> List.walk operand Num.subWrap
    masked = Num.bitwiseAnd 0xFF result
    zeroFlag = masked == 0x00
    halfCarryFlag = Num.bitwiseAnd 0x0F operand < operands |> List.map (\x -> Num.bitwiseAnd 0x0F x) |> List.walk 0 Num.add
    carryFlag = operand < operands |> List.walk 0 Num.add
    (masked, Cpu.Register.Flag.setAll zeroFlag Bool.true halfCarryFlag carryFlag)

add16 : U16, U16 -> Output16
add16 = \operand1, operand2 ->
    result = operand1 |> Num.addWrap operand2
    masked = Num.bitwiseAnd 0xFFFF result
    halfCarryFlag = Num.bitwiseAnd 0x0FFF operand1 |> Num.add (Num.bitwiseAnd 0x0FFF operand2) > 0x0FFF
    carryFlag = operand1 > 0xFFFF |> Num.sub operand2
    (masked, Cpu.Register.Flag.modify Unchanged (Value Bool.false) (Value halfCarryFlag) (Value carryFlag))

expect add 1 [1] |> testFlags == (2, 0b00000000)
expect add 15 [1] |> testFlags == (16, 0b00100000)
expect add 128 [128] |> testFlags == (0, 0b10010000)
expect add 255 [1] |> testFlags == (0, 0b10110000)

expect sub 255 [1] == (254, 0b01000000)
expect sub 1 [1] == (0, 0b11000000)
expect sub 1 [255] == (2, 0b01110000)
expect sub 16 [1] == (15, 0b01100000)

expect add16 1 1 |> testFlags == (2, 0b00000000)
expect add16 32768 32768 |> testFlags == (0, 0b00010000)
expect add16 65535 1 |> testFlags == (0, 0b00110000)
# expect add16 128 128 |> testFlags == (0, 0b10010000)
# expect add16 255 1 |> testFlags == (0, 0b10110000)

# rotate : Dir, U8, U8 -> (U8, U8)
# rotate = \dir, operand, flags ->
#    (a, b, shiftFn) =
#        when dir is
#            Left -> (B0, B7, Num.shiftLeftBy)
#            Right -> (B7, B0, Num.shiftRightZfBy)
#    carry = 1
#    # if Cpu.Register.Flag.get Carry flags then
#    #    Bit.mask a
#    # else
#    #    0x00
#    result = shiftFn 1 operand |> Num.bitwiseOr carry |> Num.bitwiseAnd 0xFF
#    carryFlag = Bit.check b operand
#    zeroFlag = result == 0x00
#    (result, Cpu.Register.Flag.setAll zeroFlag Bool.false Bool.false carryFlag)

# expect
#    actual = rotate Left 0b10110011 0b00000000
#    expected = (0b01100110, 0b00010000)
#    actual == expected

# rotateCircular : Dir, U8 -> (U8, U8)
# rotateCircular = \dir, operand ->
#    (shiftFnA, shiftFnB, a) =
#        when dir is
#            Left -> (Num.shiftRightZfBy, Num.shiftLeftBy, 0x80)
#            Right -> (Num.shiftLeftBy, Num.shiftRightBy, 0x01)
#    rotatedValue = shiftFnA 7 (Num.bitwiseAnd operand a)
#    rotatedValue
# result = shiftFnB 1 operand |> Num.bitwiseOr rotatedValue |> Num.bitwiseAnd 0xFF
# carryFlag = rotatedValue > 0
# zeroFlag = result == 0x00
# (result, Cpu.Register.Flag.setAll zeroFlag Bool.false Bool.false carryFlag)

# rlc : U8 -> (U8, U8)
# rlc = \operand ->
#    carry = Num.bitwiseAnd 0x80 operand # MSB becomes the carry flag
#    result =
#        Num.shiftLeftBy 1 operand # Shift left by 1
#        |> Num.bitwiseOr carry # Move MSB to LSB position
#        |> Num.bitwiseAnd 0xFF # Mask to keep within 8 bits
#    carryFlag = carry > 0
#    zeroFlag = result == 0x00
#    (result, Cpu.Register.Flag.setAll zeroFlag Bool.false Bool.false carryFlag)

# expect
#    actual = rlc 0b10011001
#    expected = (0b00110011, 0b00110011)
#    actual == expected

# shiftArithmetic : Dir, U8 -> (U8, U8)
# shiftArithmetic = \dir, operand ->
#    (a, result) =
#        when dir is
#            Left -> (0x80, Num.shiftLeftBy 1 operand |> Num.bitwiseAnd 0xFF)
#            Right -> (0x01, Num.shiftRightBy 1 operand |> Num.bitwiseOr (Num.bitwiseAnd 0x80 operand) |> Num.bitwiseAnd 0xFF)
#    zeroFlag = result == 0x00
#    carryFlag = Num.bitwiseAnd a operand > 0
#    (result, Cpu.Register.Flag.setAll zeroFlag Bool.false Bool.false carryFlag)

# expect
#    expected = (0b10110100, 0b00000000)
#    actual = shiftArithmetic Left 0b01011010
#    actual == expected

# expect
#    expected = (0b11101011, 0b00000000)
#    actual = shiftArithmetic Right 0b11010110
#    actual == expected
