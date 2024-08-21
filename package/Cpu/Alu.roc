## Arithmetic Logic Unit
module [
    inc,
    dec,
]

import Cpu.Register.Flag

FlagDelta : U8 -> U8
Dir : [Left, Right]

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
