module [Bit, mask, set, reset, check]

Bit : [B0, B1, B2, B3, B4, B5, B6, B7]

mask : Bit -> U8
mask = \bit ->
    Num.shiftLeftBy
        1
        (
            when bit is
                B0 -> 0 # 0x01
                B1 -> 1 # 0x02
                B2 -> 2 # 0x04
                B3 -> 3 # 0x08
                B4 -> 4 # 0x10
                B5 -> 5 # 0x20
                B6 -> 6 # 0x40
                B7 -> 7 # 0x80
        )

expect mask B0 == 0b00000001
expect mask B7 == 0b10000000

# Set bit on byte
set : Bit, U8 -> U8
set = \bit, byte ->
    Num.bitwiseOr byte (mask bit)

expect set B1 0b00000101 == 0b00000111
expect set B6 0b11100000 == 0b11100000

# Reset bit on byte
reset : Bit, U8 -> U8
reset = \bit, byte ->
    Num.bitwiseAnd byte (Num.bitwiseNot (mask bit))

expect reset B2 0b00001110 == 0b00001010
expect reset B5 0b01010000 == 0b01010000

# Check bit on byte
check : Bit, U8 -> Bool
check = \bit, byte ->
    Num.bitwiseAnd byte (mask bit) > 0

expect check B3 0b11101000 == Bool.true
expect check B4 0b11101000 == Bool.false
