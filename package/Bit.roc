module [Bit, mask, set, reset, check]

Bit : [B0, B1, B2, B3, B4, B5, B6, B7]

mask : Bit -> U8
mask = \bit ->
    Num.shiftLeftBy
        1
        (
            when bit is
                B0 -> 0
                B1 -> 1
                B2 -> 2
                B3 -> 3
                B4 -> 4
                B5 -> 5
                B6 -> 6
                B7 -> 7
        )

expect mask B7 == 0b10000000 # 0x80
expect mask B6 == 0b01000000 # 0x40
expect mask B5 == 0b00100000 # 0x20
expect mask B4 == 0b00010000 # 0x10

# Set bit on byte
set : Bit, U8 -> U8
set = \bit, byte ->
    Num.bitwiseOr byte (mask bit)

expect set B0 0x00 == 0x01
expect set B1 0x00 == 0x02
expect set B7 0x00 == 0x80
expect set B3 0x08 == 0x08

# Reset bit on byte
reset : Bit, U8 -> U8
reset = \bit, byte ->
    Num.bitwiseAnd byte (Num.bitwiseNot (mask bit))

expect reset B0 0x01 == 0x00
expect reset B1 0x02 == 0x00
expect reset B7 0x80 == 0x00
expect reset B3 0x08 == 0x00

# Check bit on byte
check : Bit, U8 -> Bool
check = \bit, byte ->
    Num.bitwiseAnd byte (mask bit) > 0

expect check B0 0x01 == Bool.true
expect check B1 0x02 == Bool.true
expect check B7 0x80 == Bool.true
expect check B3 0x08 == Bool.true
expect check B0 0x00 == Bool.false
