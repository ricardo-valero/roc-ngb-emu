import /Bit exposing [Bit]

Status :: [].{
    ## Status register (commonly known as Flags register)
    ## 0b1111_0000
    ##   ││││ ╰┴┴┴─ Unused
    ##   │││╰─ Carry
    ##   ││╰── Half carry
    ##   │╰─── Subtract
    ##   ╰──── Zero
    Member := [Zero, Subtract, HalfCarry, Carry]

    Delta := [Complement, Unchanged, Value(Bool)]

    to_bit : Member -> Bit
    to_bit = |member|
        match member {
            Zero => B7
            Subtract => B6
            HalfCarry => B5
            Carry => B4
        }

    mask : Member -> U8
    mask = |member| Bit.mask(to_bit(member))

    check : Member, U8 -> Bool
    check = |member, byte| Bit.check(to_bit(member), byte)

    modify : Delta, Delta, Delta, Delta -> (U8 -> U8)
    modify = |z, n, h, c| |byte|
        [
            (z, Zero),
            (n, Subtract),
            (h, HalfCarry),
            (c, Carry),
        ]
            .map(|(d, member)| resolve_delta(d, member, byte))
            .fold(0x00, U8.bitwise_or)

    resolve_delta : Delta, Member, U8 -> U8
    resolve_delta = |delta, member, byte| {
        m = mask(member)
        match delta {
            Complement =>
                match m.bitwise_and(byte) {
                    0x00 => m
                    _ => 0x00
                }

            Unchanged => m.bitwise_and(byte)
            Value(b) =>
                if b {
                    m
                } else {
                    0x00
                }
        }
    }

    set_all : Bool, Bool, Bool, Bool -> U8
    set_all = |z, n, h, c| {
        f = modify(Value(z), Value(n), Value(h), Value(c))
        f(0)
    }
}

expect Status.mask(Zero) == 0b1000_0000

expect Status.check(Carry, 0b0001_0000) == Bool.True

expect Status.set_all(Bool.True, Bool.True, Bool.True, Bool.True) == 0b1111_0000
expect Status.set_all(Bool.True, Bool.False, Bool.False, Bool.True) == 0b1001_0000
