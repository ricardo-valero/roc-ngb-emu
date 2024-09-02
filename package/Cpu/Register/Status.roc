module [Member, mask, modify, check, setAll]

import Bit exposing [Bit]

## Status register (commonly known as Flags register)
## 0b1111_0000
##   ││││ ╰┴┴┴─ Unused
##   │││╰─ Carry
##   ││╰── Half carry
##   │╰─── Subtract
##   ╰──── Zero
Member : [Zero, Subtract, HalfCarry, Carry]

toBit : Member -> Bit
toBit = \member ->
    when member is
        Zero -> B7
        Subtract -> B6
        HalfCarry -> B5
        Carry -> B4

mask : Member -> U8
mask = \member -> Bit.mask (toBit member)

expect mask Zero == 0b1000_0000

check : Member, U8 -> Bool
check = \member, byte -> Bit.check (toBit member) byte

expect check Carry 0b0001_0000 == Bool.true

Delta : [Complement, Value Bool]

modify : Delta, Delta, Delta, Delta -> (U8 -> U8)
modify = \z, n, h, c -> \byte ->
        [
            (z, Zero),
            (n, Subtract),
            (h, HalfCarry),
            (c, Carry),
        ]
        |> List.map (\(d, member) -> resolveDelta d member byte)
        |> List.walk 0x00 Num.bitwiseOr

resolveDelta : Delta, Member, U8 -> U8
resolveDelta = \delta, member, byte ->
    m = mask member
    when delta is
        Complement ->
            when Num.bitwiseAnd m byte is
                0x00 -> m
                _ -> 0x00

        Value b ->
            if b then
                m
            else
                0x00

setAll : Bool, Bool, Bool, Bool -> U8
setAll = \z, n, h, c -> (modify (Value z) (Value n) (Value h) (Value c)) 0

expect setAll Bool.true Bool.true Bool.true Bool.true == 0b1111_0000
expect setAll Bool.true Bool.false Bool.false Bool.true == 0b1001_0000
