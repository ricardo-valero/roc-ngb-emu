module [Flag, Delta, mask, modify, check, setAll]

import Bit exposing [Bit]

# Flags: 0bZNHC0000
# Flag : [Z, N, H, C]
Flag : [Zero, Subtract, HalfCarry, Carry]

toBit : Flag -> Bit
toBit = \flag ->
    when flag is
        Zero -> B7
        Subtract -> B6
        HalfCarry -> B5
        Carry -> B4

mask : Flag -> U8
mask = \flag -> Bit.mask (toBit flag)

expect mask Zero == 0b10000000 # 0x80

check : Flag, U8 -> Bool
check = \flag, flags -> Bit.check (toBit flag) flags

expect check Carry 0b00010000 == Bool.true

Delta : [Unchanged, Complemented, Value Bool]

modify : Delta, Delta, Delta, Delta -> (U8 -> U8)
modify = \z, n, h, c -> \flags ->
        [
            (z, Zero),
            (n, Subtract),
            (h, HalfCarry),
            (c, Carry),
        ]
        |> List.map (\(d, flag) -> resolveDelta d flag flags)
        |> List.walk 0x00 Num.bitwiseOr

resolveDelta : Delta, Flag, U8 -> U8
resolveDelta = \delta, flag, flags ->
    m = mask flag
    when delta is
        Unchanged -> Num.bitwiseAnd m flags
        Complemented ->
            when Num.bitwiseAnd m flags is
                0x00 -> m
                _ -> 0x00

        Value b ->
            if b then
                m
            else
                0x00

setAll : Bool, Bool, Bool, Bool -> U8
setAll = \z, n, h, c -> (modify (Value z) (Value n) (Value h) (Value c)) 0

expect setAll Bool.true Bool.true Bool.true Bool.true == 0b11110000
expect setAll Bool.true Bool.false Bool.false Bool.true == 0b10010000
