module [Flag, mask, modify, get, setAll]

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

get : Flag, U8 -> Bool
get = \flag, flags -> Bit.check (toBit flag) flags

expect get Carry 0b00010000 == Bool.true

modify : Delta, Delta, Delta, Delta -> (U8 -> U8)
modify = \z, n, h, c -> \flags ->
        [
            (z, mask Zero),
            (n, mask Subtract),
            (h, mask HalfCarry),
            (c, mask Carry),
        ]
        |> List.map (\(d, m) -> resolveDelta d m flags)
        |> List.walk 0x00 Num.bitwiseOr

Delta : [Unchanged, Complemented, Value Bool]

resolveDelta : Delta, U8, U8 -> U8
resolveDelta = \delta, m, flags ->
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
