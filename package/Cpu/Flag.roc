module [Flag, mask, modify, get, setAll]

# Flags: 0bZNHC0000

Flag : [Zero, Subtract, HalfCarry, Carry]

mask : Flag -> U8
mask = \flag ->
    Num.shiftLeftBy
        1
        (
            when flag is
                Zero -> 7
                Subtract -> 6
                HalfCarry -> 5
                Carry -> 4
        )

expect mask Zero == 0b10000000 # 0x80
expect mask Subtract == 0b01000000 # 0x40
expect mask HalfCarry == 0b00100000 # 0x20
expect mask Carry == 0b00010000 # 0x10

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

get : Flag, U8 -> Bool
get = \flag, flags ->
    Num.bitwiseAnd (mask flag) flags > 0

expect get Carry 0b00010000 == Bool.true

setAll : Bool, Bool, Bool, Bool -> U8
setAll = \z, n, h, c -> (modify (Value z) (Value n) (Value h) (Value c)) 0

expect setAll Bool.true Bool.true Bool.true Bool.true == 0b11110000
expect setAll Bool.true Bool.false Bool.false Bool.true == 0b10010000
