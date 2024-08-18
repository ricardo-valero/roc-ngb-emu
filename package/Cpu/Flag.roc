module [Flag, mask, modify, get]

Flag : [Zero, Subtract, HalfCarry, Carry]

mask : Flag -> U8
mask = \flag ->
    (
        when flag is
            Zero -> 7
            Subtract -> 6
            HalfCarry -> 5
            Carry -> 4
    )
    |> Num.shiftLeftBy 0x01

RegisterDelta : U8 -> U8

modify : Delta, Delta, Delta, Delta -> RegisterDelta
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
