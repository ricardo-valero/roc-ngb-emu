# SHA-256 (FIPS 180-4) as a pure function. The check slices digest their
# outputs with this so golden files stay byte-compatible with digests
# produced by coreutils' sha256sum — the passing goldens are themselves
# the end-to-end proof of this implementation.

Sha256 := [].{
    hex : List(U8) -> Str
    hex = |message| {
        chars = digest(message).fold(List.repeat(0.U8, 0), |acc, byte|
            acc.append(nibble(byte.shr_zf_wrap(4))).append(nibble(byte.bitwise_and(0x0F))))
        Str.from_utf8(chars) ?? ""
    }

    nibble : U8 -> U8
    nibble = |n| if n < 10 { n.plus(48) } else { n.plus(87) }

    digest : List(U8) -> List(U8)
    digest = |message| {
        bitlen = message.len() * 8
        var m = message.append(0x80)
        while m.len() % 64 != 56 {
            m = m.append(0x00)
        }
        var shift = 56.U8
        var going = Bool.True
        while going {
            m = m.append(bitlen.shr_zf_wrap(shift).to_u8_wrap())
            if shift == 0 {
                going = Bool.False
            } else {
                shift = shift.minus(8)
            }
        }

        var h0 = 0x6A09E667.U32
        var h1 = 0xBB67AE85.U32
        var h2 = 0x3C6EF372.U32
        var h3 = 0xA54FF53A.U32
        var h4 = 0x510E527F.U32
        var h5 = 0x9B05688C.U32
        var h6 = 0x1F83D9AB.U32
        var h7 = 0x5BE0CD19.U32

        var block = 0
        while block < m.len() {
            var w = List.repeat(0.U32, 64)
            var t = 0
            while t < 16 {
                w = w.set(t, be32(m, block + t * 4)) ?? w
                t = t.plus(1)
            }
            while t < 64 {
                w15 = w.get(t - 15) ?? 0
                w2 = w.get(t - 2) ?? 0
                s0 = rotr(w15, 7).bitwise_xor(rotr(w15, 18)).bitwise_xor(w15.shr_zf_wrap(3))
                s1 = rotr(w2, 17).bitwise_xor(rotr(w2, 19)).bitwise_xor(w2.shr_zf_wrap(10))
                sum = (w.get(t - 16) ?? 0).plus_wrap(s0).plus_wrap(w.get(t - 7) ?? 0).plus_wrap(s1)
                w = w.set(t, sum) ?? w
                t = t.plus(1)
            }

            var a = h0
            var b = h1
            var c = h2
            var d = h3
            var e = h4
            var f = h5
            var g = h6
            var h = h7
            var r = 0
            while r < 64 {
                s1 = rotr(e, 6).bitwise_xor(rotr(e, 11)).bitwise_xor(rotr(e, 25))
                ch = e.bitwise_and(f).bitwise_xor(e.bitwise_not().bitwise_and(g))
                temp1 = h.plus_wrap(s1).plus_wrap(ch).plus_wrap(k_at(r)).plus_wrap(w.get(r) ?? 0)
                s0 = rotr(a, 2).bitwise_xor(rotr(a, 13)).bitwise_xor(rotr(a, 22))
                maj = a.bitwise_and(b).bitwise_xor(a.bitwise_and(c)).bitwise_xor(b.bitwise_and(c))
                temp2 = s0.plus_wrap(maj)
                h = g
                g = f
                f = e
                e = d.plus_wrap(temp1)
                d = c
                c = b
                b = a
                a = temp1.plus_wrap(temp2)
                r = r.plus(1)
            }

            h0 = h0.plus_wrap(a)
            h1 = h1.plus_wrap(b)
            h2 = h2.plus_wrap(c)
            h3 = h3.plus_wrap(d)
            h4 = h4.plus_wrap(e)
            h5 = h5.plus_wrap(f)
            h6 = h6.plus_wrap(g)
            h7 = h7.plus_wrap(h)
            block = block.plus(64)
        }

        words : List(U32)
        words = [h0, h1, h2, h3, h4, h5, h6, h7]
        words.fold(List.repeat(0.U8, 0), |acc, word|
            acc
                .append(word.shr_zf_wrap(24).to_u8_wrap())
                .append(word.shr_zf_wrap(16).to_u8_wrap())
                .append(word.shr_zf_wrap(8).to_u8_wrap())
                .append(word.to_u8_wrap()))
    }

    rotr : U32, U8 -> U32
    rotr = |x, n| x.shr_zf_wrap(n).bitwise_or(x.shl_wrap(U8.minus(32, n)))

    be32 : List(U8), U64 -> U32
    be32 = |bytes, idx|
        (bytes.get(idx) ?? 0).to_u32().shl_wrap(24)
            .bitwise_or((bytes.get(idx + 1) ?? 0).to_u32().shl_wrap(16))
            .bitwise_or((bytes.get(idx + 2) ?? 0).to_u32().shl_wrap(8))
            .bitwise_or((bytes.get(idx + 3) ?? 0).to_u32())

    k_at : U64 -> U32
    k_at = |i| round_constants({}).get(i) ?? 0

    round_constants : {} -> List(U32)
    round_constants = |_| [
        0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5, 0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
        0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3, 0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
        0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC, 0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
        0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7, 0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
        0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13, 0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
        0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3, 0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
        0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5, 0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
        0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208, 0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2,
    ]
}

# FIPS 180-4 test vectors
expect Sha256.hex([]) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
expect Sha256.hex("abc".to_utf8()) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
expect
    Sha256.hex("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".to_utf8())
    == "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"

