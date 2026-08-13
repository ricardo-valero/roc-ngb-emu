# DEFLATE (RFC 1951) and gzip (RFC 1952) decoding as pure functions — a
# port of zlib's reference decoder `puff`. The mooneye fetch uses this to
# unpack the upstream .tar.gz, whose archive-level SHA-256 is verified
# before decoding, so this decoder only ever sees well-formed input; it
# still fails closed (Err, never crash) on anything malformed.

Inflate := [].{

	## Decompress a gzip member (header + raw deflate + trailer).
	gunzip : List(U8) -> Try(List(U8), [BadGzip, BadDeflate, ..])
	gunzip = |input| {
		if input.len() < 18 or (input.get(0) ?? 0) != 0x1F or (input.get(1) ?? 0) != 0x8B or (input.get(2) ?? 0) != 0x08 {
			Err(BadGzip)
		} else {
			flags = input.get(3) ?? 0
			var pos = 10.U64
			if flags.bitwise_and(0x04) != 0 {
				xlen = (input.get(pos) ?? 0).to_u64().bitwise_or((input.get(pos.plus(1)) ?? 0).to_u64().shl_wrap(8))
				pos = pos.plus(2).plus(xlen)
			} else {
				{}
			}
			if flags.bitwise_and(0x08) != 0 {
				while (input.get(pos) ?? 0) != 0 {
					pos = pos.plus(1)
				}
				pos = pos.plus(1)
			} else {
				{}
			}
			if flags.bitwise_and(0x10) != 0 {
				while (input.get(pos) ?? 0) != 0 {
					pos = pos.plus(1)
				}
				pos = pos.plus(1)
			} else {
				{}
			}
			if flags.bitwise_and(0x02) != 0 {
				pos = pos.plus(2)
			} else {
				{}
			}
			inflate(input, pos)
		}
	}

	## Decompress a raw deflate stream starting at `start`.
	inflate : List(U8), U64 -> Try(List(U8), [BadDeflate, ..])
	inflate = |input, start| {
		var pos = start
		var buf = 0.U64
		var cnt = 0.U64
		var out = List.repeat(0x00.U8, 0)
		var last = 0.U64
		var bad = Bool.False
		while last == 0 and !bad {
			t1 = take(input, 1, pos, buf, cnt)
			last = t1.v
			t2 = take(input, 2, t1.pos, t1.buf, t1.cnt)
			pos = t2.pos
			buf = t2.buf
			cnt = t2.cnt
			if t2.v == 0 {
				# Stored: realign to a byte boundary (residual bits are < 8),
				# read LEN, skip ~LEN, copy verbatim.
				buf = 0
				cnt = 0
				if pos.plus(4) > input.len() {
					bad = Bool.True
				} else {
					stored_len = (input.get(pos) ?? 0).to_u64().bitwise_or((input.get(pos.plus(1)) ?? 0).to_u64().shl_wrap(8))
					pos = pos.plus(4)
					if pos.plus(stored_len) > input.len() {
						bad = Bool.True
					} else {
						var k = 0.U64
						while k < stored_len {
							out = out.append(input.get(pos.plus(k)) ?? 0)
							k = k.plus(1)
						}
						pos = pos.plus(stored_len)
					}
				}
			} else if t2.v == 3 {
				bad = Bool.True
			} else {
				codebooks =
					if t2.v == 1 {
						{ lit: fixed_lit_code({}), dist: fixed_dist_code({}), pos, buf, cnt, ok: Bool.True }
					} else {
						dynamic_codes(input, pos, buf, cnt)
					}
				if !codebooks.ok {
					bad = Bool.True
				} else {
					r = codes(input, codebooks.lit, codebooks.dist, codebooks.pos, codebooks.buf, codebooks.cnt, out)
					pos = r.pos
					buf = r.buf
					cnt = r.cnt
					out = r.out
					bad = !r.ok
				}
			}
		}
		if bad {
			Err(BadDeflate)
		} else {
			Ok(out)
		}
	}

	Codebook : { counts : List(U64), symbols : List(U64) }

	# Read `n` bits LSB-first, buffering whole input bytes as needed.
	take : List(U8), U64, U64, U64, U64 -> { v : U64, pos : U64, buf : U64, cnt : U64 }
	take = |input, n, pos0, buf0, cnt0| {
		var pos = pos0
		var buf = buf0
		var cnt = cnt0
		while cnt < n {
			buf = buf.bitwise_or((input.get(pos) ?? 0).to_u64().shl_wrap(cnt.to_u8_wrap()))
			pos = pos.plus(1)
			cnt = cnt.plus(8)
		}
		mask = U64.shl_wrap(1, n.to_u8_wrap()).minus(1)
		{ v: buf.bitwise_and(mask), pos, buf: buf.shr_zf_wrap(n.to_u8_wrap()), cnt: cnt.minus(n) }
	}

	# Canonical-Huffman tables from a per-symbol code-length list.
	construct : List(U64) -> Codebook
	construct = |lengths| {
		var counts = List.repeat(0.U64, 16)
		var i = 0.U64
		while i < lengths.len() {
			l = lengths.get(i) ?? 0
			counts = counts.set(l, (counts.get(l) ?? 0).plus(1)) ?? counts
			i = i.plus(1)
		}
		counts = counts.set(0, 0) ?? counts
		var offsets = List.repeat(0.U64, 17)
		var l = 1.U64
		while l < 16 {
			offsets = offsets.set(l.plus(1), (offsets.get(l) ?? 0).plus(counts.get(l) ?? 0)) ?? offsets
			l = l.plus(1)
		}
		var symbols = List.repeat(0.U64, lengths.len())
		var s = 0.U64
		while s < lengths.len() {
			sl = lengths.get(s) ?? 0
			if sl != 0 {
				off = offsets.get(sl) ?? 0
				symbols = symbols.set(off, s) ?? symbols
				offsets = offsets.set(sl, off.plus(1)) ?? offsets
			} else {
				{}
			}
			s = s.plus(1)
		}
		{ counts, symbols }
	}

	# Decode one symbol bit by bit (puff's slow-but-simple walk).
	decode : List(U8), Codebook, U64, U64, U64 -> { sym : U64, ok : Bool, pos : U64, buf : U64, cnt : U64 }
	decode = |input, book, pos0, buf0, cnt0| {
		var pos = pos0
		var buf = buf0
		var cnt = cnt0
		var code = 0.U64
		var first = 0.U64
		var index = 0.U64
		var len = 1.U64
		var out = { sym: 0.U64, ok: Bool.False, pos, buf, cnt }
		var going = Bool.True
		while going and len <= 15 {
			t = take(input, 1, pos, buf, cnt)
			pos = t.pos
			buf = t.buf
			cnt = t.cnt
			code = code.bitwise_or(t.v)
			c = book.counts.get(len) ?? 0
			if code.minus(first) < c {
				out = { sym: book.symbols.get(index.plus(code.minus(first))) ?? 0, ok: Bool.True, pos, buf, cnt }
				going = Bool.False
			} else {
				index = index.plus(c)
				first = first.plus(c).shl_wrap(1)
				code = code.shl_wrap(1)
				len = len.plus(1)
			}
		}
		out
	}

	fixed_lit_code : {} -> Codebook
	fixed_lit_code = |{}| {
		var lengths = List.repeat(8.U64, 288)
		var i = 144.U64
		while i < 256 {
			lengths = lengths.set(i, 9) ?? lengths
			i = i.plus(1)
		}
		i = 256
		while i < 280 {
			lengths = lengths.set(i, 7) ?? lengths
			i = i.plus(1)
		}
		construct(lengths)
	}

	fixed_dist_code : {} -> Codebook
	fixed_dist_code = |{}| construct(List.repeat(5.U64, 30))

	# Dynamic-block header: code-length code, then run-length-encoded
	# literal/length and distance code lengths.
	dynamic_codes : List(U8), U64, U64, U64 -> { lit : Codebook, dist : Codebook, pos : U64, buf : U64, cnt : U64, ok : Bool }
	dynamic_codes = |input, pos0, buf0, cnt0| {
		order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
		t1 = take(input, 5, pos0, buf0, cnt0)
		nlen = t1.v.plus(257)
		t2 = take(input, 5, t1.pos, t1.buf, t1.cnt)
		ndist = t2.v.plus(1)
		t3 = take(input, 4, t2.pos, t2.buf, t2.cnt)
		ncode = t3.v.plus(4)
		var pos = t3.pos
		var buf = t3.buf
		var cnt = t3.cnt
		var cl_lengths = List.repeat(0.U64, 19)
		var i = 0.U64
		while i < ncode {
			t = take(input, 3, pos, buf, cnt)
			pos = t.pos
			buf = t.buf
			cnt = t.cnt
			cl_lengths = cl_lengths.set((order.get(i) ?? 0).to_u64(), t.v) ?? cl_lengths
			i = i.plus(1)
		}
		cl_code = construct(cl_lengths)
		var lengths = List.repeat(0.U64, nlen.plus(ndist))
		var n = 0.U64
		var ok = Bool.True
		while ok and n < nlen.plus(ndist) {
			d = decode(input, cl_code, pos, buf, cnt)
			pos = d.pos
			buf = d.buf
			cnt = d.cnt
			if !d.ok {
				ok = Bool.False
			} else if d.sym < 16 {
				lengths = lengths.set(n, d.sym) ?? lengths
				n = n.plus(1)
			} else {
				repeat_info =
					if d.sym == 16 {
						{
							value: if n > 0 {
								lengths.get(n.minus(1)) ?? 0
							} else {
								0
							},
							base: 3.U64,
							extra: 2.U64,
						}
					} else if d.sym == 17 {
						{ value: 0.U64, base: 3.U64, extra: 3.U64 }
					} else {
						{ value: 0.U64, base: 11.U64, extra: 7.U64 }
					}
				t = take(input, repeat_info.extra, pos, buf, cnt)
				pos = t.pos
				buf = t.buf
				cnt = t.cnt
				var reps = repeat_info.base.plus(t.v)
				if d.sym == 16 and n == 0 {
					ok = Bool.False
				} else {
					{}
				}
				while ok and reps > 0 and n < nlen.plus(ndist) {
					lengths = lengths.set(n, repeat_info.value) ?? lengths
					n = n.plus(1)
					reps = reps.minus(1)
				}
			}
		}
		var lit_lengths = List.repeat(0.U64, 0)
		var a = 0.U64
		while a < nlen {
			lit_lengths = lit_lengths.append(lengths.get(a) ?? 0)
			a = a.plus(1)
		}
		var dist_lengths = List.repeat(0.U64, 0)
		while a < nlen.plus(ndist) {
			dist_lengths = dist_lengths.append(lengths.get(a) ?? 0)
			a = a.plus(1)
		}
		{ lit: construct(lit_lengths), dist: construct(dist_lengths), pos, buf, cnt, ok }
	}

	# Literal/length-and-distance loop for one block.
	codes : List(U8), Codebook, Codebook, U64, U64, U64, List(U8) -> { out : List(U8), ok : Bool, pos : U64, buf : U64, cnt : U64 }
	codes = |input, lit, dist, pos0, buf0, cnt0, out0| {
		length_base : List(U64)
		length_base = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
		length_extra : List(U64)
		length_extra = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
		dist_base : List(U64)
		dist_base = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577]
		dist_extra : List(U64)
		dist_extra = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]
		var pos = pos0
		var buf = buf0
		var cnt = cnt0
		var out = out0
		var ok = Bool.True
		var going = Bool.True
		while going and ok {
			d = decode(input, lit, pos, buf, cnt)
			pos = d.pos
			buf = d.buf
			cnt = d.cnt
			if !d.ok {
				ok = Bool.False
			} else if d.sym < 256 {
				out = out.append(d.sym.to_u8_wrap())
			} else if d.sym == 256 {
				going = Bool.False
			} else if d.sym > 285 {
				ok = Bool.False
			} else {
				li = d.sym.minus(257)
				te = take(input, (length_extra.get(li) ?? 0), pos, buf, cnt)
				match_len = (length_base.get(li) ?? 0).plus(te.v)
				dd = decode(input, dist, te.pos, te.buf, te.cnt)
				if !dd.ok or dd.sym > 29 {
					ok = Bool.False
					pos = dd.pos
					buf = dd.buf
					cnt = dd.cnt
				} else {
					td = take(input, (dist_extra.get(dd.sym) ?? 0), dd.pos, dd.buf, dd.cnt)
					pos = td.pos
					buf = td.buf
					cnt = td.cnt
					distance = (dist_base.get(dd.sym) ?? 0).plus(td.v)
					if distance > out.len() {
						ok = Bool.False
					} else {
						from = out.len().minus(distance)
						var k = 0.U64
						while k < match_len {
							out = out.append(out.get(from.plus(k)) ?? 0)
							k = k.plus(1)
						}
					}
				}
			}
		}
		{ out, ok, pos, buf, cnt }
	}
}
