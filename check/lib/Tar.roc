# POSIX tar reading as a pure function: 512-byte headers (name at 0,
# octal size at 124, typeflag at 156), file data padded to 512-byte
# blocks, archive ends at an all-zero header.

Tar := [].{
	Entry : { name : Str, bytes : List(U8) }

	## Regular-file entries of a tar archive, in archive order.
	entries : List(U8) -> List(Tar.Entry)
	entries = |archive| {
		var out = List.repeat({ name: "", bytes: List.repeat(0x00.U8, 0) }, 0)
		var pos = 0.U64
		var going = Bool.True
		while going {
			if pos.plus(512) > archive.len() {
				going = Bool.False
			} else {
				name = read_string(archive, pos, 100)
				if name == "" {
					going = Bool.False
				} else {
					size = read_octal(archive, pos.plus(124), 12)
					typeflag = archive.get(pos.plus(156)) ?? 0
					if typeflag == 0x30 or typeflag == 0x00 {
						out = out.append({ name, bytes: read_range(archive, pos.plus(512), size) })
					} else {
						{}
					}
					blocks = size.plus(511).shr_zf_wrap(9)
					pos = pos.plus(512).plus(blocks.shl_wrap(9))
				}
			}
		}
		out
	}

	# NUL-terminated string of at most `max` bytes.
	read_string : List(U8), U64, U64 -> Str
	read_string = |bytes, start, max| {
		var out = List.repeat(0x00.U8, 0)
		var i = 0.U64
		var going = Bool.True
		while going and i < max {
			b = bytes.get(start.plus(i)) ?? 0
			if b == 0 {
				going = Bool.False
			} else {
				out = out.append(b)
				i = i.plus(1)
			}
		}
		Str.from_utf8(out) ?? ""
	}

	# Octal number, ignoring leading spaces and stopping at NUL/space.
	read_octal : List(U8), U64, U64 -> U64
	read_octal = |bytes, start, max| {
		var value = 0.U64
		var i = 0.U64
		while i < max {
			b = bytes.get(start.plus(i)) ?? 0
			if b >= 0x30 and b <= 0x37 {
				value = value.shl_wrap(3).plus(b.minus(0x30).to_u64())
			} else {
				{}
			}
			i = i.plus(1)
		}
		value
	}

	read_range : List(U8), U64, U64 -> List(U8)
	read_range = |bytes, start, count| {
		var out = List.repeat(0x00.U8, 0)
		var i = 0.U64
		while i < count {
			out = out.append(bytes.get(start.plus(i)) ?? 0)
			i = i.plus(1)
		}
		out
	}
}
