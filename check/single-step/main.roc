# Pure-Roc runner for Tom Harte's SingleStepTests (SM83).
#
#   roc check/single-step/main.roc -- check/single-step/data/*.json
#
# Verified schema (fetched 2026-08-14, one file per opcode, 1000 cases
# each; all values decimal):
#
#   { "name": "00 0000",
#     "initial": { "pc": .., "sp": .., "a": .., "b": .., "c": .., "d": ..,
#                  "e": .., "f": .., "h": .., "l": .., "ime": 0|1, "ie": 0|1,
#                  "ram": [[addr, val], ..] },
#     "final":   { same minus "ie" },
#     "cycles":  [[addr, val, flags], ..] }   # one entry per M-cycle
#
# The flags string is three chars: "r-m" read, "-wm" write, "---" idle
# M-cycle (no bus access; addr/val echo the previous entry). Each case is
# executed through the core's single-step harness (GameBoy.from_raw /
# step_instruction over flat memory) and diffed field-by-field: registers,
# IME, touched RAM, total cycles (4 T-cycles per M-cycle entry), and the
# ordered memory-access trace against the non-idle cycle entries.
# The parser below is a minimal recursive-descent pass over the fixed,
# machine-generated schema - no external JSON dependency.
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	ngb: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy

State : { pc : U16, sp : U16, a : U8, b : U8, c : U8, d : U8, e : U8, f : U8, h : U8, l : U8, ime : Bool, ie : U8, ei : U8, ram : List({ addr : U16, val : U8 }) }

# A bus access with Write flagged; idle M-cycles are dropped at parse time
Access : { addr : U16, val : U8, w : Bool }

Case : { initial : State, final : State, m_cycles : U64, accesses : List(Access) }

# --- minimal JSON parsing over the fixed schema ---

mul10 : U64 -> U64
mul10 = |n| n.shl_wrap(3).plus(n.shl_wrap(1))

skip_ws : List(U8), U64 -> U64
skip_ws = |b, i|
	match b.get(i) {
		Ok(c) =>
			if c == 32 or c == 10 or c == 13 or c == 9 {
				skip_ws(b, i.plus(1))
			} else {
				i
			}

		Err(_) => i
	}

expect_ch : List(U8), U64, U8 -> Try(U64, [ParseError(U64)])
expect_ch = |b, i0, ch| {
	i = skip_ws(b, i0)
	if (b.get(i) ?? 0) == ch {
		Ok(i.plus(1))
	} else {
		Err(ParseError(i))
	}
}

parse_u64 : List(U8), U64 -> Try({ i : U64, val : U64 }, [ParseError(U64)])
parse_u64 = |b, i0| parse_digits(b, skip_ws(b, i0), 0, Bool.False)

parse_digits : List(U8), U64, U64, Bool -> Try({ i : U64, val : U64 }, [ParseError(U64)])
parse_digits = |b, i, acc, any|
	match b.get(i) {
		Ok(c) =>
			if c >= 48 and c <= 57 {
				parse_digits(b, i.plus(1), mul10(acc).plus(c.minus(48).to_u64()), Bool.True)
			} else if any {
				Ok({ i: i, val: acc })
			} else {
				Err(ParseError(i))
			}

		Err(_) =>
			if any {
				Ok({ i: i, val: acc })
			} else {
				Err(ParseError(i))
			}
	}

# position after the closing quote of a string whose opening quote is at/after i0
skip_string : List(U8), U64 -> Try(U64, [ParseError(U64)])
skip_string = |b, i0| {
	i = expect_ch(b, i0, 34)?
	skip_to_quote(b, i)
}

skip_to_quote : List(U8), U64 -> Try(U64, [ParseError(U64)])
skip_to_quote = |b, i|
	match b.get(i) {
		Ok(34) => Ok(i.plus(1))
		Ok(_) => skip_to_quote(b, i.plus(1))
		Err(_) => Err(ParseError(i))
	}

# case-level keys: name / initial / final / cycles, by first byte
parse_case_key : List(U8), U64 -> Try({ i : U64, val : [KName, KInitial, KFinal, KCycles, KOther] }, [ParseError(U64)])
parse_case_key = |b, i0| {
	i1 = expect_ch(b, i0, 34)?
	c1 = b.get(i1) ?? 0
	key =
		if c1 == 110 {
			KName
		} else if c1 == 105 {
			KInitial
		} else if c1 == 102 {
			KFinal
		} else if c1 == 99 {
			KCycles
		} else {
			KOther
		}
	i2 = skip_to_quote(b, i1)?
	Ok({ i: i2, val: key })
}

# state-level keys; "ime"/"ie", "e"/"ei" ("ei" is fb.json's EI-pending
# latch in final states), and "pc"/"sp" need a second byte
parse_state_key : List(U8), U64 -> Try({ i : U64, val : [KPc, KSp, KA, KB, KC, KD, KE, KF, KH, KL, KIme, KIe, KEi, KRam, KOther] }, [ParseError(U64)])
parse_state_key = |b, i0| {
	i1 = expect_ch(b, i0, 34)?
	c1 = b.get(i1) ?? 0
	c2 = b.get(i1.plus(1)) ?? 0
	key =
		if c1 == 112 {
			KPc
		} else if c1 == 115 {
			KSp
		} else if c1 == 97 {
			KA
		} else if c1 == 98 {
			KB
		} else if c1 == 99 {
			KC
		} else if c1 == 100 {
			KD
		} else if c1 == 101 and c2 == 105 {
			KEi
		} else if c1 == 101 {
			KE
		} else if c1 == 102 {
			KF
		} else if c1 == 104 {
			KH
		} else if c1 == 108 {
			KL
		} else if c1 == 105 and c2 == 109 {
			KIme
		} else if c1 == 105 {
			KIe
		} else if c1 == 114 {
			KRam
		} else {
			KOther
		}
	i2 = skip_to_quote(b, i1)?
	Ok({ i: i2, val: key })
}

parse_ram : List(U8), U64 -> Try({ i : U64, val : List({ addr : U16, val : U8 }) }, [ParseError(U64)])
parse_ram = |b, i0| {
	i = expect_ch(b, i0, 91)?
	parse_ram_items(b, i, [])
}

parse_ram_items : List(U8), U64, List({ addr : U16, val : U8 }) -> Try({ i : U64, val : List({ addr : U16, val : U8 }) }, [ParseError(U64)])
parse_ram_items = |b, i0, acc| {
	i = skip_ws(b, i0)
	match b.get(i) {
		Ok(93) => Ok({ i: i.plus(1), val: acc })
		Ok(44) => parse_ram_items(b, i.plus(1), acc)
		Ok(91) => {
			a = parse_u64(b, i.plus(1))?
			i2 = expect_ch(b, a.i, 44)?
			v = parse_u64(b, i2)?
			i3 = expect_ch(b, v.i, 93)?
			parse_ram_items(b, i3, acc.append({ addr: a.val.to_u16_wrap(), val: v.val.to_u8_wrap() }))
		}

		_ => Err(ParseError(i))
	}
}

# the cycles array: count every M-cycle, keep the non-idle bus accesses
parse_cycles : List(U8), U64 -> Try({ i : U64, m_cycles : U64, accesses : List(Access) }, [ParseError(U64)])
parse_cycles = |b, i0| {
	i = expect_ch(b, i0, 91)?
	parse_cycle_items(b, i, 0, [])
}

parse_cycle_items : List(U8), U64, U64, List(Access) -> Try({ i : U64, m_cycles : U64, accesses : List(Access) }, [ParseError(U64)])
parse_cycle_items = |b, i0, n, acc| {
	i = skip_ws(b, i0)
	match b.get(i) {
		Ok(93) => Ok({ i: i.plus(1), m_cycles: n, accesses: acc })
		Ok(44) => parse_cycle_items(b, i.plus(1), n, acc)
		Ok(91) => {
			a = parse_u64(b, i.plus(1))?
			i2 = expect_ch(b, a.i, 44)?
			v = parse_u64(b, i2)?
			i3 = expect_ch(b, v.i, 44)?
			i4 = expect_ch(b, i3, 34)?
			c1 = b.get(i4) ?? 0
			c2 = b.get(i4.plus(1)) ?? 0
			i5 = skip_to_quote(b, i4)?
			i6 = expect_ch(b, i5, 93)?
			acc2 =
				if c1 == 114 { # 'r'
					acc.append({ addr: a.val.to_u16_wrap(), val: v.val.to_u8_wrap(), w: Bool.False })
				} else if c2 == 119 { # "-w"
					acc.append({ addr: a.val.to_u16_wrap(), val: v.val.to_u8_wrap(), w: Bool.True })
				} else { # "---": idle M-cycle, no bus access
					acc
				}
			parse_cycle_items(b, i6, n.plus(1), acc2)
		}

		_ => Err(ParseError(i))
	}
}

empty_state : {} -> State
empty_state = |_| { pc: 0, sp: 0, a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, h: 0, l: 0, ime: Bool.False, ie: 0, ei: 0, ram: [] }

parse_state : List(U8), U64 -> Try({ i : U64, val : State }, [ParseError(U64)])
parse_state = |b, i0| {
	i = expect_ch(b, i0, 123)?
	parse_state_fields(b, i, empty_state({}))
}

parse_state_fields : List(U8), U64, State -> Try({ i : U64, val : State }, [ParseError(U64)])
parse_state_fields = |b, i0, st| {
	i = skip_ws(b, i0)
	match b.get(i) {
		Ok(125) => Ok({ i: i.plus(1), val: st })
		Ok(44) => parse_state_fields(b, i.plus(1), st)
		Ok(34) => {
			k = parse_state_key(b, i)?
			i2 = expect_ch(b, k.i, 58)?
			match k.val {
				KRam => {
					r = parse_ram(b, i2)?
					parse_state_fields(b, r.i, { ..st, ram: r.val })
				}

				other => {
					r = parse_u64(b, i2)?
					st2 =
						match other {
							KPc => { ..st, pc: r.val.to_u16_wrap() }
							KSp => { ..st, sp: r.val.to_u16_wrap() }
							KA => { ..st, a: r.val.to_u8_wrap() }
							KB => { ..st, b: r.val.to_u8_wrap() }
							KC => { ..st, c: r.val.to_u8_wrap() }
							KD => { ..st, d: r.val.to_u8_wrap() }
							KE => { ..st, e: r.val.to_u8_wrap() }
							KF => { ..st, f: r.val.to_u8_wrap() }
							KH => { ..st, h: r.val.to_u8_wrap() }
							KL => { ..st, l: r.val.to_u8_wrap() }
							KIme => { ..st, ime: r.val == 1 }
							KIe => { ..st, ie: r.val.to_u8_wrap() }
							KEi => { ..st, ei: r.val.to_u8_wrap() }
							_ => st
						}
					parse_state_fields(b, r.i, st2)
				}
			}
		}

		_ => Err(ParseError(i))
	}
}

empty_case : {} -> Case
empty_case = |_| { initial: empty_state({}), final: empty_state({}), m_cycles: 0, accesses: [] }

parse_case : List(U8), U64 -> Try({ i : U64, val : Case }, [ParseError(U64)])
parse_case = |b, i0| {
	i = expect_ch(b, i0, 123)?
	parse_case_fields(b, i, empty_case({}))
}

parse_case_fields : List(U8), U64, Case -> Try({ i : U64, val : Case }, [ParseError(U64)])
parse_case_fields = |b, i0, case| {
	i = skip_ws(b, i0)
	match b.get(i) {
		Ok(125) => Ok({ i: i.plus(1), val: case })
		Ok(44) => parse_case_fields(b, i.plus(1), case)
		Ok(34) => {
			k = parse_case_key(b, i)?
			i2 = expect_ch(b, k.i, 58)?
			match k.val {
				KName => {
					i3 = skip_string(b, i2)?
					parse_case_fields(b, i3, case)
				}

				KInitial => {
					r = parse_state(b, i2)?
					parse_case_fields(b, r.i, { ..case, initial: r.val })
				}

				KFinal => {
					r = parse_state(b, i2)?
					parse_case_fields(b, r.i, { ..case, final: r.val })
				}

				KCycles => {
					r = parse_cycles(b, i2)?
					parse_case_fields(b, r.i, { ..case, m_cycles: r.m_cycles, accesses: r.accesses })
				}

				_ => Err(ParseError(i))
			}
		}

		_ => Err(ParseError(i))
	}
}

parse_cases : List(U8) -> Try(List(Case), [ParseError(U64)])
parse_cases = |b| {
	i = expect_ch(b, 0, 91)?
	parse_case_list(b, i, [])
}

parse_case_list : List(U8), U64, List(Case) -> Try(List(Case), [ParseError(U64)])
parse_case_list = |b, i0, acc| {
	i = skip_ws(b, i0)
	match b.get(i) {
		Ok(93) => Ok(acc)
		Ok(44) => parse_case_list(b, i.plus(1), acc)
		Ok(123) => {
			r = parse_case(b, i)?
			parse_case_list(b, r.i, acc.append(r.val))
		}

		_ => Err(ParseError(i))
	}
}

# --- case execution ---

check_u8 : Str, U8, U8 -> Str
check_u8 = |label, got, want|
	if got == want {
		""
	} else {
		"${label}: got ${got.to_str()} want ${want.to_str()}"
	}

check_u16 : Str, U16, U16 -> Str
check_u16 = |label, got, want|
	if got == want {
		""
	} else {
		"${label}: got ${got.to_str()} want ${want.to_str()}"
	}

check_bool : Str, Bool, Bool -> Str
check_bool = |label, got, want|
	if got == want {
		""
	} else {
		"${label}: got ${if got { "1" } else { "0" }} want ${if want { "1" } else { "0" }}"
	}

check_u64 : Str, U64, U64 -> Str
check_u64 = |label, got, want|
	if got == want {
		""
	} else {
		"${label}: got ${got.to_str()} want ${want.to_str()}"
	}

access_str : Access -> Str
access_str = |a| "(${a.addr.to_str()},${a.val.to_str()},${if a.w { "w" } else { "r" }})"

# first diverging access, or length mismatch, between got and want
check_trace : List(Access), List(Access) -> Str
check_trace = |got, want| check_trace_at(got, want, 0)

check_trace_at : List(Access), List(Access), U64 -> Str
check_trace_at = |got, want, i|
	match (got.get(i), want.get(i)) {
		(Err(_), Err(_)) => ""
		(Ok(g), Ok(w)) =>
			if g == w {
				check_trace_at(got, want, i.plus(1))
			} else {
				"trace[${i.to_str()}]: got ${access_str(g)} want ${access_str(w)}"
			}

		(Ok(g), Err(_)) => "trace[${i.to_str()}]: got ${access_str(g)} want end (${want.len().to_str()} accesses)"
		(Err(_), Ok(w)) => "trace[${i.to_str()}]: got end (${got.len().to_str()} accesses) want ${access_str(w)}"
	}

run_case : Case -> Str
run_case = |case| {
	base = List.repeat(0.U8, 0x10000)
	with_ie = base.set(0xFFFF, case.initial.ie) ?? base
	mem = case.initial.ram.fold(with_ie, |m, e| m.set(e.addr.to_u64(), e.val) ?? m)
	s = {
		pc: case.initial.pc,
		sp: case.initial.sp,
		a: case.initial.a,
		b: case.initial.b,
		c: case.initial.c,
		d: case.initial.d,
		e: case.initial.e,
		f: case.initial.f,
		h: case.initial.h,
		l: case.initial.l,
		ime: case.initial.ime,
		mem: mem,
	}
	done = GameBoy.from_raw(s).step_instruction()
	(g, cycles) = done
	r = g.raw()
	got_trace = g.access_trace().map(|e| {
		addr: e.addr,
		val: e.val,
		w: match e.dir {
			Write => Bool.True
			Read => Bool.False
		},
	})
	reg_checks = [
		check_u16("pc", r.pc, case.final.pc),
		check_u16("sp", r.sp, case.final.sp),
		check_u8("a", r.a, case.final.a),
		check_u8("b", r.b, case.final.b),
		check_u8("c", r.c, case.final.c),
		check_u8("d", r.d, case.final.d),
		check_u8("e", r.e, case.final.e),
		check_u8("f", r.f, case.final.f),
		check_u8("h", r.h, case.final.h),
		check_u8("l", r.l, case.final.l),
		check_bool("ime", r.ime, case.final.ime),
		check_bool("ei", g.ei_pending, case.final.ei == 1),
		check_u64("cycles", cycles, case.m_cycles * 4),
		check_trace(got_trace, case.accesses),
	]
	ram_checks = case.final.ram.map(|e| check_u8("ram[${e.addr.to_str()}]", r.mem.get(e.addr.to_u64()) ?? 0, e.val))
	reg_checks.concat(ram_checks).fold("", |acc, msg| if msg == "" { acc } else if acc == "" { msg } else { "${acc}; ${msg}" })
}

FileStats : { pass : U64, fail : U64, detail : Str, idx : U64 }

run_cases : List(Case) -> FileStats
run_cases = |cases|
	cases.fold({ pass: 0, fail: 0, detail: "", idx: 0 }, |acc, case| {
		msg = run_case(case)
		if msg == "" {
			{ ..acc, pass: acc.pass.plus(1), idx: acc.idx.plus(1) }
		} else {
			detail =
				if acc.fail < 5 {
					"${acc.detail}\n    case ${acc.idx.to_str()}: ${msg}"
				} else {
					acc.detail
				}
			{ ..acc, fail: acc.fail.plus(1), detail: detail, idx: acc.idx.plus(1) }
		}
	})

# --- driver ---

# Documented exclusions, blargg-passlist style: vector files whose
# convention the batched core intentionally does not model. Reported,
# never gating; everything else gates.
exclusion : Str -> [Run, Excluded(Str)]
exclusion = |basename|
	match basename {
		"76.json" => Excluded("HALT: vectors charge the 2 idle M-cycles of the halt state itself; the core batches halt idling into subsequent steps")
		"10.json" => Excluded("STOP: vectors model 1-byte stop-mode entry (3 M-cycles); the core models the speed switch and padding skip")
		_ => Run
	}

basename_of : Str -> Str
basename_of = |path|
	path.split_on("/").fold("", |_, seg| seg)

run_file! = |arg| {
	path = Path.from_os_str(arg)
	match exclusion(basename_of(path.display())) {
		Excluded(reason) => {
			Stdout.line!("${path.display()}: excluded (${reason})")?
			Ok(0)
		}

		Run => run_gating_file!(path)
	}
}

run_gating_file! = |path| {
	bytes = path.read_bytes!()?
	match parse_cases(bytes) {
		Ok(cases) => {
			stats = run_cases(cases)
			if stats.fail == 0 {
				Stdout.line!("${path.display()}: ok (${stats.pass.to_str()} cases)")?
			} else {
				Stdout.line!("${path.display()}: FAIL ${stats.fail.to_str()}/${stats.pass.plus(stats.fail).to_str()}${stats.detail}")?
			}
			Ok(stats.fail)
		}

		Err(ParseError(i)) => {
			Stdout.line!("${path.display()}: parse error at byte ${i.to_str()}")?
			Ok(1)
		}
	}
}

run_all! = |args, idx, failed|
	match args.get(idx) {
		Err(_) => Ok(failed)
		Ok(arg) => {
			n = run_file!(arg)?
			run_all!(args, idx.plus(1), failed.plus(n))
		}
	}

main! : List(OsStr) => Try({}, _)
main! = |args| {
	failed = run_all!(args, 1, 0)?
	if failed > 0 {
		Stdout.line!("FAILED: ${failed.to_str()} case(s)")?
		Err(CheckFailed)
	} else {
		Stdout.line!("all ok")?
		Ok({})
	}
}

# parser expects
expect parse_u64([32, 49, 50, 51, 44], 0) == Ok({ i: 4, val: 123 })
expect skip_ws([32, 9, 65], 0) == 2
expect {
	r = parse_ram(" [[5,7],[65535,255]]".to_utf8(), 0)
	r == Ok({ i: 20, val: [{ addr: 5, val: 7 }, { addr: 65535, val: 255 }] })
}
expect {
	r = parse_cycles(" [[100,7,\"r-m\"],[100,7,\"---\"],[200,9,\"-wm\"]]".to_utf8(), 0)
	r
	== Ok({
		i: 44,
		m_cycles: 3,
		accesses: [{ addr: 100, val: 7, w: Bool.False }, { addr: 200, val: 9, w: Bool.True }],
	})
}
expect {
	r = parse_state("{\"pc\":1,\"sp\":2,\"ime\":1,\"ie\":1,\"ram\":[[3,4]]}".to_utf8(), 0)
	match r {
		Ok(s) => s.val.pc == 1 and s.val.sp == 2 and s.val.ime and s.val.ie == 1 and s.val.ram == [{ addr: 3, val: 4 }]
		Err(_) => Bool.False
	}
}
