app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	ngb: "../package/main.roc",
	chk: "lib/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy
import chk.Harness

# Suite runner shared by the ROM check slices. First arg is the slice's
# passlist; the rest are ROM paths, defaulting to every .gb in the data/
# directory beside the passlist (seeded by the slice's fetch.roc or
# committed). Listed ROMs gate — any failure exits nonzero, so the passing
# set never shrinks; unlisted ROMs report informatively with a promotion
# hint at the end.
#
#   roc check/run.roc -- check/blargg/passlist
#   roc check/run.roc -- check/mooneye/passlist some/other.gb

chunk_steps : U64
chunk_steps = 100_000

max_chunks : U64
max_chunks = 2_000 # ~200M instructions, far beyond any of these ROMs

main! : List(OsStr) => Try({}, _)
main! = |args| {
	match args {
		[_, passlist_arg] => {
			passlist_path = Path.from_os_str(passlist_arg)
			run_suite!(passlist_path, data_roms!(passlist_path)?)
		}

		[_, passlist_arg, ..] => run_suite!(Path.from_os_str(passlist_arg), rest_as_paths(args))

		_ => Err(FailedToReadArgs("usage: <passlist> [rom.gb ...] (default ROMs: <passlist dir>/data/*.gb)"))
	}
}

# Every .gb under the data/ directory next to the passlist, name-sorted.
data_roms! : Path => Try(List(Path), _)
data_roms! = |passlist_path| {
	dir = "${dirname(passlist_path.display())}/data"
	entries = Path.utf8(dir).list!()?
	roms = entries.keep_if(|p| p.display().ends_with(".gb"))
	if roms.is_empty() {
		Err(NoRomsFetched("no .gb files in ${dir}/ — run the slice's fetch.roc first"))
	} else {
		Ok(sort_by_display(roms))
	}
}

rest_as_paths : List(OsStr) -> List(Path)
rest_as_paths = |args| {
	var out = List.repeat(Path.utf8(""), 0)
	var i = 2
	while i < args.len() {
		match args.get(i) {
			Ok(a) => {
				out = out.append(Path.from_os_str(a))
			}

			Err(_) => {}
		}
		i = i.plus(1)
	}
	out
}

run_suite! : Path, List(Path) => Try({}, _)
run_suite! = |passlist_path, roms| {
	passlist_bytes = passlist_path.read_bytes!()?
	passlist = "\n${Str.from_utf8(passlist_bytes) ?? ""}\n"
	stats = suite!(roms, 0, passlist, { gated: 0, failed: 0, promote: "" })?
	Stdout.line!("----")?
	if stats.promote != "" {
		Stdout.line!("promotable (add to the passlist):${stats.promote}")?
	} else {
		{}
	}
	if stats.failed > 0 {
		Stdout.line!("FAILED: ${stats.failed.to_str()} gating ROM(s) regressed")?
		Err(SuiteFailed)
	} else {
		Stdout.line!("ok (${stats.gated.to_str()} gating)")?
		Ok({})
	}
}

Stats : { gated : U64, failed : U64, promote : Str }

suite! : List(Path), U64, Str, Stats => Try(Stats, _)
suite! = |roms, i, passlist, acc| {
	match roms.get(i) {
		Err(_) => Ok(acc)
		Ok(rom_path) => {
			name = basename(rom_path.display())
			r = check_rom!(rom_path)?
			gating = passlist.contains("\n${name}\n")
			next =
				if gating and r.passed {
					Stdout.line!("PASS  ${name}")?
					{ ..acc, gated: acc.gated.plus(1) }
				} else if gating {
					Stdout.line!("FAIL  ${name} (gating)")?
					Stdout.line!("      serial: ${r.serial}")?
					Stdout.line!("      memory: ${r.memory}")?
					{ ..acc, gated: acc.gated.plus(1), failed: acc.failed.plus(1) }
				} else if r.passed {
					Stdout.line!("pass  ${name} (informative)")?
					{ ..acc, promote: "${acc.promote}\n  ${name}" }
				} else {
					Stdout.line!("fail  ${name} (informative)")?
					acc
				}
			suite!(roms, i.plus(1), passlist, next)
		}
	}
}

# Insertion sort on display strings' bytes; ROM lists are ~30 entries.
sort_by_display : List(Path) -> List(Path)
sort_by_display = |paths| {
	var out = List.repeat(Path.utf8(""), 0)
	var i = 0
	while i < paths.len() {
		match paths.get(i) {
			Err(_) => {}
			Ok(p) => {
				var j = 0
				while j < out.len() and !display_lt(p, out.get(j) ?? p) {
					j = j.plus(1)
				}
				out = insert_at(out, j, p)
			}
		}
		i = i.plus(1)
	}
	out
}

insert_at : List(Path), U64, Path -> List(Path)
insert_at = |list, at, item| {
	var out = List.repeat(Path.utf8(""), 0)
	var i = 0
	while i < list.len() {
		if i == at {
			out = out.append(item)
		} else {
			{}
		}
		match list.get(i) {
			Ok(p) => {
				out = out.append(p)
			}

			Err(_) => {}
		}
		i = i.plus(1)
	}
	if at >= list.len() {
		out.append(item)
	} else {
		out
	}
}

display_lt : Path, Path -> Bool
display_lt = |a, b| {
	xs = a.display().to_utf8()
	ys = b.display().to_utf8()
	var i = 0
	var verdict = 0.U8 # 0 undecided, 1 less, 2 not less
	while verdict == 0 {
		match (xs.get(i), ys.get(i)) {
			(Err(_), Ok(_)) => {
				verdict = 1
			}

			(Err(_), Err(_)) => {
				verdict = 2
			}

			(Ok(_), Err(_)) => {
				verdict = 2
			}

			(Ok(x), Ok(y)) =>
				if x < y {
					verdict = 1
				} else if x > y {
					verdict = 2
				} else {
					i = i.plus(1)
				}
			}
	}
	verdict == 1
}

check_rom! : Path => Try({ passed : Bool, serial : Str, memory : Str }, _)
check_rom! = |rom_path| {
	rom = rom_path.read_bytes!()?
	var gb = GameBoy.init(rom)
	var verdict = 0.U8 # 0 running, 1 passed, 2 failed, 3 out of budget
	var chunks = max_chunks
	while verdict == 0 {
		if chunks == 0 {
			verdict = 3
		} else {
			chunks = chunks.minus(1)
			gb = run_chunk(gb)
			v = Harness.verdict(gb)
			if v == Passed {
				verdict = 1
			} else if v == Failed {
				verdict = 2
			}
		}
	}
	Ok({ passed: verdict == 1, serial: Harness.serial_text(gb), memory: Harness.memory_text(gb) })
}

run_chunk : GameBoy -> GameBoy
run_chunk = |gb0| {
	var gb = gb0
	var i = chunk_steps
	while i > 0 {
		i = i.minus(1)
		gb = match gb.step() {
			(g, _) => g
		}
	}
	gb
}

# Path prefix up to the last '/', or "." when there is none.
dirname : Str -> Str
dirname = |path| {
	bytes = path.to_utf8()
	var end = 0
	var i = 0
	while i < bytes.len() {
		if (bytes.get(i) ?? 0) == 0x2F {
			end = i
		} else {
			{}
		}
		i = i.plus(1)
	}
	if end == 0 {
		"."
	} else {
		var out = List.repeat(0x00.U8, 0)
		var j = 0
		while j < end {
			out = out.append(bytes.get(j) ?? 0)
			j = j.plus(1)
		}
		Str.from_utf8(out) ?? "."
	}
}

basename : Str -> Str
basename = |path| {
	bytes = path.to_utf8()
	var start = 0
	var i = 0
	while i < bytes.len() {
		if (bytes.get(i) ?? 0) == 0x2F {
			start = i.plus(1)
		} else {
			{}
		}
		i = i.plus(1)
	}
	var out = List.repeat(0x00.U8, 0)
	var j = start
	while j < bytes.len() {
		out = out.append(bytes.get(j) ?? 0)
		j = j.plus(1)
	}
	Str.from_utf8(out) ?? path
}
