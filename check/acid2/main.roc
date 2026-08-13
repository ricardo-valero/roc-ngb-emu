app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	ngb: "../../package/main.roc",
	chk: "../lib/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy
import chk.Sha256

# Headless frame capture, and the acid2 golden check.
# Default:    roc check/acid2/main.roc
#   check both committed ROMs (data/) against their goldens.
# Tool mode:  roc check/acid2/main.roc -- <rom.gb> <out.ppm> [frames]
#   run a ROM for N frames (default 300) and write the framebuffer as PPM.
# Check mode: roc check/acid2/main.roc -- --check <rom.gb> <golden-prefix>
#   render 120 frames and compare the PPM digest against <prefix>.sha256
#   with compare-or-create semantics: a missing golden is written together
#   with a reviewable golden.ppm and the run exits nonzero (a bless must be
#   a deliberate act, never a CI pass); a mismatch keeps actual.ppm.

main! : List(OsStr) => Try({}, _)
main! = |args| {
	match args {
		[_] => {
			check!(Path.utf8("check/acid2/data/dmg-acid2.gb"), "check/acid2/golden")?
			check!(Path.utf8("check/acid2/data/cgb-acid2.gbc"), "check/acid2/cgb-golden")
		}

		[_, flag_arg, rom_arg, prefix_arg] =>
			if Path.from_os_str(flag_arg).display() == "--check" {
				check!(Path.from_os_str(rom_arg), Path.from_os_str(prefix_arg).display())
			} else {
				dump!(args)
			}

		_ => dump!(args)
	}
}

check! : Path, Str => Try({}, _)
check! = |rom_path, prefix| {
	image = render(rom_path.read_bytes!()?, 120)
	actual = Sha256.hex(image)
	golden_path = Path.utf8("${prefix}.sha256")
	golden_bytes = read_or_empty!(golden_path)
	if is_empty(golden_bytes) {
		{
			golden_path.write_bytes!("${actual}\n".to_utf8())?
			Path.utf8("${prefix}.ppm").write_bytes!(image)?
			Stdout.line!("GOLDEN CREATED  ${prefix}.sha256")?
			Stdout.line!("      review ${prefix}.ppm against the published reference, then commit the .sha256")?
			Err(GoldenCreated)
		}
	} else {
		{
			expected = Str.from_utf8(golden_bytes) ?? ""
			if expected.contains(actual) {
				Stdout.line!("PASS  render matches ${prefix}.sha256")?
				Ok({})
			} else {
				Path.utf8("${prefix}-actual.ppm").write_bytes!(image)?
				Stdout.line!("FAIL  digest mismatch vs ${prefix}.sha256")?
				Stdout.line!("      expected: ${expected}")?
				Stdout.line!("      actual:   ${actual}")?
				Stdout.line!("      actual frame kept at ${prefix}-actual.ppm")?
				Err(DigestMismatch)
			}
		}
	}
}

dump! : List(OsStr) => Try({}, _)
dump! = |args| {
	parsed = parse_args(args)?
	parsed.out_path.write_bytes!(render(parsed.rom_path.read_bytes!()?, parsed.frames))?
	Stdout.line!("wrote ${parsed.out_path.display()} after ${parsed.frames.to_str()} frames")?
	Ok({})
}

render : List(U8), U64 -> List(U8)
render = |rom, frames| {
	var gb = GameBoy.init(rom)
	var i = frames
	while i > 0 {
		gb = gb.run_frame(GameBoy.no_buttons({}))
		i = i.minus(1)
	}
	ppm(gb.framebuffer())
}

ppm : List(U16) -> List(U8)
ppm = |pixels| {
	var bytes = "P6\n160 144\n255\n".to_utf8()
	var i = 0
	while i < pixels.len() {
		px = pixels.get(i) ?? 0
		bytes = bytes.append(expand5(px.shr_zf_wrap(10))).append(expand5(px.shr_zf_wrap(5))).append(expand5(px))
		i = i.plus(1)
	}
	bytes
}

expand5 : U16 -> U8
expand5 = |v| {
	c = v.bitwise_and(0x1F).to_u8_wrap()
	c.shl_wrap(3).bitwise_or(c.shr_zf_wrap(2))
}

parse_args : List(OsStr) -> Try({ rom_path : Path, out_path : Path, frames : U64 }, [FailedToReadArgs(Str), ..])
parse_args = |args|
	match args {
		[_, rom_arg, out_arg] =>
			Ok({ rom_path: Path.from_os_str(rom_arg), out_path: Path.from_os_str(out_arg), frames: 300 })

		[_, rom_arg, out_arg, frames_arg, ..] =>
			Ok({ rom_path: Path.from_os_str(rom_arg), out_path: Path.from_os_str(out_arg), frames: parse_u64(frames_arg) })

		_ => Err(FailedToReadArgs("usage: <rom.gb> <out.ppm> [frames] | --check <rom.gb>"))
	}

parse_u64 : OsStr -> U64
parse_u64 = |os_str|
	Path.from_os_str(os_str).display().to_utf8().fold(
		0,
		|acc, byte|
			if byte >= 48 and byte <= 57 {
				acc * 10 + U8.minus(byte, 48).to_u64()
			} else {
				acc
			},
	)

is_empty : List(U8) -> Bool
is_empty = |bytes| bytes.len() == 0

# Indirection on purpose: the flow analyzer constant-folds a `?? fallback`
# on an effectful call at the use site and emits a spurious warning that
# fails `roc build`; behind an effectful helper it does not.
read_or_empty! : Path => List(U8)
read_or_empty! = |path| path.read_bytes!() ?? List.repeat(0x00.U8, 0)
