app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy

# Headless frame capture: run a ROM for N frames (default 300, ~5s of
# emulated time) and write the final framebuffer as a P6 PPM image.
# Usage: roc run example/frame.roc -- <rom.gb> <out.ppm> [frames]

main! : List(OsStr) => Try({}, _)
main! = |args| {
    parsed = parse_args(args)?
    rom = parsed.rom_path.read_bytes!()?
    var gb = GameBoy.init(rom)
    var i = parsed.frames
    while i > 0 {
        gb = gb.run_frame()
        i = i.minus(1)
    }
    parsed.out_path.write_bytes!(ppm(gb.framebuffer()))?
    Stdout.line!("wrote ${parsed.out_path.display()} after ${parsed.frames.to_str()} frames")?
    Ok({})
}

ppm : List(U8) -> List(U8)
ppm = |shades| {
    var bytes = "P6\n160 144\n255\n".to_utf8()
    var i = 0
    while i < shades.len() {
        gray =
            match shades.get(i) ?? 0 {
                0 => 255
                1 => 170
                2 => 85
                _ => 0
            }
        bytes = bytes.append(gray).append(gray).append(gray)
        i = i.plus(1)
    }
    bytes
}

parse_args : List(OsStr) -> Try({ rom_path : Path, out_path : Path, frames : U64 }, [FailedToReadArgs(Str), ..])
parse_args = |args|
    match args {
        [_, rom_arg, out_arg] =>
            Ok({ rom_path: Path.from_os_str(rom_arg), out_path: Path.from_os_str(out_arg), frames: 300 })

        [_, rom_arg, out_arg, frames_arg, ..] =>
            Ok({ rom_path: Path.from_os_str(rom_arg), out_path: Path.from_os_str(out_arg), frames: parse_u64(frames_arg) })

        _ => Err(FailedToReadArgs("usage: <rom.gb> <out.ppm> [frames]"))
    }

parse_u64 : OsStr -> U64
parse_u64 = |os_str|
    Path.from_os_str(os_str).display().to_utf8().fold(0, |acc, byte|
        if byte >= 48 and byte <= 57 {
            acc * 10 + U8.minus(byte, 48).to_u64()
        } else {
            acc
        })
