app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy

# Headless debug dump: run a ROM for N frames (default 300), then write the
# core's debug renders as P6 PPM images into a directory:
#   background.ppm (256x256), tiles.ppm (128x192), oam.ppm (64x80)
# Usage: roc run example/debug.roc -- <rom.gb> <out-dir> [frames]

main! : List(OsStr) => Try({}, _)
main! = |args| {
    parsed = parse_args(args)?
    rom = parsed.rom_path.read_bytes!()?
    var gb = GameBoy.init(rom)
    var i = parsed.frames
    while i > 0 {
        gb = gb.run_frame(GameBoy.no_buttons({}))
        i = i.minus(1)
    }
    write_view!(parsed.out_dir, "background.ppm", 256, 256, gb.debug_background())?
    write_view!(parsed.out_dir, "tiles.ppm", 128, 192, gb.debug_tiles())?
    write_view!(parsed.out_dir, "oam.ppm", 64, 80, gb.debug_oam())?
    Stdout.line!("wrote background/tiles/oam PPMs to ${parsed.out_dir} after ${parsed.frames.to_str()} frames")?
    Ok({})
}

write_view! : Str, Str, U64, U64, List(U8) => Try({}, _)
write_view! = |dir, name, width, height, shades| {
    path = Path.utf8("${dir}/${name}")
    path.write_bytes!(ppm(width, height, shades))?
    Ok({})
}

ppm : U64, U64, List(U8) -> List(U8)
ppm = |width, height, shades| {
    var bytes = "P6\n${width.to_str()} ${height.to_str()}\n255\n".to_utf8()
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

parse_args : List(OsStr) -> Try({ rom_path : Path, out_dir : Str, frames : U64 }, [FailedToReadArgs(Str), ..])
parse_args = |args|
    match args {
        [_, rom_arg, dir_arg] =>
            Ok({ rom_path: Path.from_os_str(rom_arg), out_dir: Path.from_os_str(dir_arg).display(), frames: 300 })

        [_, rom_arg, dir_arg, frames_arg, ..] =>
            Ok({ rom_path: Path.from_os_str(rom_arg), out_dir: Path.from_os_str(dir_arg).display(), frames: parse_u64(frames_arg) })

        _ => Err(FailedToReadArgs("usage: <rom.gb> <out-dir> [frames]"))
    }

parse_u64 : OsStr -> U64
parse_u64 = |os_str|
    Path.from_os_str(os_str).display().to_utf8().fold(0, |acc, byte|
        if byte >= 48 and byte <= 57 {
            acc * 10 + U8.minus(byte, 48).to_u64()
        } else {
            acc
        })
