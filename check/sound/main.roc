app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy
import ngb.Sha256

# APU audio regression: render 180 frames of the given ROM (01-registers)
# as a WAV and compare its digest against ./golden.sha256 with
# compare-or-create semantics — a missing golden is written along with a
# listenable golden.wav and the run exits nonzero (a bless must be a
# deliberate act, never a CI pass); a mismatch keeps actual.wav.
# The dmg_sound conformance suite lives in check/blargg.
# Usage: roc run check/sound/main.roc -- <rom.gb>

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, rom_arg, ..] => check!(Path.from_os_str(rom_arg))
        _ => Err(FailedToReadArgs("usage: <rom.gb>"))
    }
}

check! : Path => Try({}, _)
check! = |rom_path| {
    wav = render_wav(rom_path.read_bytes!()?, 180)
    actual = Sha256.hex(wav)
    golden_path = Path.utf8("check/sound/golden.sha256")
    golden_bytes = golden_path.read_bytes!() ?? List.repeat(0x00.U8, 0)
    if is_empty(golden_bytes) {
        golden_path.write_bytes!("${actual}\n".to_utf8())?
        Path.utf8("check/sound/golden.wav").write_bytes!(wav)?
        Stdout.line!("GOLDEN CREATED  check/sound/golden.sha256")?
        Stdout.line!("      listen to check/sound/golden.wav before committing the .sha256")?
        Err(GoldenCreated)
    } else {
        expected = Str.from_utf8(golden_bytes) ?? ""
        if expected.contains(actual) {
            Stdout.line!("PASS  WAV matches the golden digest")?
            Ok({})
        } else {
            Path.utf8("check/sound/actual.wav").write_bytes!(wav)?
            Stdout.line!("FAIL  WAV digest mismatch")?
            Stdout.line!("      expected: ${expected}")?
            Stdout.line!("      actual:   ${actual}")?
            Stdout.line!("      actual WAV kept at check/sound/actual.wav for listening")?
            Err(DigestMismatch)
        }
    }
}

render_wav : List(U8), U64 -> List(U8)
render_wav = |rom, frames| {
    var gb = GameBoy.init(rom)
    var pcm = List.repeat(0x00.U8, 0)
    var i = frames
    while i > 0 {
        gb = gb.run_frame(GameBoy.no_buttons({}))
        drained = gb.take_samples()
        gb = drained.gb
        var j = 0
        while j < drained.samples.len() {
            pcm = append_i16(pcm, drained.samples.get(j) ?? 0.0)
            j = j.plus(1)
        }
        i = i.minus(1)
    }
    wav_header(pcm.len()).concat(pcm)
}

# F32 in [-1, 1] to little-endian signed 16-bit PCM
append_i16 : List(U8), F32 -> List(U8)
append_i16 = |bytes, sample| {
    centered = F32.to_u64_try((sample + 1.0) * 32000.0) ?? 32000 # 0..64000
    value =
        if centered >= 32000 {
            centered.minus(32000)
        } else {
            U64.minus(65536, 32000.minus(centered))
        }
    bytes.append(value.to_u8_wrap()).append(value.shr_zf_wrap(8).to_u8_wrap())
}

le32 : U64 -> List(U8)
le32 = |v| [v.to_u8_wrap(), v.shr_zf_wrap(8).to_u8_wrap(), v.shr_zf_wrap(16).to_u8_wrap(), v.shr_zf_wrap(24).to_u8_wrap()]

le16 : U64 -> List(U8)
le16 = |v| [v.to_u8_wrap(), v.shr_zf_wrap(8).to_u8_wrap()]

wav_header : U64 -> List(U8)
wav_header = |data_len|
    "RIFF".to_utf8()
        .concat(le32(data_len.plus(36)))
        .concat("WAVE".to_utf8())
        .concat("fmt ".to_utf8())
        .concat(le32(16)) # PCM chunk size
        .concat(le16(1)) # PCM format
        .concat(le16(2)) # stereo
        .concat(le32(48000))
        .concat(le32(48000 * 4)) # byte rate
        .concat(le16(4)) # block align
        .concat(le16(16)) # bits per sample
        .concat("data".to_utf8())
        .concat(le32(data_len))

is_empty : List(U8) -> Bool
is_empty = |bytes| bytes.len() == 0
