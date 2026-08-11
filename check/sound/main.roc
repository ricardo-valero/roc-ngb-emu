app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy
import ngb.Harness
import ngb.Sha256

# The sound check: run the dmg_sound singles through the verdict runner
# (./passlist gates; the rest report informatively), then render 180 frames
# of 01-registers as a WAV and compare its digest against ./golden.sha256
# with compare-or-create semantics (a missing golden is written along with
# a listenable golden.wav and the run exits nonzero; a mismatch keeps
# actual.wav). Usage: roc run check/sound/main.roc -- <passlist> <rom.gb>...

chunk_steps : U64
chunk_steps = 100_000

max_chunks : U64
max_chunks = 2_000

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, passlist_arg, ..] => {
            Stdout.line!("== conformance (passlist gates)")?
            passlist_bytes = Path.from_os_str(passlist_arg).read_bytes!()?
            passlist = "\n${Str.from_utf8(passlist_bytes) ?? ""}\n"
            stats = suite!(args, 2, passlist, { gated: 0, failed: 0, promote: "", wav_rom: Path.utf8("") })?
            if stats.promote != "" {
                Stdout.line!("promotable (add to the passlist):${stats.promote}")?
            } else {
                {}
            }

            Stdout.line!("== golden WAV digest")?
            wav_result = wav_check!(stats.wav_rom)?

            Stdout.line!("----")?
            if stats.failed > 0 {
                Stdout.line!("FAILED: ${stats.failed.to_str()} gating ROM(s) regressed")?
                Err(SuiteFailed)
            } else {
                wav_result_check = wav_result # WAV verdict already reported
                _ = wav_result_check
                Stdout.line!("ok (${stats.gated.to_str()} gating)")?
                Ok({})
            }
        }

        _ => Err(FailedToReadArgs("usage: <passlist> <rom.gb>..."))
    }
}

Stats : { gated : U64, failed : U64, promote : Str, wav_rom : Path }

suite! : List(OsStr), U64, Str, Stats => Try(Stats, _)
suite! = |args, i, passlist, acc0| {
    match args.get(i) {
        Err(_) => Ok(acc0)
        Ok(rom_arg) => {
            rom_path = Path.from_os_str(rom_arg)
            name = basename(rom_path.display())
            acc =
                if name == "01-registers.gb" {
                    { ..acc0, wav_rom: rom_path }
                } else {
                    acc0
                }
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
            suite!(args, i.plus(1), passlist, next)
        }
    }
}

wav_check! : Path => Try({}, _)
wav_check! = |rom_path| {
    wav = render_wav(rom_path.read_bytes!()?, 180)
    actual = Sha256.hex(wav)
    golden_path = Path.utf8("check/sound/golden.sha256")
    golden_bytes = golden_path.read_bytes!() ?? List.repeat(0x00.U8, 0)
    if is_empty(golden_bytes) {
        {
            golden_path.write_bytes!("${actual}\n".to_utf8())?
            Path.utf8("check/sound/golden.wav").write_bytes!(wav)?
            Stdout.line!("GOLDEN CREATED  check/sound/golden.sha256")?
            Stdout.line!("      listen to check/sound/golden.wav before committing the .sha256")?
            Err(GoldenCreated)
        }
    } else {
        {
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
}

check_rom! : Path => Try({ passed : Bool, serial : Str, memory : Str }, _)
check_rom! = |rom_path| {
    rom = rom_path.read_bytes!()?
    var gb = GameBoy.init(rom)
    var verdict = 0
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
        gb = match gb.step() { (g, _) => g }
    }
    gb
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

is_empty : List(U8) -> Bool
is_empty = |bytes| bytes.len() == 0
