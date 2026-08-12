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
# passlist, the rest are ROM paths. Listed ROMs gate — any failure exits
# nonzero, so the passing set never shrinks; unlisted ROMs report
# informatively with a promotion hint at the end.

chunk_steps : U64
chunk_steps = 100_000

max_chunks : U64
max_chunks = 2_000 # ~200M instructions, far beyond any of these ROMs

main! : List(OsStr) => Try({}, _)
main! = |args| {
    match args {
        [_, passlist_arg, ..] => {
            passlist_bytes = Path.from_os_str(passlist_arg).read_bytes!()?
            passlist = "\n${Str.from_utf8(passlist_bytes) ?? ""}\n"
            stats = suite!(args, 2, passlist, { gated: 0, failed: 0, promote: "" })?
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

        _ => Err(FailedToReadArgs("usage: <passlist> <rom.gb>..."))
    }
}

Stats : { gated : U64, failed : U64, promote : Str }

suite! : List(OsStr), U64, Str, Stats => Try(Stats, _)
suite! = |args, i, passlist, acc| {
    match args.get(i) {
        Err(_) => Ok(acc)
        Ok(rom_arg) => {
            rom_path = Path.from_os_str(rom_arg)
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
            suite!(args, i.plus(1), passlist, next)
        }
    }
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
        gb = match gb.step() { (g, _) => g }
    }
    gb
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
