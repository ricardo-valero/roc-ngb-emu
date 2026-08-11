app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy
import ngb.Harness

# Headless test-ROM runner: execute a ROM until Harness reports a verdict
# or the cycle budget runs out. Exit 0 only on pass.

chunk_steps : U64
chunk_steps = 100_000

max_chunks : U64
max_chunks = 2_000 # ~200M instructions, far beyond any of these ROMs

main! : List(OsStr) => Try({}, _)
main! = |args| {
    rom_path = read_arg_file_path(args)?
    rom = rom_path.read_bytes!()?
    var gb = GameBoy.init(rom)
    var verdict = 0 # 0 running, 1 passed, 2 failed, 3 out of budget
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
    Stdout.line!("serial: ${Harness.serial_text(gb)}")?
    Stdout.line!("memory: ${Harness.memory_text(gb)}")?
    match verdict {
        1 => {
            Stdout.line!("PASSED")?
            Ok({})
        }

        2 => {
            Stdout.line!("FAILED")?
            Err(TestFailed)
        }

        _ => {
            Stdout.line!("TIMEOUT: no verdict within the cycle budget")?
            Err(TestTimedOut)
        }
    }
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

read_arg_file_path : List(OsStr) -> Try(Path, [FailedToReadArgs(Str), ..])
read_arg_file_path = |args|
    match args {
        [_, path_arg, ..] => Ok(Path.from_os_str(path_arg))
        _ => Err(FailedToReadArgs("expected path argument"))
    }
