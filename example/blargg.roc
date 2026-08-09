app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
    ngb: "../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.GameBoy

# Headless Blargg runner: execute a test ROM until its serial output reports
# "Passed"/"Failed", or the cycle budget runs out. Exit 0 only on "Passed".

chunk_steps : U64
chunk_steps = 100_000

max_chunks : U64
max_chunks = 2_000 # ~200M instructions, far beyond any cpu_instrs ROM

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
            text = serial_text(gb)
            if text.contains("Passed") {
                verdict = 1
            } else if text.contains("Failed") {
                verdict = 2
            }
        }
    }
    Stdout.line!("serial: ${serial_text(gb)}")?
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
        gb = step_gb(gb)
    }
    gb
}

step_gb : GameBoy -> GameBoy
step_gb = |gb| match gb.step() { (g, _) => g }

serial_text : GameBoy -> Str
serial_text = |gb| Str.from_utf8(gb.serial()) ?? ""

read_arg_file_path : List(OsStr) -> Try(Path, [FailedToReadArgs(Str), ..])
read_arg_file_path = |args|
    match args {
        [_, path_arg, ..] => Ok(Path.from_os_str(path_arg))
        _ => Err(FailedToReadArgs("expected path argument"))
    }
