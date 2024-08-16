app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.13.0/nW9yMRtZuCYf1Oa9vbE5XoirMwzLbtoSgv7NGhUlqYA.tar.br",
    emu: "../package/main.roc",
}

import cli.Stdout
import cli.Path
import cli.Arg
import cli.Task exposing [Task]
import emu.Cartridge

readArgFilePath : Task.Task Path.Path _
readArgFilePath =
    args = Arg.list! {}
    when args is
        [_, pathStr, ..] -> Task.ok (Path.fromStr pathStr)
        _ -> Task.err (FailedToReadArgs "expected path argument")

main = run |> Task.onErr \err -> crash "ERROR: $(Inspect.toStr err)"

run =
    romPath = readArgFilePath!
    # romPath = Path.withExtension path "gb"
    isFile = Path.isFile! romPath
    cart = cartLoad! romPath
    if isFile then
        # Stdout.line "File with path: $(Inspect.toStr romPath)"
        Stdout.line "Header: $(Inspect.toStr cart)"
    else
        Stdout.line "Invalid file"

cartLoad = \path ->
    romData = Path.readBytes! path
    Task.ok (Cartridge.header romData)
