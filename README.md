# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** playable, with a sound core! The SM83 CPU passes all 12 Blargg
`cpu_instrs` ROMs (including the combined MBC1 one), the PPU renders
[dmg-acid2](https://github.com/mattcurrie/dmg-acid2) pixel-perfect, MBC1/MBC3
cartridges work, and the four-channel APU synthesizes audio at 48 kHz —
verified by Blargg's `dmg_sound` register test and a frozen WAV digest.
A [roc-ray](https://github.com/lukewilliamboswell/roc-ray) app plays ROMs in
a window with keyboard input. Not yet: speaker output (blocked on a roc-ray
PCM-streaming API), battery saves (same story for file writes), MBC3 RTC.

Play a ROM — the app embeds `rom/play.gb` at **build time**, so swap the
file first, then rebuild (all inside `nix develop`):

```bash
nix run .#fetch-roms                # first time: seeds rom/play.gb with dmg-acid2
cp your-game.gb rom/play.gb         # any 32 KiB / MBC1 / MBC3 ROM
roc build example/play.roc && ./play
```

Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
Esc quits.

Run the verification suites (test ROMs are fetched on first run):

```bash
nix run .#run-blargg          # CPU: Blargg cpu_instrs, 12 ROMs
nix run .#check-acid2         # PPU: dmg-acid2 vs frozen reference digest
nix run .#check-sound         # APU: dmg_sound 01-registers + frozen WAV digest
```

Dump any ROM's screen to an image, or inspect a cartridge header:

```bash
roc run example/frame.roc -- rom/dmg-acid2.gb out.ppm 120
roc run example/wav.roc -- rom/dmg_sound/01-registers.gb out.wav 180
roc run example/cartridge.roc -- <rom-path>.gb
```

Develop:

```bash
roc check package/main.roc    # type-check the package
roc test package/main.roc     # run all inline expects
```

Development uses the new (Zig-based) Roc compiler, pinned via the Nix flake.
The pre-migration code (2024 Roc syntax and toolchain) lives on the `legacy`
branch. Rendering will target [roc-ray](https://github.com/lukewilliamboswell/roc-ray)
(see `spike/rocray-hello/SPIKE.md` for the toolchain pairing notes).

Get in touch and let's work on this together!

I'm using these resources:

- 📄 [Gameboy Pan Docs](https://gbdev.io/pandocs)
- 🎥 [Gameboy Emulator Development - Low Level Devel](https://www.youtube.com/watch?v=e87qKixKFME&list=PLVxiWMqQvhg_yk4qy2cSC3457wZJga_e5)
- 📝 [Writing an emulator](https://blog.tigris.fr/2019/07/09/writing-an-emulator-the-first-steps/)
- 🧪 [Blargg's test ROMs](https://github.com/retrio/gb-test-roms) (CPU verification oracle)

Heavily inspired by:

- [Elmboy](https://github.com/Malax/elmboy)
