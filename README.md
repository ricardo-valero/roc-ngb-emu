# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** playable, with a sound core! The SM83 CPU passes all 12 Blargg
`cpu_instrs` ROMs (including the combined MBC1 one), the PPU renders
[dmg-acid2](https://github.com/mattcurrie/dmg-acid2) pixel-perfect, MBC1/MBC3
cartridges work, and the four-channel APU synthesizes audio at 48 kHz —
verified by Blargg's `dmg_sound` register test and a frozen WAV digest.
A [roc-ray](https://github.com/lukewilliamboswell/roc-ray) app plays ROMs in
a window with keyboard input, and the same core runs **in the browser** via
WebGPU on the [roc-web](https://github.com/ricardo-valero/roc-web) platform
(see below) — pixel-identical to native, 90+ fps. Not yet: speaker output (the browser path unblocks this
via Web Audio; roc-ray needs a PCM-streaming API), battery saves, MBC3 RTC.

Play a ROM — the app embeds `rom/play.gb` at **build time**, so swap the
file first, then rebuild (all inside `nix develop`):

```bash
nix run .#fetch-roms                # first time: seeds rom/play.gb with dmg-acid2
cp your-game.gb rom/play.gb         # any 32 KiB / MBC1 / MBC3 ROM
roc build example/play.roc && ./play
```

Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
Esc quits.

## Play in the browser (roc-web)

The browser app lives here (`example/web.roc` + `web/`), built on
[roc-web](https://github.com/ricardo-valero/roc-web) — a wasm32 Roc
platform (Zig host + WebGPU renderer) referenced by release-bundle URL,
just like roc-ray. Inside `nix develop`:

```bash
roc build example/web.roc --output=web/play.wasm   # embeds rom/play.gb
python3 -m http.server 8642 --directory web        # open http://localhost:8642/
```

Same controls as the windowed app (no Esc — it's a browser tab). The page
picks WebGPU and falls back to Canvas2D; the status line shows which.
Version discipline mirrors roc-ray: a roc-web release pairs with the Roc
nightly it was built against — bump the platform URL and the flake pin
together. Full spike evidence and benchmarks:
`openspec/changes/wasm-platform-spike/report.md`.

## Verification

Run the suites (test ROMs are fetched on first run):

```bash
nix run .#run-blargg          # CPU: Blargg cpu_instrs, 12 ROMs
nix run .#check-acid2         # PPU: dmg-acid2 vs golden digest
nix run .#check-sound         # APU: dmg_sound 01-registers + golden WAV digest
nix run .#run-ladder          # accuracy ladder: Blargg timing + mooneye halt/timer
```

Golden checks use compare-or-create: digests live in `golden/`; blessing a
new golden = delete the `.sha256` and re-run (writes a reviewable image/WAV
alongside, exits 3 so CI can never bless silently). The ladder gates the
ROMs listed in `golden/ladder.passlist` and reports the rest informatively —
promote a ROM by adding its name once it passes.

Dump any ROM's screen or debug views (background map, tiles, OAM) to images,
or inspect a cartridge header:

```bash
roc run example/frame.roc -- rom/dmg-acid2.gb out.ppm 120
roc run example/debug.roc -- rom/dmg-acid2.gb out-dir 120
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
branch. Frontends: [roc-ray](https://github.com/lukewilliamboswell/roc-ray)
for the native window (see `spike/rocray-hello/SPIKE.md` for toolchain
pairing notes) and [roc-web](https://github.com/ricardo-valero/roc-web) for
the browser — each platform release pairs with a Roc nightly; bump platform
URLs and the flake pin together.

Get in touch and let's work on this together!

I'm using these resources:

- 📄 [Gameboy Pan Docs](https://gbdev.io/pandocs)
- 🎥 [Gameboy Emulator Development - Low Level Devel](https://www.youtube.com/watch?v=e87qKixKFME&list=PLVxiWMqQvhg_yk4qy2cSC3457wZJga_e5)
- 📝 [Writing an emulator](https://blog.tigris.fr/2019/07/09/writing-an-emulator-the-first-steps/)
- 🧪 [Blargg's test ROMs](https://github.com/retrio/gb-test-roms) (CPU verification oracle)

Heavily inspired by:

- [Elmboy](https://github.com/Malax/elmboy)
