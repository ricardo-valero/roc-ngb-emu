# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** playable, with a sound core! The SM83 CPU passes all 12 Blargg
`cpu_instrs` ROMs (including the combined MBC1 one), the PPU renders
[dmg-acid2](https://github.com/mattcurrie/dmg-acid2) pixel-perfect, MBC1/MBC3/MBC5
cartridges work, and the four-channel APU synthesizes audio at 48 kHz —
verified by Blargg's `dmg_sound` register test and a frozen WAV digest.
A [roc-ray](https://github.com/lukewilliamboswell/roc-ray) app plays ROMs in
a window with keyboard input, and the same core runs **in the browser** via
WebGPU on the [roc-web](https://github.com/ricardo-valero/roc-web) platform
(see below) — pixel-identical to native, 90+ fps, **with sound** and
runtime ROM loading (drop a `.gb` on the page). Not yet: speaker output in
the native app (roc-ray needs a PCM-streaming API), battery saves, MBC3 RTC.

The repo splits into `package/` (the emulator core, pure Roc),
`app/` (the two frontends: `ray.roc` native window, `web/` browser),
`check/` (vertical check slices — each owns its runner, hash-pinned ROM
list, passlist/golden, and packaging), and `example/` (headless dev tools
that exercise the core).

Play a ROM in a native window — the ray app embeds `rom/play.gb` at
**build time**, so put the file there first, then build (all inside
`nix develop`):

```bash
cp your-game.gb rom/play.gb              # any 32 KiB / MBC1 / MBC3 ROM
roc build app/ray.roc --output=ray && ./ray
```

Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
Esc quits.

## Play in the browser (roc-web)

The browser app lives in `app/web/` — `main.roc` (a near-twin of
`app/ray.roc`: same config-and-`render!` shape, same `host.key_down`
buttons), a few-line `index.html`, and the vendored `lib/` from
[roc-web](https://github.com/ricardo-valero/roc-web), the wasm32 Roc
platform referenced by release-bundle URL just like roc-ray. ROMs load at
**runtime**: the page fetches `play.gb` by default, and dropping any `.gb`
onto the page (or the picker) swaps games without a rebuild. Sound works
(48 kHz APU output via an AudioWorklet — press a key to unmute, a browser
autoplay rule). Inside `nix develop`:

```bash
roc build app/web/main.roc --output=app/web/play.wasm
cp rom/play.gb app/web/                                 # default ROM the page fetches
python3 -m http.server 8642 --directory app/web         # open http://localhost:8642/
```

Same controls as the windowed app (no Esc — it's a browser tab). The app
config picks the renderer (`Auto` = WebGPU → WebGL → Canvas2D); the status
line shows the backend, fps, and audio buffer health. Version discipline
mirrors roc-ray: a roc-web release (bundle + `lib/`) pairs with the Roc
nightly it was built against — bump the platform URL, the vendored lib,
and the flake pin together. Spike evidence and benchmarks:
`openspec/changes/archive/2026-08-10-wasm-platform-spike/report.md`.

## Checks

Each check is a vertical slice under `check/<name>/`: its runner, its
ROM list (fetched by Nix with pinned hashes — no shared ROM folder), and
its passlist or golden, wired up in `flake.nix`:

```bash
nix run .#check-blargg        # Blargg suites: cpu_instrs, timing, dmg_sound
nix run .#check-mooneye       # timing/halt: mooneye acceptance subset
nix run .#check-acid2         # PPU: dmg-acid2 vs golden digest
nix run .#check-sound         # APU: golden WAV digest of 01-registers
```

ROM suites gate on the slice's `passlist` (listed ROMs must pass — the set
never shrinks; the rest report informatively — promote a ROM by adding its
name once it passes). Golden digests use compare-or-create: blessing =
delete the slice's `golden.sha256` and re-run (writes a reviewable
image/WAV next to it, exits 3 so CI can never bless silently).

Dump any ROM's screen, debug views (background map, tiles, OAM), or audio
to files, or inspect a cartridge header:

```bash
roc run check/acid2/main.roc -- <rom>.gb out.ppm 120
roc run example/debug.roc -- <rom>.gb out-dir 120
roc run check/sound/main.roc -- <rom>.gb out.wav 180
roc run example/cartridge.roc -- <rom>.gb
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
