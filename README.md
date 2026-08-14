# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** playable, with a sound core! The SM83 CPU passes all 12 Blargg
`cpu_instrs` ROMs (including the combined MBC1 one), the PPU renders
[dmg-acid2](https://github.com/mattcurrie/dmg-acid2) and [cgb-acid2](https://github.com/mattcurrie/cgb-acid2) pixel-perfect (CGB color, double-speed, and VRAM DMA — CGB games are playable), MBC1/MBC3/MBC5
cartridges work, and the four-channel APU synthesizes audio at 48 kHz —
verified by Blargg's `dmg_sound` register test and a frozen WAV digest.
A [roc-ray](https://github.com/ricardo-valero/roc-ray) app (our fork, which
adds binary file I/O and PCM audio streaming) plays ROMs in a window **with
sound** and keyboard input and loads them at runtime, and the same core runs
**in the browser** via WebGPU on the
[roc-web](https://github.com/ricardo-valero/roc-web) platform (see below) —
pixel-identical to native, 90+ fps, with sound and runtime ROM loading
(drop a `.gb` on the page). Battery saves persist — `<rom>.sav` on disk
natively (the ecosystem's raw-RAM format, with the 48-byte RTC footer),
IndexedDB in the browser — and the MBC3 real-time clock runs, halts,
latches, and catches up across sessions from the footer timestamp.
Wall-clock time enters the pure core as per-frame *data* (`{ buttons,
now }`), so headless runs stay deterministic. Not yet: save states
(BESS — see WISHLIST).

The repo splits into `package/` (the emulator core, pure Roc),
`app/` (the two frontends: `ray.roc` native window, `web/` browser),
`check/` (vertical check slices — each owns its runner, hash-pinned ROM
list, passlist/golden, and packaging), and `example/` (headless dev tools
that exercise the core).

Play a ROM in a native window — the ray app reads the ROM from disk at
**startup** (first argument, else `rom/play.gbc`), so swapping games
needs no rebuild. The platform is the local
[roc-ray fork](https://github.com/ricardo-valero/roc-ray) checkout at
`../roc-ray` (branch `file-io`); build its host once with `zig build`
there, then (inside `nix develop`):

```bash
roc build app/ray.roc --output=ray
./ray your-game.gb                       # any 32 KiB / MBC1 / MBC3 / MBC5 ROM
./ray                                    # plays rom/play.gbc
```

Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
Esc quits.

Sound plays through the fork's PCM stream: the APU's 48 kHz stereo output
is pushed to the host each frame, so game audio just works (silence, not
a crash or pitch warble, if emulation ever stalls).

Battery-backed games save to `<rom-path>.sav` next to the ROM — loaded at
startup, written back when the game saves (the RAM-disable edge) and on
Esc. The file is the ecosystem's raw cart-RAM format (plus the 48-byte
RTC footer on MBC3 clock carts), so saves move freely between this
emulator, others, and flashcarts.

## Play in the browser (roc-web)

The browser app lives in `app/web/` — `main.roc` (a near-twin of
`app/ray.roc`: same config-and-`render!` shape, same `host.key_down`
buttons), a few-line `index.html`, and the vendored `lib/` from
[roc-web](https://github.com/ricardo-valero/roc-web), the wasm32 Roc
platform — currently referenced as the local `../roc-web` checkout, which
carries the battery/clock contract until a v0.4.0 bundle is released.
ROMs load at **runtime**: the page fetches `play.gb` by default, and
dropping any `.gb` onto the page (or the picker) swaps games without a
rebuild. Sound works (48 kHz APU output via an AudioWorklet — press a key
to unmute, a browser autoplay rule). Battery saves persist in IndexedDB,
keyed by cartridge header (title + checksum), so the fetched default and
a dropped copy of the same game share one save; they're written on the
game's own save moments and when the tab hides. Inside `nix develop`:

```bash
roc build app/web/main.roc --output=app/web/play.wasm
cp rom/play.gb app/web/                                 # default ROM the page fetches
roc http_server.roc -- --port 8642 --dir app/web        # open http://localhost:8642/
```

The server is pure Roc too ([basic-webserver](https://github.com/roc-lang/basic-webserver)
0.16.0): a declared file root with host-enforced MIME types and path
safety — no python in the loop. It knows nothing about the emulator: a
generic static server whose flags default to python's (`--port 8000`,
`--dir .`).

Same controls as the windowed app (no Esc — it's a browser tab). The app
config picks the renderer (`Auto` = WebGPU → WebGL → Canvas2D); the status
line shows the backend, fps, and audio buffer health. Version discipline
mirrors roc-ray: a roc-web release (bundle + `lib/`) pairs with the Roc
nightly it was built against — bump the platform URL, the vendored lib,
and the flake pin together. Spike evidence and benchmarks:
`openspec/changes/archive/2026-08-10-wasm-platform-spike/report.md`.

## Checks

Each check is a vertical slice under `check/<name>/`: its runner, its
ROMs in `data/` (fetched by the slice's `fetch.roc` — pinned URLs,
SHA-256-verified, kept untracked by a local `.gitignore`), and its
passlist or golden. Pure Roc, no nix beyond the devshell (matching
sibling roc-nes-emu) — including the archive handling: mooneye's
upstream ships one `.tar.gz`, which its fetch unpacks with the pure-Roc
`check/lib` DEFLATE decoder and tar reader, and acid2's fetch follows
GitHub's release-asset redirects itself:

```bash
roc check/blargg/fetch.roc    # once: fetch the Blargg ROMs
roc check/mooneye/fetch.roc   # once: fetch + unpack the mooneye subset
roc check/acid2/fetch.roc     # once: fetch dmg-acid2 + cgb-acid2
roc check/sound/fetch.roc     # once: fetch the sound-check ROM

roc check/run.roc -- check/blargg/passlist    # Blargg: cpu_instrs, timing, dmg_sound
roc check/run.roc -- check/mooneye/passlist   # timing/halt: mooneye acceptance subset
roc check/acid2/main.roc                      # PPU: dmg/cgb-acid2 vs golden digests
roc check/sound/main.roc                      # APU: golden WAV digest of 01-registers
roc check/battery/main.roc                    # battery/.sav/RTC: synthetic carts, no ROMs
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
