# Core Debug API (wasmboy-inspired, platform-independent)

## Why

A study of wasmboy (2026-08-10) identified three core-side patterns that make a future browser frontend and debugger cheap — and all of them are pure Roc, valuable today through the existing headless apps, and independent of whether the wasm platform spike (companion change `wasm-platform-spike`) succeeds. Doing this work now keeps the core the single source of truth: debug visualizations and stepping semantics live in `package/`, so every frontend (roc-ray, headless, future web) gets them for free.

## What Changes

- **Run-until with reason codes**: generalize `GameBoy.run_frame` into a `run_until` entry point that returns a reason tag — `FrameReady`, `AudioBufferFull(threshold)`, `Breakpoint`, or an error — mirroring wasmboy's `executeUntilCondition` return codes. `run_frame` remains as the frame-only convenience. Includes a program-counter breakpoint (the minimum a debugger needs to step).
- **PPU debug renderers as pure core functions**: `Ppu` gains on-demand renders of the full 256×256 background map, the tile-data sheet, and OAM sprites — `GameBoy -> List(U8)` pixel buffers, following wasmboy's "core renders, host blits" rule. A headless example dumps them as PPM images so they are usable and testable now.
- **Golden harness upgrades**: verification adopts wasmboy's compare-or-create semantics — a missing golden file is written (blessing = delete + re-run), and image oracles write a human-viewable PNG/PPM alongside the digest for review on mismatch.
- **Accuracy ladder extension**: add Blargg `instr_timing`/`mem_timing` and the mooneye halt/timer ROM set to the CPU verification suite (wasmboy's curated selection), fetched like existing test ROMs.
- Explicitly out of scope: save states (requires `GameBoy` serialization — a separate design problem), audio-buffer threshold plumbing to any host, and all frontend/UI work (lives with the wasm platform or the future web debugger app).

## Capabilities

### New Capabilities

- `core-debug`: the core's debugging surface — run-until stepping with reason codes, PC breakpoints, and pure-function debug renders (background map, tile data, OAM) with a headless dump path.

### Modified Capabilities

- `cpu-verification`: the suite grows beyond `cpu_instrs` — timing and halt/timer conformance ROMs (Blargg `instr_timing`, `mem_timing`; mooneye halt/timer set) must pass.
- `ppu-verification`: golden oracles change from "frozen digest, hand-managed" to compare-or-create with a human-viewable image written alongside the digest.
- `apu-verification`: same compare-or-create semantics for the WAV digest oracle.

## Impact

- `package/GameBoy.roc` (run_until, breakpoint state), `package/Ppu.roc` (debug renderers), `package/Mmu.roc` (possibly: PC/register exposure is already available via the record).
- `example/` — a debug-dump app (or extension of `frame.roc`); `example/blargg.roc` grows the new ROM ladder.
- `nix/fetch-roms.nix` and check scripts — new test ROMs, compare-or-create flow.
- No platform, host, or dependency changes; everything runs on basic-cli headless. Companion change: `wasm-platform-spike` (new repo bootstrap) consumes `run_until` and the debug renders later but does not block on them.
