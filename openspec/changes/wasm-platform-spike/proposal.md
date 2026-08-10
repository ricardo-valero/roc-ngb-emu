# wasm32 Roc Platform — Spike (new repo bootstrap)

## Why

The wasmboy exploration (2026-08-10) concluded a browser frontend is the natural next home for the emulator: the pinned Roc nightly ships a `wasm32` target, roc-wasm4/Rocci Bird proves "Roc app + Zig host → browser" on the new compiler, and a browser host dissolves the blockers that parked `rocray-file-io` (binary file IO for ROM loading and battery saves, PCM streaming for the silent APU). WASM-4 itself was ruled out earlier (64 KB console cap); this is a platform of our own with ordinary wasm memory. Two things are unproven and must be answered before any platform investment: the new compiler's platform-authoring story for wasm32, and whether the immutable-record core hits 60 fps in wasm.

## What Changes

- **New repository** (name open — working title `roc-wasm-frame`; GB-agnostic: framebuffer + input + audio + storage, the emulator is just its first consumer) containing:
  - A minimal Roc platform declaring only the `wasm32` target, with a Zig host (`wasm32-freestanding`) providing the Roc runtime hooks (`roc_alloc` etc.) and a fixed export surface, cribbed from roc-wasm4's build wiring.
  - A JS harness page: instantiate the module, call `init`/`run_frame`, render the framebuffer via WebGPU (per-frame texture write + fullscreen triangle, WGSL, behind a pluggable renderer seam — the stack is Zig + wasm + WebGPU from day one, per the multi-emulator ambition; WebGPU calls live in JS glue, keeping the Zig host freestanding).
- **Spike milestones**, in order, each a go/no-go gate:
  1. *Hello wasm*: any Roc app entrypoint called from the browser via the Zig host.
  2. *Emulator frame*: the unchanged `roc-ngb-emu` package (consumed from a sibling checkout) runs `GameBoy.run_frame`; framebuffer pixels appear on canvas.
  3. *Benchmark*: sustained frames-per-second measured on a real game ROM, plus `--opt` variants; recorded in the spike report.
- **Spike report** committed back to this change (`report.md`): performance numbers, platform-authoring friction, and a recommendation — proceed to a real platform (follow-up changes: platform API design in the new repo, web app + debugger consumer here) or stop and fall back to the roc-ray upstream path.
- This change is tracked here because it gates this repo's roadmap; the code lands in the new repo. No `roc-ngb-emu` code changes (the package must be consumable as-is — if the spike reveals it is not, that finding goes in the report and becomes its own change).

## Capabilities

_None — `skip_specs: true`. No spec-level behavior of this repo changes; the spike's output is knowledge (the report) plus an external scaffold. If the spike passes, the new repo starts its own OpenSpec, and this repo gets a follow-up change for the web frontend consumer._

## Impact

- New external repo (scaffold + spike code). This repo: only `openspec/changes/wasm-platform-spike/report.md`.
- Dependencies (new repo): Zig toolchain matching the Roc nightly's expectations, static file server for the harness page. Nix flake mirroring this repo's pin discipline (see rocray pairing rule — same idea: platform release must pair with a Roc nightly).
- Risk contained by design: the spike is throwaway-by-default; only the report is a committed deliverable of this change.
