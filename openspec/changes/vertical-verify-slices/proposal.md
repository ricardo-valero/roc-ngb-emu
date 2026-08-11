# Vertical check slices (revised after user review)

## Why

The repo is organized by layer, so one logical thing — "the Blargg check" — is smeared across `example/`, `nix/`, `golden/`, and `flake.nix`. The first implementation round (2026-08-10) proved a single slice but kept too much of the old machinery; user review redirected it: ROMs should be fetched **the Nix way** (`fetchurl` + hash, store paths, no shared `rom/` cache for checks), every ROM suite should carry a **per-slice passlist** (which dissolves `run-ladder` — it was never a separate concern, just the passlist mechanism), naming should be uniform (`check-blargg`, `check-acid2`, `check-sound`), and checks don't belong in the devshell.

## What Changes

- **`check/<name>/` slices, listed explicitly in the flake as `packages.check-<name>`**:
  - `check/blargg/` — `passlist` + packaging with inline ROM list (cpu_instrs set **plus** the Blargg timing ROMs from the old ladder) + `passlist` (the 12 cpu_instrs + `instr_timing.gb` gate; `mem_timing*` informative until they pass).
  - `check/mooneye/` — runner + the mooneye halt/timer acceptance ROMs (extracted from the official tarball in a fixed-output-style derivation) + `passlist` (the 6 currently passing).
  - `check/acid2/` — the frame-dump app (moved from `example/frame.roc`, still usable by hand) + `golden.sha256` (moved from `golden/`), compare-or-create semantics unchanged.
  - `check/sound/` — verdict runner + WAV renderer (moved from `example/wav.roc`) + `passlist` (`01-registers.gb` gates, singles informative) + `golden.sha256`.
- **ROMs via `pkgs.fetchurl` with hashes** (per-slice inline plain list of `{ url, hash }`, display names derived from URL basenames). No curl loops, no `$PWD/rom` for checks, hash-verified, Nix-cached.
- **`package/Harness.roc`**: the pass/fail protocol detection (Blargg serial text, Blargg memory signature, mooneye Fibonacci bytes) moves into the package as pure functions — it has multiple consumers now, so the extraction earlier deferred is due. Slice runners become thin ~40-line files; no cross-slice paths remain.
- **Deletions**: `nix/run-blargg.nix` (already gone), `nix/run-ladder.nix`, `nix/check-acid2.nix`, `nix/check-sound.nix`, `nix/fetch-roms.nix`, `nix/lib.nix` (the curl helper from round one), `golden/` (contents move into slices), `verify/` (renamed `check/`).
- **Devshell**: checks removed (run them via `nix run .#check-<name>`); shell keeps the toolchain only.
- **`rom/` remains solely for `rom/play.gb`** (the user-supplied game the apps embed).
- Retained from round one: `app/ray.roc` rename. Later review rounds: flake lists slices explicitly (no readDir discovery), no convenience ROM package, one shared `check/run.roc` runner.

## Capabilities

_None — `skip_specs: true`, unchanged: specs name behavior, not layout or commands. The checks' observable gate behavior is preserved (same ROMs gate; same goldens; ladder's gating set is preserved verbatim across the split into blargg/mooneye passlists)._

## Impact

- Files: `check/{blargg,mooneye,acid2,sound}/`, `package/Harness.roc`, `flake.nix`, README; deletions listed above; `example/` shrinks to `cartridge.roc` + `debug.roc`.
- Verification: `check-blargg` (12 cpu_instrs + instr_timing gate, mem_timing informative), `check-mooneye` (6 gate), `check-acid2` and `check-sound` digests unchanged, 204+ package tests (Harness expects move with the code), both apps build.
- Risk: protocol code moves (not rewritten) into `Harness.roc`; the gating set is copied verbatim from `golden/ladder.passlist`; hashes pinned at authoring time via `nix store prefetch-file`.
