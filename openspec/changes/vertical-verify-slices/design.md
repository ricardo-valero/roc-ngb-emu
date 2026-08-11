# Design — vertical-verify-slices (revised)

## Context

See proposal.md. Round one (superseded, visible in this branch's first commit) used a curl-with-cache helper into `$PWD/rom` and kept ladder/check scripts; user review 2026-08-10 redirected to the design below. Standing constraints: Roc apps can't compile inside the Nix sandbox (platform bundles need network), so checks still `roc run` at runtime; flakes only see tracked files.

## Goals / Non-Goals

**Goals:** every check is one folder; ROMs are store paths with pinned hashes; one uniform passlist mechanism; `flake.nix` never edited for new checks; devshell free of checks.

**Non-Goals:** hermetic Roc builds (own spike); changing what any check verifies.

## Decisions

- **`checks/<name>/package.nix` → `packages.check-<name>`** via readDir discovery. Uniform with the pre-existing `check-acid2`/`check-sound` naming the user wants kept.
- **ROMs are `fetchurl` store paths.** Each slice's `roms.nix` is a bare list of `{ url, hash }` — the user's "just a list of URLs" plus the integrity hash Nix requires. Display/passlist names derive from the URL basename (percent-decoded); store names sanitize spaces. Hashes gathered once with `nix store prefetch-file`. The mooneye set comes from the official `.tar.xz` via `fetchurl` + a small `runCommand` extraction, so it's equally pinned and cached.
- **One passlist mechanism, per slice.** A `passlist` file in the slice: listed ROMs gate (fail ⇒ suite fails; the set never shrinks), unlisted report informatively with a promotion hint. blargg = 13 gating (12 cpu_instrs + instr_timing) + 4 informative timing ROMs; mooneye = 6 gating + rest informative; sound = `01-registers.gb` gating + 11 informative singles. This is `run-ladder`'s logic generalized — so `run-ladder` is deleted, not migrated.
- **`package/Harness.roc` now, not later.** With three slice runners the protocol detection (serial text, `$A000` memory protocol, mooneye Fibonacci bytes) would otherwise exist in three copies; as pure `GameBoy ->` functions it belongs in the package, tested by `roc test`. Slice runners are thin per-slice mains (blargg/mooneye/sound share shape but not files — verticality over DRY for the 40-line loop; the shared logic lives below in the package).
- **acid2/sound goldens live in their slices** (`checks/<name>/golden.sha256`); compare-or-create and bless-by-deletion semantics unchanged; viewable bless/mismatch artifacts are written next to the golden and gitignored.
- **`example/frame.roc` and `example/wav.roc` move into their slices** (acid2's main.roc, sound's wav.roc) — they were the checks' engines; both remain runnable by hand and the README documents the new paths. `example/` keeps the genuinely standalone `cartridge.roc` and `debug.roc`.
- **`rom/` is app-only** (`play.gb`). The acid2 fetchurl is exposed as `packages.dmg-acid2-rom` so the zero-ROM path is `cp "$(nix build .#dmg-acid2-rom --print-out-paths)" rom/play.gb`.

## Risks / Trade-offs

- [Upstream re-uploads would change hashes] → fetchurl fails loudly on mismatch; that's the feature.
- [Three thin runners can drift] → the drift-prone part (protocols) is in `Harness.roc` under `roc test`; the loops are trivially small.
- [Gating regressions during the split] → blargg+mooneye passlists are copied verbatim from `golden/ladder.passlist` + the 12 cpu_instrs; task 4 re-runs everything.

## Open Questions

- None blocking.
