# Design — vertical-verify-slices

## Context

See proposal.md for motivation and the 2026-08-10 explore session for the full discussion. Constraints that shape the approach:

- `roc build`/`roc run` download platform bundles at first use, so Nix *derivations* can't compile Roc apps in the sandbox (no network). Today's checks run roc at runtime inside `writeShellApplication`; slices keep that model.
- `example/blargg.roc` is invoked by path from `check-sound.nix` (the `01-registers` gate + informative singles) and `run-ladder.nix`; those must keep working mid-migration.
- ROMs are untracked, cached under `rom/`; flakes only see tracked files (`git add` before `nix run`).
- Naming decided in exploration: top-level dir `verify/` (matches the `*-verification` spec vocabulary; `checks/` was rejected as misleading — these can't be `nix flake check` checks precisely because of the runtime-roc constraint).

## Goals / Non-Goals

**Goals:**

- One slice (`verify/blargg/`) that fully owns its code, ROM list, and packaging, invoked as `nix run .#verify-blargg`.
- Flake auto-discovery so slice #2 (and beyond) never edits `flake.nix`.
- A fetch helper that scopes downloads to what a slice actually needs.

**Non-Goals:**

- Migrating acid2/sound/ladder (follow-up changes, mechanical once the pattern is proven).
- `package/Harness.roc` extraction — premature with one consumer moved; do it when sound migrates and sharing actually begins.
- Hermetic Roc builds in Nix (would need the platform bundle as a fixed-output derivation + a way to point roc's cache at it — its own spike).
- Any change to what the check verifies or reports.

## Decisions

- **Slice layout `verify/<name>/{main.roc, package.nix}`**, goldens/passlists join their slice when their suite migrates. Alternative (flat `verify/blargg.roc` + `verify/blargg.nix`) rejected: the folder is the unit the flake discovers, and later slices carry extra files (goldens) anyway.
- **`package.nix` is a `callPackage`-style function returning one `writeShellApplication`** named `verify-<name>`, which (1) fetches its ROM list via the shared helper, (2) runs the suite by `roc run verify/<name>/main.roc -- <rom>` per ROM, preserving today's per-ROM PASS/FAIL output and exit semantics. Rationale: smallest possible contract; a slice is "a function from pkgs to a runnable check".
- **Discovery in `flake.nix`**: `builtins.readDir ./verify` filtered to directories, mapped to `packages."verify-${name}" = pkgs.callPackage ./verify/${name}/package.nix { inherit roc fetch-lib; }`. The devshell includes all discovered slices via the same attrset. Alternative (explicit list) rejected: defeats the never-edit-the-flake goal.
- **`nix/lib.nix` fetch helper**: a function `fetchRoms { dir, base?, roms }` producing shell text that curls each missing file into `rom/<dir>/` (URL-encoding spaces, mirroring today's loop). It generates *script text* consumed inside the slice's `writeShellApplication` rather than a derivation, because the ROM cache is runtime state in the working tree, not a store path. Alternative (per-slice inline curl loops) rejected: three near-identical loops existed in fetch-roms already; the drift risk is real.
- **Repoint, don't duplicate, the shared runner**: `check-sound.nix` and `run-ladder.nix` get a one-line path change to `verify/blargg/main.roc`. This creates two cross-slice references — accepted as *temporary* wiring debt, explicitly resolved by the Harness extraction when sound migrates. Alternative (copy the runner into each consumer now) rejected: three copies of protocol code with zero test coverage over the copies.
- **`rom/cpu_instrs/` cache location is unchanged**, so already-fetched ROMs are reused and `fetch-roms` (still fetching sound/acid2/ladder/play.gb) remains compatible during the transition.
- **`app/ray/main.roc` → `app/ray.roc`**: single-file apps sit directly under `app/`; `app/web/` stays a folder because it genuinely bundles two files. Update README and `/ray` build command accordingly.

## Risks / Trade-offs

- [Discovery makes packages implicit — a broken `package.nix` breaks `nix flake show` for everything] → One slice exercises the mechanism now; the pattern is `callPackage` with a fixed argument set, so new slices fail early and locally.
- [Cross-slice paths from check-sound/run-ladder into `verify/blargg/`] → Documented here as temporary; removed by the sound migration + Harness extraction follow-up.
- [Contributors habituated to `nix run .#run-blargg`] → README updated; the old attribute disappears rather than aliasing, so muscle memory fails loudly once instead of silently drifting.
- [Flake-invisible files: forgetting `git add verify/` makes discovery silently find nothing] → Known repo gotcha (memory: flakes only see tracked files); the task list makes `git add` explicit.

## Open Questions

- None blocking. Whether `verify-<name>` or `<name>` is the flake attribute prefix for future non-check slices can wait until a non-check slice exists.
