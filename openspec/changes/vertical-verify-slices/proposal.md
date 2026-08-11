# Vertical verify slices (first slice: blargg)

## Why

The repo is organized by layer, so one logical thing — "the Blargg check" — is smeared across `example/blargg.roc`, `nix/run-blargg.nix`, `nix/fetch-roms.nix`, and `flake.nix`; adding the accuracy ladder (2026-08-10) touched five locations for one feature. The monolithic `fetch-roms` also makes every check download every suite's ROMs (run-blargg pulls the mooneye tarball it never uses). Decided with the user 2026-08-10: reorganize toward vertical slices — each check owns its code, its ROM list, its packaging, and (eventually) its goldens — starting with a **single slice** to prove the pattern before migrating the rest.

## What Changes

- **New `verify/blargg/` slice**: `main.roc` (the runner, moved from `example/blargg.roc`, behavior identical) + `package.nix` (fetches *only* the cpu_instrs ROMs via a shared curl-with-cache helper, then runs the suite) — invoked as `nix run .#verify-blargg`.
- **Flake auto-discovery**: `flake.nix` discovers slices by reading `verify/` and `callPackage`-ing each `package.nix`; adding a future slice never edits the flake.
- **Shared fetch helper in `nix/lib.nix`**: fetching-with-cache into the untracked `rom/` dir is infrastructure, not slice identity; each slice passes its own ROM list.
- **Removals**: `nix/run-blargg.nix`; the cpu_instrs section of `nix/fetch-roms.nix` (the rest of fetch-roms stays until its suites migrate); the `rom/play.gb` auto-seeding block in fetch-roms (user decision 2026-08-10: app concern hiding in the ROM fetcher — the apps' contract becomes explicit, README documents `cp rom/dmg-acid2.gb rom/play.gb` as the zero-ROM option). `example/blargg.roc` moves rather than being deleted — but note check-sound and run-ladder currently invoke it by path, so they are repointed at `verify/blargg/main.roc` without other changes.
- **Riding along (user decision)**: `app/ray/main.roc` → `app/ray.roc` — a single-file app needs no folder.
- Explicitly out of scope, queued for follow-up changes once the pattern is proven: migrating acid2/sound/ladder into slices (dissolving `golden/` and most of `fetch-roms`), extracting the runner's pass/fail protocol detection into a pure `package/Harness.roc` (do it when the *second* slice consumer arrives), and hermetic `roc build` inside Nix derivations (blocked on sandbox networking; slices run roc at runtime like today's scripts).

## Capabilities

_None — `skip_specs: true`. Specs describe behavior (a headless runner exists; 12 ROMs pass) and name no paths or commands; this change moves files and rewires packaging without changing any observable check behavior. If a later slice migration changes a check's interface (e.g. bless flow), that change declares the delta._

## Impact

- Files: `verify/blargg/{main.roc,package.nix}` (new), `nix/lib.nix` (new), `flake.nix` (discovery), `nix/run-blargg.nix` (deleted), `nix/fetch-roms.nix` (shrunk), `nix/check-sound.nix` + `nix/run-ladder.nix` (path repoint only), `app/ray.roc` (rename), README (commands + layout note).
- Verification: `nix run .#verify-blargg` must report 12/12 exactly as `run-blargg` did; check-sound, run-ladder, check-acid2, `roc test package/main.roc`, and both app builds must pass unchanged.
- Risk: low — no core code changes; the moved runner is byte-identical apart from nothing; the flake discovery is the only new mechanism and is exercised by the one slice.
