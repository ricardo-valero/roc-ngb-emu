# Pure-Roc Checks: Retire the Nix Check Harness

## Why

roc-nes-emu's checks (a deliberate deviation decided 2026-08-11) proved the pattern: each check is a basic-cli Roc app run directly (`roc check/<name>/main.roc -- ...`), with a sibling `fetch.roc` that downloads its ROMs, and gating via a committed passlist. Here, the same runners already exist as basic-cli Roc apps — the nix layer (`package.nix` per check + `gb-test-roms.nix`) only wraps `roc build && run` and supplies nix-store ROM paths. That wrapper costs a nix evaluation on every check run, a second place ROM lists live, and a harness style the sibling has already abandoned. Converging removes the last non-dev-shell nix from the repo and makes the two emulators' check folders read the same.

## What Changes

- Each check slice gains a `fetch.roc` (basic-cli + roc-lang/http, sibling-style) that downloads its ROMs into `check/<name>/data/` (untracked, as today) from commit-pinned URLs — and, improving on the sibling, verifies each file against a pinned SHA-256 using the existing pure-Roc `check/lib/Sha256`.
- Runners take ROM paths as arguments with `check/<name>/data/` defaults; suite checks (blargg, mooneye) keep their committed passlists; digest goldens (acid2, sound) are unchanged — hashing was already pure Roc.
- `check/*/package.nix`, `check/gb-test-roms.nix`, and the flake's `check-*` packages are deleted; the flake keeps only the devshell. README swaps `nix run .#check-*` for `roc check/<name>/main.roc` invocations.
- Out of scope: any change to what the checks verify (passlists, goldens, budgets), roc-nes-emu itself, CI.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

_None — `skip_specs: true`. The verification specs (`cpu-verification`, `apu-verification`, `ppu-verification`) state behavior contracts (headless runners, suite gating via passlist, compare-or-create goldens, ROMs fetched into an untracked directory) that hold identically before and after; none of them names nix as the harness. The dev-environment spec covers only the devshell, which stays. Entry-point spelling is implementation detail._

## Impact

- **Removed**: `check/blargg/package.nix`, `check/mooneye/package.nix`, `check/acid2/package.nix`, `check/sound/package.nix`, `check/gb-test-roms.nix`, flake `check-*` packages (flake.nix shrinks to devshell).
- **Added**: `check/<name>/fetch.roc` per slice (ROM lists move here from the nix files, keeping their pins), small arg-handling in runners for default data dirs.
- **Docs**: README check instructions; WISHLIST if it references the nix harness.
- **Risk**: the fetch apps depend on GitHub raw-content availability at fetch time (same as the sibling; nix had the same upstream dependency, cached differently). Hash verification keeps the pinning guarantee. This machine's flaky DNS for github.com is a known quirk with a documented workaround.
