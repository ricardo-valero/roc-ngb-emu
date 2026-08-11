# Design — vertical-verify-slices (revised)

## Context

See proposal.md. Round one (superseded, visible in this branch's first commit) used a curl-with-cache helper into `$PWD/rom` and kept ladder/check scripts; user review 2026-08-10 redirected to the design below. Standing constraints: Roc apps can't compile inside the Nix sandbox (platform bundles need network), so checks still `roc run` at runtime; flakes only see tracked files.

## Goals / Non-Goals

**Goals:** every check is one folder; ROMs are store paths with pinned hashes; one uniform passlist mechanism; uniform `check-<name>` naming; devshell free of checks.

**Non-Goals:** hermetic Roc builds (own spike); changing what any check verifies.

## Decisions

- **`check/<name>/package.nix` → `packages.check-<name>`**, listed explicitly in the flake (user: no readDir magic). Uniform with the pre-existing `check-acid2`/`check-sound` naming the user wants kept.
- **ROMs are `fetchurl` store paths.** Each slice inlines a bare list of `{ url, hash }` — the user's "just a list of URLs" plus the integrity hash Nix requires. Display/passlist names derive from the URL basename (percent-decoded); store names sanitize spaces. Hashes gathered once with `nix store prefetch-file`. The mooneye set comes from the official `.tar.xz` via `fetchurl` + a small `runCommand` extraction, so it's equally pinned and cached.
- **One passlist mechanism, per slice.** A `passlist` file in the slice: listed ROMs gate (fail ⇒ suite fails; the set never shrinks), unlisted report informatively with a promotion hint. blargg = 13 gating (12 cpu_instrs + instr_timing) + 4 informative timing ROMs; mooneye = 6 gating + rest informative; sound = `01-registers.gb` gating + 11 informative singles. This is `run-ladder`'s logic generalized — so `run-ladder` is deleted, not migrated.
- **`package/Harness.roc` now, not later.** With three slice runners the protocol detection (serial text, `$A000` memory protocol, mooneye Fibonacci bytes) would otherwise exist in three copies; as pure `GameBoy ->` functions it belongs in the package, tested by `roc test`.
- **One shared verdict runner at `check/run.roc`** (user review, round three): the three per-slice runners came out byte-identical because the file holds zero slice-specific logic — it's generic infrastructure like Harness, so it lives once at the `check/` level. Verticality means slices own what makes them *distinct* (ROM list, passlist, golden, packaging); genuinely unique programs (`acid2/main.roc` frame dumper, `sound/wav.roc`) stay in their slices.
- **acid2/sound goldens live in their slices** (`check/<name>/golden.sha256`); compare-or-create and bless-by-deletion semantics unchanged; viewable bless/mismatch artifacts are written next to the golden and gitignored.
- **`example/frame.roc` and `example/wav.roc` move into their slices** (acid2's main.roc, sound's wav.roc) — they were the checks' engines; both remain runnable by hand and the README documents the new paths. `example/` keeps the genuinely standalone `cartridge.roc` and `debug.roc`.
- **`rom/` is app-only** (`play.gb`), populated explicitly by the user.

## Risks / Trade-offs

- [Upstream re-uploads would change hashes] → fetchurl fails loudly on mismatch; that's the feature.
- [Three thin runners can drift] → the drift-prone part (protocols) is in `Harness.roc` under `roc test`; the loops are trivially small.
- [Gating regressions during the split] → blargg+mooneye passlists are copied verbatim from `golden/ladder.passlist` + the 12 cpu_instrs; task 4 re-runs everything.

## Open Questions

- None blocking.

## Round four (user review): Nix-native ROMs, Roc-native checks

- **One pinned source for retrio/gb-test-roms** (`check/gb-test-roms.nix`, `fetchFromGitHub` rev `c240dd7d` + hash) replaces every per-file URL+hash pair; slices list plain repo-relative paths. Mirrors the mooneye tarball pattern.
- **Check logic lives in Roc, not bash.** `check/run.roc` (suite loop, passlist gating, promotion hints), `check/acid2/main.roc --check`, and `check/sound/main.roc` implement everything the shell scripts did; each `package.nix` is now just "fetch pinned ROMs, invoke the Roc program with store paths". Exit-3-on-bless became exit-1 (basic-cli maps Err to 1) — still nonzero, so CI still can't bless silently.
- **`package/Sha256.roc`**: SHA-256 in pure Roc (FIPS vectors as expects) so golden digests stay byte-compatible with the sha256sum-era files — the untouched goldens passing is the end-to-end proof. Nightly quirks found on the way, worth remembering: type errors can defer to runtime as "dispatch on a value that can never exist" (the U64 shift-amount param is U8), and the flow analyzer emits spurious UNCONDITIONAL CONDITION warnings on `??`-with-effectful-call results.

## Round five (user review): dmg_sound conformance merges into blargg

The dmg_sound singles' pass/fail runs are the same kind of thing as
cpu_instrs — Blargg ROM through the verdict runner against a passlist — so
they join `check/blargg` (passlist gains `01-registers.gb`), and
`check/sound` shrinks to its true identity: the golden WAV regression of
01-registers only (no passlist, one ROM arg). Merging immediately paid
off: `04-sweep.gb` and `06-overflow on trigger.gb` turned out to pass and
were promoted — blargg now gates 16.
