# Design: Modernize Roc Toolchain

## Context

The repo is a Roc package (Game Boy emulator building blocks: CPU registers, ALU, instruction decoding, cartridge header parsing) with inline `expect` tests, developed against a Roc nightly from 2024-08-23. Roc's compiler has since been rewritten in Zig with a new syntax; the old flake input (`github:roc-lang/roc` exposing `packages.${system}.full`) no longer exists upstream. Verified facts as of 2026-08-09:

- `roc-lang/roc` main has no `flake.nix` (checked via GitHub API).
- `roc-lang/roc-overlay` is the official Nix flake mirroring new-compiler nightly binaries; exposes `packages.${system}.nightly` and pinned tags like `packages.${system}."nightly-2026-08-07-8d23662"`; supports aarch64-darwin.
- The new compiler recently hit feature parity; 0.1.0 targeted for late 2026. Only nightlies exist.

## Goals / Non-Goals

**Goals:**
- Devshell provides a working, pinned new-compiler Roc toolchain via Nix.
- All Roc sources compile and their inline expects pass under that toolchain, with behavior unchanged.
- Pre-migration state preserved on a `legacy` branch.
- Repo hygiene: drop unused `.zed/` settings.

**Non-Goals:**
- No new emulator functionality or refactors beyond what the syntax forces.
- No expansion of `package/main.roc` exposures (it exposes only `Cartridge.Header` today; revisit separately).
- No CI setup.
- Not tracking `nightly` floating tag — we pin a dated tag.

## Decisions

1. **`legacy` branch off current main; new work stays on `main`.** Alternative (new `v2` branch, main frozen) rejected: main should remain the living default branch for defaults/CI/visitors; the legacy state stays one checkout away with full history.

2. **Nix via `roc-lang/roc-overlay`, pinned to a dated nightly tag.** Alternatives: building roc from source in nix (slow, unsupported upstream right now), floating `packages.nightly` (non-reproducible day to day). A dated tag (e.g. `nightly-2026-08-07-8d23662`) plus `flake.lock` gives double pinning; bumping is an explicit tag edit. Add `roc-overlay.inputs.nixpkgs.follows = "nixpkgs"` to avoid a second nixpkgs. Swap `nil` for `nixd` as the Nix language server and drop the `formatter`/alejandra output entirely (user decision — flake gets simpler: inputs + devshell only). Keep the flake's `flakeExposed` systems pattern but constrain to the overlay's four supported systems if evaluation fails elsewhere.

3. **Hand-migrate syntax under the new compiler, calibrated by real errors.** Alternative (`roc format --migrate` from the last Rust-compiler nightlies) rejected as the primary path: it requires hunting down and running a second, dead toolchain for ~1,400 lines of highly regular code. Instead: migrate `Bit.roc` first, run `roc check` on it, and use the actual compiler errors to fix our assumptions about the final syntax (lambda form, builtin names, `match` shape, header/package syntax) before touching the large files. Known syntax deltas to apply: `\x ->` → `|x|`, camelCase → snake_case (including builtins like `Num.shiftLeftBy` → shifted equivalents), `&&`/`||` → `and`/`or`, `when ... is` → `match`, plus whatever headers/builtin renames `roc check` reveals.

4. **Migration order: small → large.** `main.roc`, `Constant.roc`, `Bit.roc`, `Register/Status.roc`, `Register.roc`, `Alu.roc`, then `Cartridge/Header.roc` (399) and `Instruction.roc` (609), then `examples/cartridge.roc`. Each file must pass `roc check` before the next; `roc test` gates completion.

## Confirmed Syntax Rules (calibrated on Bit.roc, nightly-2026-08-07)

- `module [...]` headers are deprecated → headerless **type modules**: the file defines one top-level type matching the filename, methods attached via `Name := [...].{ ... }` (nominal) or `Name :: ...` (opaque).
- Calls are parenthesized: `f(a, b)`. Lambdas: `|a, b| body`. Blocks use `{ }`.
- `when x is` → `match x { Pattern => expr ... }`; tag payloads use `Tag(payload)`; alternatives `A | B`.
- snake_case everywhere; `&&`/`||` → `and`/`or`; `Bool.true` → `Bool.True`/`True`.
- Builtins are per-type with static dispatch: `byte.bitwise_and(x)`, `U8.shl_wrap(a, n)` (shifts renamed: `shiftLeftBy` → `shl_wrap`, `shiftRightZfBy` → `shr_zf_wrap`; count taken modulo bit-width).
- Type application uses parens: `List(U8)`; `Result` → `Try(ok, err)` with `Ok`/`Err` tags.
- Dots in import names are **package qualifiers**, not directories. Subdirectory modules: `import Cartridge/Header as Header`, then expose the alias in `package [Header] {}`.
- Top-level `expect`s live outside the type-module block; `roc check <file>` loads the whole containing package, so errors must be filtered per file until all modules migrate.

## Risks / Trade-offs

- [Assumed new-syntax details are wrong (my knowledge predates the final parity build)] → Calibration step on `Bit.roc` before bulk migration; trust compiler errors over prior assumptions.
- [Pinned nightly has a bug affecting `expect`/`roc test`] → Pin is a plain string; try an adjacent dated tag. The pre-parity compiler is young — if a blocker surfaces, record it and pick a newer nightly.
- [Mechanical rename changes semantics silently (e.g. wrong builtin substitution)] → Inline `expect`s are the safety net; they encode current behavior and must pass unmodified in meaning.
- [`nix run github:roc-lang/roc` style commands in docs/readme drift] → README mentions little; check and update alongside.
- [Overlay lacks a system someone uses] → Overlay covers x86_64/aarch64 for linux+darwin; acceptable.

## Migration Plan

1. `git branch legacy` (no checkout) — preserves current state; push both branches.
2. Rewrite `flake.nix`; `nix flake lock`; enter devshell; `roc version` sanity check.
3. Delete `.zed/`.
4. File-by-file syntax migration in the order above.
5. `roc check` on every file + `roc test` on the package and example as final verification.

Rollback: `legacy` branch, or revert the migration commits on main.

## Open Questions

- Exact pinned tag: pick the newest dated tag available at implementation time rather than hardcoding `nightly-2026-08-07-8d23662` now.
- Whether the new compiler changed `package`/`module` header syntax — resolved by the calibration step.
