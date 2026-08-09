# Modernize Roc Toolchain

## Why

The project is pinned to a Roc nightly from 2024-08-23, built by the old Rust compiler. Since then, Roc was rewritten in Zig with a substantially different syntax, and the `roc-lang/roc` repository no longer ships a `flake.nix` — our flake input (`roc.packages.${system}.full`) is dead and will fail to resolve on any update. The codebase (~1,400 lines of Roc) cannot compile under the new toolchain until its syntax is migrated. Modernizing now, before the 0.1.0 release later in 2026, keeps the project alive.

## What Changes

- Create a `legacy` branch pointing at the current `main` HEAD to preserve the pre-migration state; all new work continues on `main`.
- **BREAKING**: Rewrite `flake.nix` to drop the `github:roc-lang/roc` input and consume `github:roc-lang/roc-overlay` instead, pinned to a dated nightly tag (e.g. `nightly-2026-08-07-8d23662`) for reproducibility. Replace `nil` with `nixd` as the Nix language server, drop the `formatter` output (alejandra), and refresh `flake.lock` and nixpkgs.
- Remove the `.zed/` editor settings folder (Nix LSP/formatter config only, no longer used).
- **BREAKING**: Migrate all Roc sources from 2024-era syntax to the new compiler's syntax (`|args|` lambdas, snake_case identifiers and builtins, `and`/`or` operators, `match` in place of `when ... is`, updated headers as required). Files: `package/main.roc`, `package/Bit.roc`, `package/Constant.roc`, `package/Cpu/Alu.roc`, `package/Cpu/Instruction.roc`, `package/Cpu/Register.roc`, `package/Cpu/Register/Status.roc`, `package/Cartridge/Header.roc`, `examples/cartridge.roc`.
- Behavior is preserved: all existing inline `expect` tests must pass unchanged in meaning under the new toolchain.

## Capabilities

### New Capabilities

- `dev-environment`: Reproducible Nix devshell providing the new Zig-based Roc compiler (pinned nightly via roc-overlay) plus the `nixd` Nix language server.
- `roc-package`: The emulator package and examples compile (`roc check`) and pass their inline tests (`roc test`) under the pinned new-compiler toolchain.

### Modified Capabilities

<!-- none — no prior specs exist -->

## Impact

- `flake.nix`, `flake.lock`: rewritten inputs/outputs; anyone entering the devshell gets the new compiler.
- `.zed/`: deleted.
- All `.roc` files (~1,400 lines across 9 files): syntax rewritten; largest are `Cpu/Instruction.roc` (609 lines) and `Cartridge/Header.roc` (399 lines).
- Git branches: new `legacy` branch; `main` moves forward with the migration.
- The old toolchain cannot build the migrated code (and vice versa) — the `legacy` branch is the escape hatch.
