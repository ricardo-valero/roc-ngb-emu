# Tasks: Modernize Roc Toolchain

## 1. Branch and Toolchain Setup

- [x] 1.1 Create `legacy` branch pointing at current `main` HEAD (`git branch legacy`); push it if a remote exists
- [x] 1.2 Check `github:roc-lang/roc-overlay` for the newest dated nightly tag and choose the pin (`nightly-2026-08-07-8d23662`)
- [x] 1.3 Rewrite `flake.nix`: inputs = nixpkgs + roc-overlay (with `inputs.nixpkgs.follows`), devshell with the pinned roc nightly and `nixd`; no `formatter` output, no `nil`, no `alejandra`
- [x] 1.4 Regenerate `flake.lock` (`nix flake lock`), enter the devshell, and verify `roc version` reports the pinned nightly and `nixd` is on PATH
- [x] 1.5 Delete the `.zed/` directory

## 2. Syntax Calibration

- [x] 2.1 Migrate `package/Bit.roc` to the new syntax as currently understood (`|args|` lambdas, snake_case, `match`, updated builtins)
- [x] 2.2 Run `roc check` on `Bit.roc`, fix against real compiler errors, and record the confirmed syntax rules (including header/builtin changes) for the remaining files (recorded in design.md)
- [x] 2.3 Verify `package/main.roc` package header syntax under the new compiler and update if required (type modules + `import Cartridge/Header as Header`)

## 3. Bulk Migration

- [x] 3.1 Migrate `package/Constant.roc` and `package/Cpu/Register/Status.roc`; `roc check` passes (note: restored `Unchanged` in `Status.Delta` — required by Alu, lost in an old Flag→Status rename)
- [x] 3.2 Migrate `package/Cpu/Register.roc`; `roc check` passes (U16→U8 narrowing is `to_u8_wrap`)
- [x] 3.3 Migrate `package/Cpu/Alu.roc`; `roc check` passes (note: fixed pre-existing broken import `Cpu.Register.Flag`→`Register/Status`; Alu never compiled on legacy)
- [x] 3.4 Migrate `package/Cartridge/Header.roc` (399 lines); `roc check` passes via `package/main.roc` (direct file check trips a compiler quirk: the package-exposed path `Cartridge/Header` and the file's own logical name `Header` are reported as an import-source alias conflict)
- [x] 3.5 Migrate `package/Cpu/Instruction.roc` (609 lines); `roc check` passes (note: Condition/AddressingMode reference Status.Member/Register.Type8/Type16 structurally — nightly compiler bug: nested types can't be named through subdirectory imports; values still coerce into the nominal types)
- [x] 3.6 Migrate `examples/cartridge.roc`; `roc check` passes (platform switched to basic-cli 0.21.0, the new-compiler release; old Task API replaced by `main! : List(OsStr) => Try({}, _)` effects)

## 4. Verification

- [x] 4.1 Run `roc test` across all package modules; every pre-existing inline `expect` passes (33/33 via `roc test package/main.roc`; note: `roc test` on a subdirectory file resolves `/`-imports from the file's own dir, so testing routes through main.roc, which now imports all modules)
- [x] 4.2 Diff migrated code against `legacy` to confirm changes are syntax-only (opcode tables: 514 branches token-identical; Header tables: 267 pairs identical; documented deviations: Alu repair (broken import + `Unchanged` delta, never compiled on legacy), Alu's commented-out experiments dropped (preserved on legacy), structural type copies in Instruction (compiler bug workaround), type-module restructure, snake_case field renames)
- [x] 4.3 Review `README.md` for stale toolchain/installation references and update if needed (added nix develop / check / test section and legacy-branch pointer)
