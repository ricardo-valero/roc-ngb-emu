# Tasks — vertical-verify-slices (revised)

## 1. Ride-along rename (done in round one)

- [x] 1.1 `app/ray/main.roc` → `app/ray.roc`; README command updated; build verified

## 2. Harness extraction

- [x] 2.1 `package/Harness.roc`: move Blargg serial/memory-protocol and mooneye detection out of the runner as pure functions, exposed from `package/main.roc`, with inline expects; `roc test` passes

## 3. Slices (fetchurl + passlist pattern)

- [x] 3.1 Prefetch hashes; `check/blargg/{main.roc,roms.nix,passlist,package.nix}` — cpu_instrs + Blargg timing ROMs; 13 gate, mem_timing* informative
- [x] 3.2 `check/mooneye/{main.roc,package.nix,passlist}` — tarball fetchurl + extraction derivation; 6 gate
- [x] 3.3 `check/acid2/{main.roc,package.nix,golden.sha256}` — frame dumper moved from `example/frame.roc`; digest flow unchanged
- [x] 3.4 `check/sound/{main.roc,wav.roc,package.nix,passlist,golden.sha256}` — verdict runner + WAV renderer moved from `example/wav.roc`; 01-registers gates, singles informative, WAV digest unchanged

## 4. Flake, deletions, docs

- [x] 4.1 `flake.nix`: discovery over `check/` → `packages.check-<name>`; `dmg-acid2-rom` package; devshell drops all checks; delete `nix/{run-ladder,check-acid2,check-sound,fetch-roms,lib}.nix`, `golden/`, `verify/`
- [x] 4.2 README: verification section (four `check-*` commands, passlist/bless docs), tool paths, `rom/play.gb` zero-ROM path via `dmg-acid2-rom`

## 5. Verify no regression

- [x] 5.1 All four checks pass with the same gating sets as before the split; `roc test package/main.roc` passes; both apps build
