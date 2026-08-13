## 1. Fetch apps

- [x] 1.1 Port the sibling's fetch.roc shape to `check/blargg/fetch.roc`: ROM list moved from `check/blargg/package.nix` (cpu_instrs singles + combined, instr_timing, mem_timing, dmg_sound singles) with per-file SHA-256, downloading from retrio/gb-test-roms at the commit `gb-test-roms.nix` pins, verifying via `chk.Sha256`, into `check/blargg/data/`; verify one file end-to-end before porting the full list
- [x] 1.2 `check/mooneye/fetch.roc`: pinned `.tar.gz` from gekkio.fi, archive SHA-256 verified, unpacked with new pure-Roc `check/lib/Inflate.roc` (DEFLATE/gzip, puff port) + `check/lib/Tar.roc`, each extracted ROM hash-verified into `data/`
- [x] 1.3 `check/acid2/fetch.roc` (dmg-acid2 + cgb-acid2 release assets, manual redirect-follow) and `check/sound/fetch.roc` (dmg_sound 01-registers), same verify-into-`data/` shape; every data/ dir untracked via local `*` .gitignore

## 2. Runner defaults

- [x] 2.1 Blargg and mooneye suite runners: default ROM set to `check/<name>/data/` when invoked without arguments (passlist reading unchanged); explicit paths still accepted
- [x] 2.2 `check/acid2/main.roc` and `check/sound/main.roc`: default their ROM arguments to the fetched `data/` paths

## 3. Retire nix

- [x] 3.1 Delete `check/blargg/package.nix`, `check/mooneye/package.nix`, `check/acid2/package.nix`, `check/sound/package.nix`, `check/gb-test-roms.nix`; drop the `check-*` packages from `flake.nix` (devshell stays)
- [x] 3.2 README: replace `nix run .#check-*` instructions with `roc check/<name>/fetch.roc` (once) + `roc check/<name>/main.roc`; sweep WISHLIST/openspec project docs for stale harness references

## 4. Verify

- [x] 4.1 From a clean state (no `data/` dirs): fetch all four slices, then run all four checks green from the devshell; re-run fetch to confirm idempotent skip; corrupt one ROM byte to confirm the hash check fails it
