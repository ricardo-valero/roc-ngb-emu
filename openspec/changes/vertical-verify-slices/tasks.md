# Tasks — vertical-verify-slices

## 1. Ride-along rename

- [x] 1.1 `app/ray/main.roc` → `app/ray.roc` (no folder for a single file); update README command; verify `roc build app/ray.roc --output=ray` builds

## 2. The blargg slice

- [x] 2.1 `nix/lib.nix`: `fetchRoms` helper generating the curl-with-cache script text (dir, base URL, rom list; space URL-encoding as today)
- [x] 2.2 `verify/blargg/main.roc`: move `example/blargg.roc` unchanged; `verify/blargg/package.nix`: writeShellApplication `verify-blargg` = scoped fetch (cpu_instrs list only) + per-ROM `roc run` loop with today's PASS/FAIL/12-of-12 output and exit semantics
- [x] 2.3 Repoint `nix/check-sound.nix` and `nix/run-ladder.nix` at `verify/blargg/main.roc`; delete `nix/run-blargg.nix`; remove the cpu_instrs section and the `rom/play.gb` auto-seed block from `nix/fetch-roms.nix`; README documents explicit `cp` into `rom/play.gb`

## 3. Flake discovery

- [x] 3.1 `flake.nix`: discover `verify/*/package.nix` via readDir → `packages.verify-<name>`; devshell includes discovered slices; drop the explicit run-blargg wiring; `git add` everything so the flake sees it

## 4. Verify no regression

- [x] 4.1 `nix run .#verify-blargg` reports 12/12 identically; `nix run .#check-sound`, `.#run-ladder`, `.#check-acid2` all pass unchanged
- [x] 4.2 `roc test package/main.roc` passes; both apps build (`app/ray.roc`, `app/web/main.roc`); README reflects the new commands and layout
