## 1. Joypad in the bus

- [x] 1.1 Add `Buttons` record and a `buttons` field to `Mmu` with `set_buttons`; implement P1 write (select bits only) and read (active-low selected group) per D3, replacing the static stub; expects for action group, d-pad group, both, and none
- [x] 1.2 Thread `Buttons` through `GameBoy.run_frame` (with a `no_buttons` default) and update `example/frame.roc`; existing suites stay green

## 2. Play app

- [x] 2.1 Seed `rom/play.gb` from dmg-acid2 in `nix/fetch-roms.nix` when absent
- [x] 2.2 `example/play.roc`: roc-ray app embedding `rom/play.gb` (D1) — texture presentation scaled 4x with Point filter, DMG green palette, key mapping (arrows/X/Z/Enter/Backspace, Esc exits), one `run_frame` per tick (D4)
- [x] 2.3 Build and run: dmg-acid2 face visible in the window; document swapping in a game ROM

## 3. Wrap-up

- [x] 3.1 `roc check` all examples, `roc test` package, Blargg 11/11, check-acid2 PASS; README updated with play instructions
- [x] 3.2 Verify delta-spec scenarios covered; ready for archive
