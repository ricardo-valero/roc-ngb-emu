# Play App (roc-ray)

## Why

The emulator is complete through the PPU (Blargg 11/11, dmg-acid2 pixel-perfect) but only runs headless. The play app is the payoff milestone: a window showing a running ROM at 60fps with keyboard input — and the last missing hardware piece, the joypad register, is what makes games interactive at all.

## What Changes

- Joypad support in the core: the bus holds current button state; P1/JOYP (`0xFF00`) implements hardware semantics — the game writes the select bits (4–5), reads get the selected group's buttons in the low nibble, active-low. Replaces the static `0xCF` stub.
- `GameBoy.run_frame` takes a `Buttons` record (8 bools); the headless frame example passes the no-input default.
- New `example/play.roc` roc-ray app: embeds `rom/play.gb` at build time (the roc-ray 0.9.0 host has no binary file read), runs one emulator frame per render tick, presents the framebuffer via a mutable-pixel texture scaled 4×, and maps keyboard → `Buttons` (arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select, Esc quits).
- `fetch-roms` seeds `rom/play.gb` with dmg-acid2 when absent so the play app always builds; users drop any game ROM into that path.
- Out of scope: joypad interrupt (IF bit 4 — virtually all games poll), audio, save states, turbo/pause UI.

## Capabilities

### New Capabilities

- `play-app`: interactive play — windowed presentation of the emulator framebuffer at frame pacing, with keyboard input mapped to the joypad.

### Modified Capabilities

- `memory-bus`: adds the joypad register requirement — P1/JOYP select-bit writes and active-low group reads backed by externally-set button state.

## Impact

- **Code**: `package/Mmu.roc` (buttons state + P1 semantics), `package/GameBoy.roc` (`Buttons` through `run_frame`), `example/frame.roc` (no-input default), new `example/play.roc`, `nix/fetch-roms.nix` (seed `rom/play.gb`).
- **Tests**: P1 register expects (group select, active-low, both-groups, no-select); existing suites must stay green (Blargg 11/11, check-acid2 PASS).
- **Dependencies**: roc-ray 0.9.0 platform (already spike-verified against the pinned nightly); used only by `example/play.roc`, never by the package.
