# PPU Core (Headless)

## Why

The CPU core is complete and Blargg-verified, but the emulator produces no picture: LY is a static `0x90` stub so ROMs don't hang, and VRAM/OAM are inert bytes. The PPU is the gate to every visible milestone — the Tetris title screen, the roc-ray play app (planned follow-up), and any game actually running. Doing it headless first (framebuffer → image dump) keeps the same discipline that worked for the CPU: a binary oracle, no platform dependencies, CI-friendly.

## What Changes

- New scanline-based PPU producing a 160×144 framebuffer: background and window layers (both tile-data addressing modes, SCX/SCY scrolling, WX/WY placement with the internal window line counter), and OAM sprites (8×8 and 8×16, X/Y flip, OBP0/OBP1 palettes, 10-sprites-per-scanline limit, DMG X-coordinate priority, BG-over-OBJ attribute).
- PPU mode state machine (OAM scan → drawing → HBlank; VBlank on lines 144–153) advanced by the cycles `GameBoy.step` already reports — same pattern as the timer. LY progresses for real; the `LY = 0x90` stub is removed. LY/LYC coincidence, STAT mode/LYC interrupt sources, and the VBlank interrupt are raised through the existing IF mechanism.
- OAM DMA (`0xFF46`): writing the register copies `value << 8 .. +0x9F` into OAM — games (and dmg-acid2) load sprite data exclusively this way.
- Framebuffer exposed from `GameBoy` as post-palette 2-bit shades, plus a frame-stepping entry point (`run_frame`-style) that the play app will reuse.
- New headless CLI example: run a ROM for N frames, dump the framebuffer as a PPM image; a fetch script pulls the freely-licensed dmg-acid2 test ROM.
- Verification: dmg-acid2 rendered output matches its published reference image (frozen as a digest once verified); Blargg `cpu_instrs` stays 11/11 (LY-stub removal and new interrupts must not regress CPU/timer behavior).
- Out of scope: roc-ray integration, joypad input (both land in the follow-up play-app change), pixel-FIFO timing accuracy, mid-scanline register effects, Game Boy Color.

## Capabilities

### New Capabilities

- `ppu-core`: DMG picture generation — PPU timing/mode state machine with its interrupts, background/window/sprite scanline rendering, and framebuffer exposure.
- `ppu-verification`: headless frame capture — dumping rendered frames as images and holding rendering to the dmg-acid2 reference oracle without regressing Blargg results.

### Modified Capabilities

- `memory-bus`: adds the OAM DMA transfer requirement (`0xFF46` write triggers the OAM copy); LCD/PPU IO registers become live state read by the PPU rather than inert bytes.

## Impact

- **Code**: new `package/Ppu.roc`; `package/Mmu.roc` gains OAM DMA and drops the LY init stub; `package/GameBoy.roc` ticks the PPU alongside the timer and exposes the framebuffer + frame stepping; new `example/frame.roc`; `nix/fetch-roms.nix` extended with dmg-acid2.
- **Tests**: inline expects for tile decoding, palettes, scanline composition, sprite priority, and mode timing; dmg-acid2 digest becomes the rendering regression oracle; existing 129 expects and the 11-ROM Blargg suite must stay green.
- **Dependencies**: none new (PPM needs no encoder; dmg-acid2 fetched like the Blargg ROMs, untracked).
- **Risk**: STAT interrupt edge cases (IRQ blocking) are simplified in this change; noted in design, revisited only if an oracle demands it.
