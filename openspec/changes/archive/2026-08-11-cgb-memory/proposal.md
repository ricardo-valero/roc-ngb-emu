# CGB arc, increment 1: memory infrastructure

## Why

The Game Boy Color arc (explored 2026-08-11) starts with the plumbing every later increment stands on: mode detection, banked VRAM/WRAM, palette RAM, and the CGB register file. This increment is deliberately invisible — no rendering change, no speed change — so it can land with every existing DMG check byte-identical, before the risky work (color PPU, priority, double speed) begins on top of it.

## What Changes

- **CGB mode detection**: `Mmu`/`GameBoy` gain a mode flag derived from the already-parsed header `cgb_flag`; in CGB mode the CPU boots with `A = 0x11` (how games detect the console — one byte that decides whether Pokémon Crystal shows color). DMG carts keep `A = 0x01` and today's behavior exactly.
- **Banked VRAM**: a second 8 KiB bank selected by VBK (0xFF4F, bit 0; reads back as `0xFE | bank`); bank-0 storage remains the flat `mem` region. The PPU keeps reading via today's path this increment (CGB-mode rendering is knowingly wrong until increment 2; a bank-explicit accessor lands now for it to adopt).
- **Banked WRAM**: 0xD000–0xDFFF switched by SVBK (0xFF70, bits 0–2, 0 selects 1); bank 1 remains the flat `mem` region, banks 2–7 in new side storage.
- **Palette RAM**: 64 bytes BG + 64 bytes OBJ behind the index/data port pairs BCPS/BCPD (0xFF68/69) and OCPS/OCPD (0xFF6A/6B) with bit-7 auto-increment on data writes.
- **Stubs recorded, not faked**: KEY1 (0xFF4D) stores the prepare bit and reads back at normal speed (switching is increment 3); OPRI (0xFF6C) stores its bit (used in increment 2).
- **DMG mode inertness**: every register above is inert on DMG carts (reads 0xFF, writes dropped), as on hardware.

## Capabilities

### New Capabilities

- `cgb-core`: Game Boy Color machine behaviors — console detection, banked VRAM/WRAM, palette RAM ports, and the CGB register file (grown by later increments: color rendering, double speed, HDMA).

## Impact

- `package/Mmu.roc` (mode flag, bank storage, dispatch, ports, expects), `package/GameBoy.roc` (`A = 0x11` at init in CGB mode).
- Verification: new inline expects for every behavior above; all four checks and 213 package tests must pass **unchanged** — the increment's defining property.
- Sequenced next (separate changes): `cgb-ppu` (RGB555 framebuffer — decision B from the exploration — attributes, palettes, priority, cgb-acid2 oracle), `cgb-speed-dma`, `cgb-audio-and-polish`.
