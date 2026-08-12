# CGB arc, increment 3: double speed and VRAM DMA

## Why

Increment 2 made CGB rendering correct, but real CGB games lean on two remaining hardware features to *feed* that renderer: the double-speed CPU mode (KEY1 + STOP) and the VRAM DMA engine (GDMA for bulk copies, HDMA for 16 bytes per HBlank). Without them, games that stream tiles per-frame — most of the commercial library — show broken or missing graphics despite the correct PPU.

## What Changes

- **KEY1 speed switching**: with the prepare bit armed, the STOP instruction toggles double speed and clears the bit; KEY1 bit 7 reports the current speed. In double speed the CPU and timers run 2× relative to the PPU/APU — plumbed by feeding video/audio ticks half the CPU cycles.
- **GDMA** (HDMA1–5, 0xFF51–0xFF55): writing HDMA5 with bit 7 clear performs an immediate copy of (length+1)×16 bytes from source (low 4 bits masked) to VRAM (destination masked into 0x8000–0x9FF0, honoring the current VBK bank), zero-time like the existing OAM DMA.
- **HDMA**: writing HDMA5 with bit 7 set arms a per-HBlank transfer of 16 bytes per visible scanline, counted down in HDMA5's readback (bit 7 clear while active, 0xFF when done); writing bit-7-clear while active cancels. Hooked at the PPU's mode-0 entry.
- Register semantics: HDMA1–4 are write-only (read 0xFF); everything is CGB-gated, DMG inert.

## Capabilities

### Modified Capabilities

- `cgb-core`: ADDED — double-speed mode; ADDED — VRAM DMA.

## Impact

- `package/Mmu.roc` (speed flag, DMA registers/engine, HBlank hook function), `package/GameBoy.roc` (STOP handler, cycle plumbing in `finish`), `package/Ppu.roc` (one hook call at mode-0 entry).
- Verification: expects for switching, plumbing arithmetic, GDMA (both VRAM banks), HDMA countdown/cancel, DMG inertness; all existing checks byte-identical (DMG untouched; CGB additions don't alter cgb-acid2, which uses neither feature before its stable frame — if its digest moves, that's a real bug to chase, not re-bless).
