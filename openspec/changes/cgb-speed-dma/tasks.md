# Tasks — cgb-speed-dma

## 1. Double speed

- [x] 1.1 `Mmu`: `double_speed` field, `stop_switch`, KEY1 readback with bit 7; `GameBoy`: STOP calls `stop_switch`, `finish` halves PPU/APU cycles when double; expects (switch/round-trip, KEY1 readback, LY advances half rate)

## 2. VRAM DMA

- [x] 2.1 `Mmu`: HDMA1–5 registers (write-only 1–4), GDMA immediate copy honoring VBK, HDMA arm/countdown/cancel + `hdma_hblank`; `Ppu.tick` hook at mode-0 entry; expects (GDMA both banks, HDMA countdown via PPU ticks, cancel, DMG inert)

## 3. Regression

- [x] 3.1 All checks byte-identical (cgb-acid2 the canary); full test suite; both apps build
