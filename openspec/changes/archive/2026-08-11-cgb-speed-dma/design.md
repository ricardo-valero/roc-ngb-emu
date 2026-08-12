# Design — cgb-speed-dma

## Context

Increment 3 of the CGB arc. Relevant seams: `GameBoy.finish` feeds one cycle count to `mmu.tick` (timers), `ppu.tick`, and `apu.tick`; `Stop` currently executes as a NOP that skips its padding byte; the PPU enters mode 0 at the dots-252 boundary in `Ppu.tick`; OAM DMA (0xFF46) already models zero-time bulk copies.

## Goals / Non-Goals

**Goals:** the two features games actually block on, with the plumbing arithmetic tested; every existing check byte-identical.

**Non-Goals:** cycle-accurate DMA stalling (copies are zero-time, like OAM DMA — games spin during transfers); STOP's non-switch behaviors (LCD off, joypad wake) beyond today's NOP treatment; double-speed serial.

## Decisions

- **`double_speed : Bool` on `Mmu`** beside the model; `Mmu.stop_switch` toggles it when CGB+armed (else identity), called unconditionally from the `Stop` arm. KEY1 readback becomes `speed<<7 | 0x7E | prepare`.
- **Plumbing in `finish`**: timers keep the full CPU cycle count (DIV/TIMA are CPU-clocked and genuinely run 2× in double speed); PPU and APU receive `cycles / 2` when double (all instruction cycle counts are multiples of 4, so halving is exact). `run_frame`'s 40000-step budget still covers a double-speed frame (~35k steps worst case).
- **DMA state as four `Mmu` fields** (`hdma_src`, `hdma_dst`, `hdma_blocks`, `hdma_active`); HDMA1–4 writes mask into the fields (source low nibble, destination to 0x1FF0 within VRAM); reads are 0xFF (write-only registers). Copies go byte-by-byte through `mmu.read`/`mmu.write` so source banking and the VBK destination bank fall out for free.
- **HBlank hook**: `Mmu.hdma_hblank` (16 bytes + countdown when active) called from `Ppu.tick`'s mode-0 entry — the one place HBlank begins for visible lines.
- **cgb-acid2 is the canary**: it uses neither feature before its stable frame, so its digest must not move; a change there is a regression, not a re-bless.

## Risks / Trade-offs

- [Zero-time DMA is inaccurate] → same accepted trade as OAM DMA; games poll HDMA5 or spin, both satisfied.
- [Double-speed budget margins in `run_frame`] → worst case ~35k of 40000 steps; noted, with headroom.
