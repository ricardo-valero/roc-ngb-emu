# Design: PPU Core (Headless)

## Context

The machine state is a pure `GameBoy` record; `step` returns elapsed cycles and everything cycle-driven (currently the timer) advances through `finish`. Memory is one flat `List(U8)` behind `Mmu`, so VRAM (`0x8000–0x9FFF`), OAM (`0xFE00–0xFE9F`), and the LCD registers (`0xFF40–0xFF4B`) already exist as storage — nothing reads them. LY is stubbed to `0x90` at init so boot-era ROMs poll successfully; that stub becomes wrong the moment real LY progression exists. Verified baselines that must not break: 129 inline expects, Blargg `cpu_instrs` 11/11.

## Goals / Non-Goals

**Goals:**
- Scanline-accurate DMG rendering good enough for dmg-acid2: BG + window + sprites with correct palettes, scrolling, flips, size modes, and priority rules.
- Real PPU timing at machine-cycle granularity: mode sequence, LY progression, VBlank/STAT interrupts, LY/LYC.
- Headless verification (PPM dump + dmg-acid2 reference digest) runnable in CI, plus a frame-stepping API the play-app change will consume unchanged.

**Non-Goals:**
- Pixel-FIFO / dot-level accuracy, mid-scanline register changes (SCX writes during mode 3 etc.), OAM/VRAM access blocking during modes 2/3, the OAM corruption bug.
- STAT IRQ-line blocking subtleties (one shared edge for all sources) — simplified, see D6.
- roc-ray, joypad, audio, GBC.

## Decisions

### D1: Scanline renderer, not pixel FIFO
Render a whole scanline at once when the line's drawing phase begins. dmg-acid2 is explicitly designed to be passable by scanline renderers (it avoids mid-scanline effects), and every later milestone (Tetris, Pokémon-class games) is too. FIFO accuracy would multiply complexity for zero observable gain against our oracles. Revisit only if a target game visibly glitches.

### D2: PPU state is a nominal `Ppu` record inside `GameBoy`; registers stay in `Mmu`
`Ppu` holds what the hardware hides from the bus: dot counter within the line, framebuffer, window internal line counter. Everything the game can address (LCDC, STAT, SCY/SCX, LY, LYC, BGP, OBP0/1, WY/WX) lives in `mem` behind `Mmu` like the timer registers do — one source of truth, and the PPU reads VRAM/OAM through `Mmu.read` untouched. LY and STAT's mode/coincidence bits are written by the PPU via `poke`.

### D3: Ticked from `finish`, same contract as the timer
`Ppu.tick : Ppu, Mmu, U64 -> { ppu : Ppu, mmu : Mmu }` advances the dot counter by elapsed cycles, walks mode transitions (456 dots/line, mode 2 = 80, mode 3 = 172 fixed, remainder HBlank; lines 144–153 VBlank), renders a scanline on entry to mode 3, and raises IF bits (VBlank bit 0, STAT bit 1) through `Mmu.request_interrupt`. Chunked cycle deltas (up to ~24 per instruction) may cross a mode boundary; the tick loop consumes dots mode-by-mode so transitions are never skipped.

### D4: Framebuffer = `List(U8)` of post-palette shades (0–3), row-major 160×144
Palette application happens at render time (BGP/OBP as of that scanline), so the consumer just tone-maps 0–3 → RGB. In-place mutation applies as long as the framebuffer list is only touched through the render path. `GameBoy` exposes `framebuffer : GameBoy -> List(U8)` and `run_frame : GameBoy -> GameBoy` (step until the next VBlank entry, bounded by a step budget) — the exact surface the play app needs later.

### D5: OAM DMA is an instant copy
Writing `0xFF46` copies `value<<8 … value<<8 + 0x9F` into OAM in zero emulated time. Real DMA takes 160 machine cycles during which the CPU can only see HRAM — games already respect this by spinning in an HRAM routine, so instant copy is invisible to them. Cycle-accurate DMA is FIFO-era accuracy we're explicitly not buying.

### D6: STAT interrupts: per-source rising edge, no shared-line blocking
Fire the STAT interrupt when a newly-entered state matches an enabled source (mode 0/1/2 enable bits, LYC coincidence bit). Real hardware ORs all sources into one line and only interrupts on the line's rising edge ("STAT blocking"). The simplification can double-fire where hardware wouldn't; neither dmg-acid2 nor our target games depend on the difference. Flagged as the known gap to revisit if an oracle demands it.

### D7: Window quirks that ARE in scope (dmg-acid2 tests them)
The window keeps its own line counter that only increments on lines where the window actually rendered (WY latched at frame start conceptually; WX=166/WY tricks appear in dmg-acid2). 8×16 sprites mask bit 0 of the tile index. Sprite priority: lower X wins, ties broken by OAM order; at most 10 sprites per scanline selected in OAM order during mode-2 scan.

### D8: Verification pipeline = PPM dump + frozen digest
`example/frame.roc` runs a ROM for N frames (default ~5 seconds of emulated time or an explicit count) and writes a binary PPM (P6, 0–3 mapped to 4 grays) — no encoder dependency. dmg-acid2's ROM is fetched (MIT-licensed, from mattcurrie/dmg-acid2 releases) alongside the Blargg ROMs. Acceptance: rendered output visually matches the published reference image once; the framebuffer digest is then frozen into the suite script as the regression oracle. Commercial ROMs (Tetris) stay manual: user-supplied path, eyeball check.

### D9: LY stub removal is the regression tripwire
Removing `LY = 0x90` from `Mmu.init` means any ROM that polls LY now depends on real PPU progression. Blargg's `cpu_instrs` must stay 11/11 after the swap — run the suite at the timing-skeleton milestone (before any rendering exists), not just at the end.

## Risks / Trade-offs

- [Chunked ticks smear mode-boundary timing by up to one instruction] → Mode durations are consumed dot-by-dot from a carry-over counter; boundaries can lag real hardware by ≤24 dots, which scanline-level oracles cannot observe.
- [STAT edge simplification (D6) double-fires where hardware wouldn't] → Accepted; documented; revisit against a STAT-specific test ROM (e.g. mooneye) only if a real game misbehaves.
- [dmg-acid2 digest freezes accidental quirks along with correct behavior] → Digest is only frozen after a human confirms the render against the published reference image; the PPM stays reproducible for re-inspection.
- [Frame stepping API guessed wrong for the play app] → Surface is minimal (`run_frame` + `framebuffer`); the spike already showed the roc-ray side, and both live in our codebase to adjust cheaply.

## Open Questions

- Post-boot STAT/LY initial phase (which dot of which line does hardware resume at?): start at line 0, dot 0, mode 2 — revisit if dmg-acid2's first frames misalign.
- Whether `run_frame`'s step budget needs to account for HALT-heavy idling (HALT steps report 4 cycles, so a frame is ≤ ~17.6k steps — likely fine).
