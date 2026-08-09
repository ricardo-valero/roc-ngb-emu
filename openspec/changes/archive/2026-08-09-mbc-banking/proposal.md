# MBC Banking (MBC1 + MBC3)

## Why

Only 32 KiB ROM-only cartridges run today — `Mmu.init` truncates larger images and writes below `0x8000` are ignored. That covers Tetris and test ROMs; nearly every notable DMG game is banked: Super Mario Land, Zelda: Link's Awakening, and Kirby's Dream Land are MBC1, Pokémon Red is MBC3 with cartridge RAM. MBC support is the gap between a verified core and an emulator that plays the library — and it comes with a free oracle: Blargg's *combined* `cpu_instrs.gb` is a 64 KiB MBC1 ROM (the reason we've been running the 11 individual ROMs until now).

## What Changes

- The bus keeps the full ROM image and maps it banked: `0x0000–0x3FFF` fixed bank 0, `0x4000–0x7FFF` the switchable bank, selected by MBC registers written to the (previously ignored) `<0x8000` range.
- MBC1: 5-bit ROM bank register with 0→1 translation, 2-bit secondary register, banking-mode select; MBC3: 7-bit ROM bank with 0→1 translation, RAM bank select 0–3.
- Cartridge RAM at `0xA000–0xBFFF`: gated by the enable register (`0x0A`), banked, held in memory for the session. **Battery persistence is out of scope** (needs binary file writes — the trigger recorded in the deferred `rocray-file-io` change).
- The MBC type is read from the cartridge header (`0x0147`); ROM-only carts behave exactly as today.
- Verification: the combined `cpu_instrs.gb` (MBC1) joins the Blargg suite and must pass over serial; the 11 individual ROMs, check-acid2, and all inline expects stay green.
- Out of scope: MBC3 real-time clock (Pokémon runs; day/night events don't advance), MBC5, MBC1 multicart (>1 MiB) wiring, battery saves.

## Capabilities

### New Capabilities

- `cartridge-banking`: MBC1/MBC3 ROM bank mapping, cartridge RAM with enable gating and banking, header-driven controller selection.

### Modified Capabilities

- `memory-bus`: the ROM region requirement changes from "read-only, writes ignored, ≤32 KiB" to banked mapping with register writes; ROM loading loses the 32 KiB limit.
- `cpu-verification`: adds the combined `cpu_instrs.gb` (MBC1) to the suite.

## Impact

- **Code**: `package/Mmu.roc` (ROM retained as its own field, banking state, read dispatch for `<0x8000` and `0xA000–0xBFFF`, register writes); `nix/fetch-roms.nix` + `nix/run-blargg.nix` (combined ROM). CPU/PPU/GameBoy untouched — everything goes through `Mmu.read`/`write`.
- **Tests**: bank-translation and RAM-gating expects; combined-ROM suite run; no regressions tolerated in the existing 156 expects, Blargg 11/11, or the acid2 digest.
- **Performance note**: `read` gains region comparisons on the hot path; Blargg wall-times will show whether it matters (it should not).
