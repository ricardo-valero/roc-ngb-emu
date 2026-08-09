# Design: MBC Banking

## Context

`Mmu` copies the first 32 KiB of ROM into the flat 64 KiB `mem` list and ignores writes below `0x8000`. Everything (CPU, PPU, DMA, examples) accesses memory exclusively through `Mmu.read`/`Mmu.write`, so banking is contained in one module. `Cartridge/Header.roc` already parses the MBC type but nothing consumes it yet.

## Goals / Non-Goals

**Goals:** MBC1 and MBC3 ROM banking correct enough for the target library (Mario Land, Zelda, Kirby, Pokémon Red); session-lifetime cartridge RAM; combined `cpu_instrs.gb` passing as the automated oracle; zero behavior change for ROM-only carts.

**Non-Goals:** battery persistence (deferred `rocray-file-io`), MBC3 RTC (latch writes accepted and ignored; RTC register reads return 0), MBC5/MBC2/multicart, cycle-accurate open-bus behavior.

## Decisions

### D1: ROM lives in its own field; `mem` keeps everything at `0x8000`+
`Mmu` gains `rom : List(U8)` (full image, no truncation) plus banking state; `mem` stays 64 KiB but its lower half is no longer consulted. Reads dispatch: `<0x4000` → `rom[addr]` (fixed bank 0), `<0x8000` → `rom[bank * 0x4000 + offset]`, `0xA000–0xBFFF` → cartridge RAM, else `mem` as today. Alternative — re-copying 16 KiB into `mem` on every bank switch — was rejected: games switch banks constantly (some per-frame), and the copy would dwarf the two extra comparisons on `read`.

### D2: Banking state is plain fields, controller type is a tag
`mbc : [None, Mbc1, Mbc3]` derived from header byte `0x0147` at init (direct byte map; the full `Header.read` record is more than needed). State: `rom_bank : U8` (raw register), `bank2 : U8` (MBC1 secondary / MBC3 RAM bank), `mode : Bool` (MBC1), `ram_enable : Bool`. Effective bank numbers are computed at read time and masked to the ROM's actual bank count (mask = banks−1; ROM sizes are powers of two), which also handles out-of-range writes the way hardware wiring does.

### D3: Register semantics
Writes `<0x8000` by range: `0x0000–0x1FFF` RAM enable (low nibble `== 0xA`); `0x2000–0x3FFF` ROM bank — MBC1 keeps 5 bits with 0→1 translation *before* combining with bank2, MBC3 keeps 7 bits with 0→1 translation; `0x4000–0x5FFF` bank2 (MBC1, 2 bits) / RAM bank (MBC3, values 0–3; RTC selects `0x08+` are stored but map to reads of 0); `0x6000–0x7FFF` MBC1 mode select / MBC3 latch (ignored). MBC1 effective ROM bank in the `0x4000` region = `bank2 << 5 | rom_bank5`; in mode 1 the `0x0000` region uses `bank2 << 5` and cartridge RAM banks by bank2 — mode 0 pins both to bank 0. ROM-only (`None`): all `<0x8000` writes ignored, exactly today's behavior.

### D4: Cartridge RAM is a fixed 32 KiB list, gated
Allocated unconditionally at init (largest DMG size; simpler than sizing from header byte `0x0149`, costs 32 KiB). Reads with RAM disabled return `0xFF`; writes with RAM disabled are dropped. Bank offset = `ram_bank * 0x2000` (MBC1 mode 1 uses bank2, else bank 0).

### D5: Oracle = combined `cpu_instrs.gb` appended to the Blargg suite
`fetch-roms` downloads `cpu_instrs/cpu_instrs.gb`; `run-blargg` runs it after the 11 individual ROMs (12 PASS lines total). It exercises MBC1 ROM banking under real code (the test harness itself bank-switches between subtests). MBC3 is covered by inline expects plus manual play of an MBC3 title.

## Risks / Trade-offs

- [Hot-path `read` gains comparisons] → Bounded: two range checks before the common `mem` fallthrough; verify Blargg suite wall-time doesn't move materially.
- [Fixed 32 KiB cart RAM hides header-size bugs a real cart would expose] → Accepted for session RAM; revisit when battery saves make RAM size externally visible.
- [MBC1 mode-1 edge cases (large-ROM zero-region banking) are rarely exercised by target games] → Implemented per Pan Docs but flagged; mooneye MBC test ROMs are the escalation path if a game misbehaves.

## Open Questions

- None blocking. RTC deferral revisited only if an MBC3 target needs more than "clock reads 0".
