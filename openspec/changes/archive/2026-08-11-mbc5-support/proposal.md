# MBC5 cartridge support

## Why

Dropping a late-era or GBC-era `.gb` on the web app (or embedding one natively) yields a permanent white screen: most such carts use MBC5, which the core silently runs as ROM-only (2026-08-11 diagnosis of the "reload shows blank" report). MBC5 is the most common mapper after MBC1 and mechanically simpler — no mode register, no zero→one bank translation.

## What Changes

- `Mmu` gains an `Mbc5` controller (header types 0x19–0x1E): 9-bit ROM banking (low byte at 0x2000–0x2FFF, bit 8 at 0x3000–0x3FFF, **bank 0 selectable** — unlike MBC1/MBC3 there is no zero→one translation), 4-bit RAM banking at 0x4000–0x5FFF, RAM enable at 0x0000–0x1FFF, writes at 0x6000+ ignored.
- Cartridge RAM backing grows from 32 KiB to 128 KiB (MBC5's max, 16 banks); MBC1/MBC3 masking already confines them to the low banks.
- Inline expects: 9-bit bank selection incl. the high bit, bank-0 selectability, RAM bank switching, bank-count wrapping.
- README status line: MBC5 joins MBC1/MBC3.
- **Ride-along (user review): `Harness` and `Sha256` move out of the emulator package** into a new `check/lib/` package (`chk`) that depends on the emulator package — they are check infrastructure, not Game Boy domain API. The emulator package returns to exposing exactly `[Header, GameBoy]`. Falls back to status quo if package→package deps fight the nightly (documented either way).

## Capabilities

### Modified Capabilities

- `cartridge-banking`: ADDED requirement — MBC5 banking semantics.

## Impact

- `package/Mmu.roc` (controller, new `rom_bank_hi` field, expects), `package/Cartridge/Header.roc` (already maps MBC5 — no change), `package/main.roc` (exposes), `check/lib/` (new package), `check/run.roc` + `check/acid2/main.roc` + `check/sound/main.roc` (import updates), README.
- Verification: all four checks unchanged-green; 200+ package tests plus the new MBC5 expects; a real MBC5 game booting in the web app is the acceptance test the bug report implies.
