# Tasks — mbc5-support

## 1. Core

- [x] 1.1 `Mmu`: `Mbc5` in the controller union + selection (0x19–0x1E), `rom_bank_hi` field, `write_mbc`/`switch_region_bank`/`cart_ram_slot` arms, 128 KiB `cart_ram`
- [x] 1.2 Inline expects: 9-bit selection, bank-0 selectable, high-bit wiring via wrap, RAM bank independence; `roc test` green

## 2. Check-lib ride-along

- [x] 2.1 `check/lib/main.roc` package exposing `Harness` + `Sha256` (dep on the emulator package); emulator package back to `[Header, GameBoy]`; check apps repointed; all four checks green

## 3. Wrap

- [x] 3.1 README status line; full regression (checks + package tests + both app builds)
