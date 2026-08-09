## 1. Bus restructure (behavior-neutral)

- [x] 1.1 `Mmu` gains `rom` (full image, no truncation), `cart_ram` (32 KiB), and banking state (`mbc` tag from header byte `0x0147`, `rom_bank`, `bank2`, `mode`, `ram_enable`) per D1/D2; read dispatch: bank 0 region, banked region, cartridge RAM, else `mem`
- [x] 1.2 ROM-only carts behave identically: all 156 expects, Blargg 11/11, check-acid2 PASS before any MBC register exists

## 2. MBC1

- [x] 2.1 Register writes per D3: RAM enable, 5-bit ROM bank with 0→1 translation, 2-bit bank2, mode select; effective-bank computation with bank-count masking
- [x] 2.2 Expects: bank switch changes the `0x4000` window, bank-0 translation, bank2 extension (bank 0x21), mode-1 zero-region banking

## 3. MBC3

- [x] 3.1 Register writes: 7-bit ROM bank with 0→1 translation, RAM bank 0–3 (RTC selects stored, reads 0), latch ignored
- [x] 3.2 Expects: 7-bit bank selection, bank-0 translation, RAM bank round-trip

## 4. Cartridge RAM

- [x] 4.1 `0xA000`–`0xBFFF` reads/writes through `cart_ram` with enable gating (disabled: reads `0xFF`, writes dropped) and per-controller banking per D4, with expects

## 5. Verification

- [x] 5.1 `fetch-roms` downloads the combined `cpu_instrs.gb`; `run-blargg` runs it as the 12th entry
- [x] 5.2 Combined ROM passes over serial; iterate on MBC1 behavior if it reports failures
- [x] 5.3 If a game ROM is available locally: play an MBC1 title (Mario Land/Zelda) and an MBC3 title (Pokémon Red) via `rom/play.gb` as a manual smoke test

## 6. Wrap-up

- [x] 6.1 All suites green (12-ROM Blargg, check-acid2, package expects); README status updated (banked cartridges supported, no battery saves yet)
- [x] 6.2 Delta-spec scenarios verified; ready for archive
