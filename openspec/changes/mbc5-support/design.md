# Design — mbc5-support

## Context

See proposal.md. `Mmu` models controllers as `mbc : [None, Mbc1, Mbc3]` with `rom_bank : U8` (raw register), `bank2` (secondary), `mode`, `ram_enable`; effective banks resolve in `switch_region_bank`/`zero_region_base`/`cart_ram_slot`, register writes in `write_mbc`.

## Goals / Non-Goals

**Goals:** MBC5 games boot and bank correctly; the existing controllers' behavior is untouched byte-for-byte.

**Non-Goals:** rumble (feature-flagged in headers, motor bit simply masked off with the RAM bank), battery persistence (own change), MBC2/6/7.

## Decisions

- **New `rom_bank_hi : U8` field** rather than widening `rom_bank` to U16: the existing controllers' register semantics (masking, zero-translation) read exactly as before, and MBC5's two ROM registers map one-to-one onto two fields. `bank2` keeps double-dutying as the RAM bank register, as it already does for MBC3.
- **No zero→one translation for MBC5** in `switch_region_bank` — the defining behavioral difference; `(hi << 8 | low) % bank_count`.
- **`cart_ram` grows to 128 KiB for all carts** (MBC5 max). MBC1 masks its RAM bank to 2 bits and MBC3 rejects >3, so the extra space is simply unused by them; one allocation size beats per-controller sizing.
- **Ride-along, `check/lib/` package**: `chk` exposes `Harness` (depends on the emulator package for `GameBoy`) and `Sha256` (pure); check apps add `chk: "../lib/main.roc"` (or `../../check/lib/main.roc`) alongside `ngb`. The emulator package exposes only `[Header, GameBoy]` again. This is the first package→package dependency on the nightly — if it fails, `Sha256` still moves (no deps needed) and `Harness` stays put with a note.

## Risks / Trade-offs

- [No MBC5 test ROM gates this] → inline expects cover the register semantics; the mooneye suite's MBC5 ROMs can join `check/mooneye`'s informative set later. Real-game acceptance: the user's previously-blank cart.
- [Package→package deps unproven] → explicit fallback above; either outcome is recorded.
