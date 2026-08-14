# Proposal: battery-saves-rtc

## Why

Nothing survives closing the window: you can play Pokémon Crystal but not
keep your save, which is the line between "playable" and feature complete.
The fork's `write_bytes!` (change `fork-rocray-file-io`) removed the last
platform blocker, so the remaining work is core-side: expose cart RAM as
`.sav` bytes, model the MBC3 RTC, and give each frontend a load/write-back
path.

## What Changes

- The core gains a battery surface: `battery : GameBoy -> List(U8)`
  (cart RAM masked to the header-declared size, plus the 48-byte RTC
  footer for RTC carts) and `with_battery : GameBoy, List(U8) -> GameBoy`
  (accepts 44- and 48-byte footers, and bare RAM with no footer).
- MBC3 grows its RTC: epoch-based registers (base timestamp + latched
  copies), the 0x6000 latch protocol, halt flag and day-carry. Wall-clock
  time enters the pure core as data — the per-frame input record widens
  from bare buttons to `{ buttons, now : U64 }` — so reads never perform
  an effect. On load, elapsed time since the footer's timestamp advances
  the clock. **BREAKING** for direct `run_frame`/`run_until` callers:
  the input record shape changes.
- The ray app loads `<rom>.sav` at startup if present and writes it back
  on exit and on the `ram_enable` falling edge (the "game just saved"
  signal); the web app does the same against IndexedDB, plus a
  visibility-change flush.
- Out of scope: BESS save states (follow-up change `bess-save-states` per
  WISHLIST), real-time RTC drift correction beyond epoch catch-up,
  MBC30 (>4 RAM banks).

## Capabilities

### New Capabilities

- `battery-persistence`: the core's battery surface — `.sav`-compatible
  extraction/injection of cart RAM and RTC state, footer format
  compatibility (44/48-byte, always write 48).

### Modified Capabilities

- `cartridge-banking`: MBC3 gains RTC register mapping (0x08-0x0C RAM-bank
  selects clock registers), the latch protocol, and halt semantics.
- `core-debug`: the `run_until`/`run_frame` entry points take a widened
  input record `{ buttons, now }` (breaking API change at the package
  surface).
- `play-app`: save loading at startup and write-back triggers.
- `web-app`: IndexedDB persistence with the same trigger semantics.

## Impact

- `package/Mmu.roc` (cart RAM masking, MBC3 RTC registers, `ram_enable`
  edge exposure), `package/GameBoy.roc` (battery surface, input record),
  `package/Cartridge/Header.roc` (RAM size accessor if missing).
- `app/ray.roc` and `app/web/main.roc` (+ roc-web JS shim for IndexedDB);
  ray needs a host clock reading — verify the fork exposes one, else it
  is a small fork addition following the `args!` recipe.
- `check/`: a battery round-trip check (save, reload, bytes identical)
  and an RTC advance check with a fixed `now` — pure Roc like the rest.
