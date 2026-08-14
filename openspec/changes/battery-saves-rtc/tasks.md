# Tasks: battery-saves-rtc

## 1. Core: input widening and battery surface

- [x] 1.1 Widen `run_frame`/`run_until` input to `{ buttons, now : U64 }`;
  update `no_buttons` (→ `no_input`-style helper with `now = 0`), both
  apps' call sites, `check/lib`, and examples; all existing checks pass
  unchanged
- [x] 1.2 Add a header RAM-size accessor and a battery-backed predicate
  (cartridge-type byte) in `package/Cartridge/Header.roc` if missing
- [x] 1.3 Implement `battery : GameBoy -> List(U8)` (cart RAM masked to
  header size; empty for non-battery carts) and
  `with_battery : GameBoy, List(U8) -> GameBoy` (pad/truncate, never
  fails) with inline tests

## 2. Core: MBC3 RTC

- [x] 2.1 Add RTC state to `Mmu` (`regs`, `latched`, `last_now`, halt +
  day-carry semantics) with lazy advance at observation points
- [x] 2.2 Map clock registers via `0x4000`-region selects `0x08`–`0x0C`
  (reads return latched values, writes set the running clock), implement
  the `0x00`→`0x01` latch at `0x6000`–`0x7FFF`, gate on RAM enable;
  RAM banking undisturbed
- [x] 2.3 Extend `battery`/`with_battery` with the RTC footer: write the
  48-byte form, accept 48/44/bare; catch-up on load from the footer
  timestamp (halted clock stays put)
- [x] 2.4 Increment the save-event counter on the RAM-disable-after-write
  edge and expose it on `GameBoy`

## 3. Checks

- [x] 3.1 Battery round-trip check under `check/`: write RAM, extract,
  inject into a fresh machine, extract again — byte-identical; include a
  44-byte-footer fixture that loads and re-saves as 48
- [x] 3.2 RTC check with fixed `now` values: latch protocol, advance math
  (61 s → +1 min +1 s), halt freeze, day carry; deterministic (no wall
  clock in checks)

## 4. Ray app

- [x] 4.1 Fork: add UNIX epoch seconds to the host snapshot next to
  `timestamp_nanos` (Zig host + `Host.roc`/`HostHost.roc` plumbing, the
  `args!` recipe); rebuild and repoint the app
- [x] 4.2 Load `<rom-path>.sav` at startup when present (tolerate
  missing/short), pass `now` each frame, flush via `write_bytes!` on
  save-counter change and on exit; skip entirely for non-battery carts
- [ ] 4.3 Verify: save in Pokémon Crystal, quit, relaunch — the load
  screen offers the save; `.sav` also loads in another emulator (format
  compatibility spot-check)

## 5. Web app

- [x] 5.1 Vendored roc-web lib: pass `Date.now()/1000` into the host
  record; add an IndexedDB save store keyed by header title + global
  checksum, with a `visibilitychange` flush hook
- [x] 5.2 `app/web/main.roc`: restore battery bytes when a ROM loads
  (default fetch and drop alike), pass `now`, push battery bytes to the
  store on save-counter change
- [ ] 5.3 Verify: save, reload the page — progress persists; two
  different ROMs keep distinct saves; tab-hide flushes an undisabled-RAM
  save

## 6. Docs and closeout

- [x] 6.1 README: battery saves and RTC in the status paragraph, `.sav`
  siting/format note in the play sections; WISHLIST: move the two items
  to the done ledger
- [x] 6.2 `openspec validate --strict` passes; run the full check suite
