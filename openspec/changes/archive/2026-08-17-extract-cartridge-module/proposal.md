# Extract Cartridge Module

## Why

`Mmu.roc` (1341 lines) plays five chips at once: the address/data bus, the RAMs,
the I/O peripherals, the CGB machinery — and the entire cartridge (MBC1/3/5
banking, cart RAM, the MBC3 RTC, battery signaling). The cartridge is the most
self-contained of these and already has a home (`Cartridge/Header.roc`), making
it the right first slice of the module-per-hardware-block reorganization that
sibling roc-ngba-emu already follows (`Bus` / `Cartridge` / `Timer` as separate
modules). Extracting it shrinks the bus module toward its actual hardware role
and sets the pattern for later slices (timer, joypad, APU sample ring).

## What Changes

- New `package/Cartridge.roc` nominal type owning all cartridge state currently
  inlined in `Mmu`: `rom`, `mbc`, `rom_bank`, `rom_bank_hi`, `bank2`, `mode`,
  `ram_enable`, `cart_ram`, `rtc` (with `RtcRegs`/`RtcState` and the
  `rtc_advance`/latch logic), `ram_written`, `save_events`.
- `Cartridge` exposes its own bus-facing API (read/write for the
  `0x0000–0x7FFF` ROM and `0xA000–0xBFFF` external RAM regions) plus the
  battery surface (`battery` bytes with RTC footer, `with_battery`,
  `battery_fit`, save-event signal).
- `Mmu` delegates those regions to `Cartridge` and drops the moved fields;
  `GameBoy`'s battery/`.sav` API forwards to the cartridge instead of reaching
  into `Mmu` fields.
- No behavior change: `.sav` format, MBC semantics, RTC catch-up, save-event
  edges, and all check-slice results stay byte-identical.

## Capabilities

### New Capabilities

None — this is a structural refactor; no new externally-observable behavior.

### Modified Capabilities

None — the requirements in `cartridge-banking`, `memory-bus`, and the battery
change remain exactly as specified; only which module implements them moves.
This change sets `skip_specs: true` in `.openspec.yaml` accordingly.

## Impact

- `package/Mmu.roc` — cartridge state and region handling removed, delegation
  added; unique-ownership discipline for `cart_ram` moves with the state.
- `package/Cartridge.roc` — new module (sits beside `Cartridge/Header.roc`).
- `package/GameBoy.roc` — `battery`, `battery_fit`, `with_battery`, and
  save-event access go through the cartridge.
- `package/main.roc` — import list gains `Cartridge`.
- Gates (must stay green, unchanged): `check/battery` (synthetic MBC/RTC/sav
  carts), `check/blargg` passlist (includes the MBC1 combined ROM),
  `check/single-step` (Flat layout untouched), `roc check` / `roc test`.
