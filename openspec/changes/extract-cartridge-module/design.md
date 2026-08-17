# Design — Extract Cartridge Module

## Context

See proposal.md — Why. Current state that shapes the approach:

- All cartridge state lives as flat fields on the `Mmu` record
  (`Mmu.roc:54-95`): `rom`, `cart_ram`, `mbc`, `rom_bank`, `rom_bank_hi`,
  `bank2`, `mode`, `ram_enable`, `rtc`, `ram_written`, `save_events`, plus the
  free-standing `RtcRegs`/`RtcState` types and `rtc_init`/`rtc_advance`
  (`Mmu.roc:11-52`).
- `Mmu.read`/`Mmu.write` are the only functions touching `mem`, keeping the
  list uniquely owned for in-place mutation (`Mmu.roc:1-2`). The same
  discipline implicitly covers `cart_ram` and `rom`.
- `GameBoy.battery` / `battery_fit` / `with_battery` reach directly into
  `gb.mmu.cart_ram`, `gb.mmu.rom`, and `gb.mmu.battery_footer()`
  (`GameBoy.roc:79-107`).
- The nightly compiler cannot reference nested nominal types through
  **subdirectory** imports (verified on `nightly-2026-08-15`; see the
  workaround comment at `Cpu/Instruction.roc:7`). Root-level modules are fine.
- Verified on the pinned nightly: a root module `X.roc` can coexist with a
  directory `X/` and can itself `import /X/Sub`.

## Goals / Non-Goals

**Goals:**

- One nominal `Cartridge` type owning everything on the cartridge PCB: ROM,
  MBC banking registers, external RAM, RTC, battery signaling.
- `Mmu` shrinks to bus + RAM + I/O concerns; cartridge regions delegate.
- Byte-identical behavior, gated by the existing check slices.

**Non-Goals:**

- No timer/joypad/serial extraction (later slices of the same reorganization).
- No new cartridge features (no MBC2, no rumble, no save states).
- No change to the `.sav` format, header parsing (`Cartridge/Header.roc`
  stays as-is), or the check harness surface (`Flat` layout, `trace`).

## Decisions

**D1 — `Cartridge.roc` at package root, not inside `Cartridge/`.**
Root placement means `GameBoy` and `Mmu` can reference nested nominal types
(`Cartridge.Rtc`, tag payloads) with qualified names — the subdirectory
nested-type bug only bites non-root modules. Verified that the module can sit
beside the existing `Cartridge/` directory and import `Cartridge/Header`
itself. Alternative considered: `Cartridge/Mbc.roc` + `Cartridge/Rtc.roc`
submodules — rejected for now because every nested type they exposed would
need structural-copy workarounds at use sites; revisit when the compiler bug
is fixed.

**D2 — `Cartridge` value nested inside `Mmu`, not beside it in `GameBoy`.**
The bus decodes addresses, so the bus must reach the cartridge on every
`read`/`write`; keeping it a field of `Mmu` (`{ ..., cart : Cartridge }`)
avoids threading a second state value through every bus call. This mirrors
the hardware: the cartridge hangs off the external bus. Alternative — a
sibling field on `GameBoy` — would force `Mmu.read : Mmu, Cartridge, U16`
everywhere and break the record-update idiom in the hot path.

**D3 — API shape: region handlers plus battery surface.**
`Cartridge` exposes `init : List(U8) -> Cartridge`, `read_rom`,
`read_ram`, `write_control` (ROM-region writes = MBC commands),
`write_ram`, and the battery surface (`battery_bytes`, `fit`, `load`,
`footer`, save-event access), plus RTC advancement given `now`. `Mmu.read`/
`Mmu.write` keep the address decode and call these for `0x0000–0x7FFF` and
`0xA000–0xBFFF`. Rationale: the decode stays in one place (the bus), and the
cartridge never sees absolute addresses it wouldn't see on real hardware
pins beyond A0–A14 + region selects.

**D4 — Move `rom` into `Cartridge`.**
The full ROM image is cartridge property; `Mmu.is_cgb()` and `GameBoy`'s
header queries go through `mmu.cart.rom` (or a `Cartridge.header_*` helper).
Alternative — leaving `rom` on `Mmu` — keeps a cartridge field outside the
cartridge and was rejected as exactly the smell this change removes.

**D5 — Unique-ownership discipline transfers verbatim.**
Only `Cartridge`'s own read/write functions touch `cart_ram` and `rom`;
`Mmu` never aliases them. The nested record does not change Roc's ownership
story: one owner per list, accessed through one module's API.

## Risks / Trade-offs

- [Hot-path regression: an extra record hop on every ROM fetch] → The
  region dispatch count is unchanged (same branch, different callee).
  `check/bench` doesn't exist in this repo, but Blargg timing ROMs and the
  play app give a coarse regression signal; if frame pacing degrades in
  `app/ray.roc`, inline the ROM-read fast path back into `Mmu.read` while
  keeping state ownership in `Cartridge`.
- [Record-update churn: `{ ..mmu, cart: { ..cart, ... } }` nesting] → Keep
  cartridge mutations inside `Cartridge` functions returning a new
  `Cartridge`; `Mmu` does exactly one nested update per delegated write.
- [Silent behavior drift in RTC/battery edge cases] → `check/battery` covers
  latch, halt, catch-up, footer variants, and save-event edges with synthetic
  carts; run it after each move, not once at the end.
- [Compiler quirks with a root module shadowing its directory name] →
  Verified compiles on the pinned nightly before this design; if a future
  nightly regresses this, fall back to naming the module `Cart.roc`.

## Migration Plan

Pure in-repo refactor: land as one commit series gated by
`roc check package/main.roc`, `roc test package/main.roc`,
`check/battery`, and the Blargg passlist. Rollback = revert; no data or
format migration (`.sav` files unchanged).

## Open Questions

None.
