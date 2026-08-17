# Tasks — Extract Cartridge Module

## 1. Create the Cartridge module

- [x] 1.1 Create `package/Cartridge.roc` (root level, beside the `Cartridge/`
      directory) with the nominal `Cartridge` type owning `rom`, `cart_ram`,
      `mbc`, `rom_bank`, `rom_bank_hi`, `bank2`, `mode`, `ram_enable`, `rtc`,
      `ram_written`, `save_events`; move `RtcRegs`, `RtcState`, `rtc_init`,
      `rtc_advance` in from `Mmu.roc`
- [x] 1.2 Move MBC detection from `Mmu.init` into `Cartridge.init : List(U8) -> Cartridge`
      (header type byte → `None`/`Mbc1`/`Mbc3`/`Mbc5`, RAM sizing, RTC presence)
- [x] 1.3 Implement the bus-facing API per design D3: `read_rom`, `read_ram`,
      `write_control`, `write_ram` (moving the banking/RTC-select/latch logic
      out of `Mmu.read`/`Mmu.write`)
- [x] 1.4 Move the battery surface into `Cartridge`: sav bytes with RTC footer,
      fit classification, load (pad/truncate + footer variants), save-event
      signal; port the related inline `expect`s
- [x] 1.5 Add `import Cartridge` to `package/main.roc` and expose it if the
      check slices need it directly; `roc check package/main.roc` passes

## 2. Delegate from the bus

- [x] 2.1 Replace the moved `Mmu` fields with a single `cart : Cartridge`
      field; delete the moved types/functions from `Mmu.roc`
- [x] 2.2 Route `0x0000–0x7FFF` and `0xA000–0xBFFF` in `Mmu.read`/`Mmu.write`
      through the `Cartridge` API (one nested record update per delegated
      write; `Flat` layout path untouched)
- [x] 2.3 Point `Mmu.is_cgb`, `battery_footer`, and any other rom/cart_ram
      references at `cart`; confirm only `Cartridge` functions touch
      `cart_ram`/`rom` (unique-ownership audit per design D5)

## 3. Update consumers

- [x] 3.1 Rewire `GameBoy.battery`, `battery_fit`, `with_battery`, and
      save-event access to forward to the cartridge instead of reaching into
      `Mmu` fields
- [x] 3.2 Check `app/ray.roc`, `app/web/main.roc`, `check/`, and `example/`
      for direct uses of moved `Mmu` fields (e.g. `save_events`, `cart_ram`)
      and update them

## 4. Verify

- [x] 4.1 `roc check package/main.roc` and `roc test package/main.roc` pass
- [x] 4.2 `roc check/battery/main.roc` passes (RTC latch/halt/catch-up, footer
      variants, save-event edges)
- [x] 4.3 `roc check/run.roc -- check/blargg/passlist` passes (includes the
      MBC1 combined `cpu_instrs` ROM); single-step slice still green
- [x] 4.4 Play-test `app/ray.roc` with an MBC3 battery game: save in-game,
      quit, relaunch, confirm the save round-trips and frame pacing is
      unchanged
