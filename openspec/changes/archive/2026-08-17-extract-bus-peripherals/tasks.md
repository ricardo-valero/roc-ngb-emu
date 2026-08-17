# Tasks — Rename Mmu to Bus, Extract Timer and Joypad

## 1. Rename (pure, no logic changes)

- [x] 1.1 `git mv package/Mmu.roc package/Bus.roc`; rename the `Mmu` nominal
      type and all `Mmu.`/`mmu` identifiers to `Bus`/`bus` across
      `Bus.roc`, `GameBoy.roc` (including the `mmu` field → `bus`),
      `Ppu.roc`, `Apu.roc`, and `main.roc`
- [x] 1.2 `roc check package/main.roc` and `roc test package/main.roc` pass
      on the rename alone; commit this stage separately

## 2. Extract Timer

- [x] 2.1 Create `package/Timer.roc` per design D2: counters record + pure
      `tick` (prescaler table, DIV increment, TIMA overflow reload via TMA,
      `irq` flag) + DIV-reset helper; port the tick arithmetic verbatim from
      `Bus.roc`
- [x] 2.2 Replace `div_counter`/`tima_counter` fields in `Bus` with
      `timer : Timer`; `Bus.tick` reads DIV/TAC/TIMA/TMA, delegates, pokes
      results, raises IF bit 2 on `irq`; DIV write-reset delegates
- [x] 2.3 Move the timer inline expects (DIV rate/reset, TIMA rates,
      overflow reload + IF, disabled-hold) — bus-driven versions stay in
      `Bus.roc`, add direct `Timer.tick` unit expects for the overflow wrap
- [x] 2.4 `roc test package/main.roc` passes; import `Timer` from `main.roc`

## 3. Extract Joypad

- [x] 3.1 Create `package/Joypad.roc` per design D3: `Buttons` type,
      `none`, `p1` (select bits + active-low nibble math ported verbatim)
- [x] 3.2 `Bus` keeps `buttons : Joypad.Buttons`, delegates its P1 read;
      switch `no_buttons` callers (`GameBoy.no_input`, expects) to
      `Joypad.none`; keep a `Bus.no_buttons` delegator only if a frontend
      needs it
- [x] 3.3 Move/keep the joypad expects (group select, AND of both groups,
      none-selected reads 0xF) and add direct `Joypad.p1` unit expects
- [x] 3.4 `roc test package/main.roc` passes; import `Joypad` from `main.roc`

## 4. Verify

- [x] 4.1 `roc check package/main.roc` and `roc test package/main.roc` pass
      (full suite)
- [x] 4.2 `roc check/run.roc -- check/mooneye/passlist` passes (timer/halt
      acceptance subset)
- [x] 4.3 `roc check/run.roc -- check/blargg/passlist` passes
      (`instr_timing`, `dmg_sound` exercise the timer paths)
- [x] 4.4 `roc check/battery/main.roc` and `roc check/acid2/main.roc` pass;
      single-step spot-check (e.g. `00`, `3e`, `f0`) green
- [x] 4.5 Build `app/ray.roc` and boot a game to confirm input still works
      (buttons through the renamed bus)
