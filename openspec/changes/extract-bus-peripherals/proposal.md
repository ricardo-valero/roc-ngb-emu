# Rename Mmu to Bus, Extract Timer and Joypad

## Why

With the cartridge extracted, `Mmu.roc` still plays several SoC peripherals
(timers, joypad, serial) on top of its real role, and its name obscures what
it is: there is no memory management on a DMG — it's the address decoder plus
the bus. Renaming it `Bus` and lifting the timer and joypad out as root
modules continues the module-per-hardware-block reorganization
(`extract-cartridge-module` set the pattern), matches sibling roc-ngba-emu's
naming (`Bus.roc`, `Timer.roc`), and makes the file tree read as the
hardware block diagram's components with `GameBoy.roc` as the SoC enclosure.

## What Changes

- Rename `package/Mmu.roc` → `package/Bus.roc`, the `Mmu` nominal type →
  `Bus`, and the `GameBoy.mmu` field → `bus`. Package-internal only: no
  `Mmu` references exist in `check/`, `example/`, or `app/`.
- New `package/Timer.roc`: the DIV/TIMA divider state (`div_counter`,
  `tima_counter`) and the tick arithmetic (prescaler selection, overflow
  reload through TMA, interrupt request) as a pure data-in/data-out unit;
  the bus keeps owning the DIV/TIMA/TMA/TAC register bytes in `mem` and
  applies the returned updates.
- New `package/Joypad.roc`: the `Buttons` record type, `no_buttons`, and
  the P1/JOYP matrix computation (select bits + active-low nibbles); the
  bus keeps the select bits in `mem` and delegates reads.
- No behavior change: register semantics, timing, and interrupt edges stay
  byte-identical; all check-slice results unchanged.

## Capabilities

### New Capabilities

None — structural refactor; no externally-observable behavior changes.

### Modified Capabilities

None — the `memory-bus` requirements (minimal timer, joypad register,
serial capture, region semantics) stay exactly as specified; only which
module implements them moves. This change sets `skip_specs: true` in
`.openspec.yaml` accordingly.

## Impact

- `package/Mmu.roc` → `package/Bus.roc` (rename + timer/joypad logic out).
- New `package/Timer.roc`, `package/Joypad.roc`.
- `package/GameBoy.roc`, `package/Ppu.roc`, `package/Apu.roc` — import and
  identifier rename (~430 mechanical occurrences package-wide).
- `package/main.roc` — import list updated.
- Gates (must stay green, unchanged): `roc check` / `roc test`, the mooneye
  acceptance subset (timer/halt timing), Blargg passlist (`instr_timing`,
  `dmg_sound` exercise timer-driven paths), `check/battery`, `check/acid2`,
  a single-step spot-check.
