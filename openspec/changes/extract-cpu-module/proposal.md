# Extract the CPU as a Root Module

## Why

`Cpu/` is a directory with no parent module: it holds only decode and data
(`Register`, `Register/Status`, `Alu`, `Instruction`). The CPU's *execution* —
`run`, `run_bare`, `execute`, `execute_cb`, `src8`/`dst8`, `mem_read`/
`mem_write`, `push16`/`pop16`, `carry_flag`, `sign_extend`, plus interrupt
polling and dispatch — lives in `GameBoy.roc`, and the CPU's state is scattered
there as four loose fields (`reg`, `ime`, `halted`, `ei_pending`).

That is an asymmetry with every other unit. `Ppu.roc` owns PPU state and
`tick`; `Apu.roc` owns APU state and `tick`; `Timer.roc` and `Joypad.roc` owe
their existence to the same principle. The CPU has no such module, so
`GameBoy.roc` (991 lines) is doing three jobs at once: the SoC composition, the
CPU execution unit, and the harness/frontend API. Extracting `Cpu.roc` is the
next slice in the module-per-hardware-block reorganization that
`extract-cartridge-module` and `extract-bus-peripherals` established, and it
makes `GameBoy.roc` the SoC the README's block diagram already claims it is.
Sibling roc-ngba-emu splits this way already (`Cpu.roc` + `Step.roc` vs
`Gba.roc`).

## What Changes

- New `package/Cpu.roc` — the parent module of the existing `Cpu/` directory,
  exactly as `Cartridge.roc` parents `Cartridge/`:

  ```
  Cpu := { reg : Register, ime : Bool, halted : Bool, ei_pending : Bool }.{
      step : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }
  }
  ```

  parallel to `Ppu.tick : Ppu, Bus, U64 -> { ppu : Ppu, bus : Bus }`. It absorbs
  the interrupt poll, dispatch, HALT wake, EI delay, the whole
  `execute`/`execute_cb` tree, the operand helpers, and the CPU-path bus
  accessors (including the access trace threading).
- `GameBoy` replaces its four CPU fields with one `cpu : Cpu`. `GameBoy.step`
  becomes: call `Cpu.step`, then hand the reported cycles to
  `finish` — the peripheral fan-out (bus/timer tick, PPU, APU, double-speed
  halving) that stays SoC-level.
- **BREAKING** (package-internal + two in-repo callers): `gb.reg`, `gb.ime`,
  `gb.halted`, `gb.ei_pending` become `gb.cpu.reg`, `gb.cpu.ime`,
  `gb.cpu.halted`, `gb.cpu.ei_pending`. No published API is affected — the
  package is consumed only from this repo.
- The harness surface (`from_raw`, `raw`, `step_instruction`, `access_trace`)
  stays on `GameBoy` and delegates, so `check/single-step` keeps its entry
  points.
- Drop the two dead type annotations `ppu : Ppu` (`Ppu.roc:16`) and
  `apu : Apu` (`Apu.roc:55`): stale workarounds from an older nightly, since
  each function's return annotation already constrains the record literal.
  Verified — removing both leaves `roc check` clean. The similar-looking
  `base : Bus` in `Bus.init` and `gb : GameBoy` in `GameBoy` methods are **not**
  dead and stay: those values have methods called on them before they are
  returned, and method dispatch needs the nominal type known up front.
- No behavior change: instruction semantics, cycle counts, interrupt timing,
  and trace ordering stay byte-identical.

## Capabilities

### New Capabilities

None — structural refactor.

### Modified Capabilities

None. `cpu-core` still requires "a step function that fetches, decodes, and
executes one instruction … returning the updated state and the machine cycles
consumed" and the interrupt semantics unchanged; `core-debug`'s single-step
harness and access-trace requirements keep their entry points. Only which
module implements them moves. This change sets `skip_specs: true` in its
`.openspec.yaml` accordingly.

## Impact

- New `package/Cpu.roc`; `package/main.roc` import list updated.
- `package/GameBoy.roc` — the execution core moves out (expect it to land near
  ~350 lines: SoC composition + harness/frontend API + its inline expects).
- `package/Ppu.roc`, `package/Apu.roc` — one dead annotation line each.
- `app/web/main.roc:120-121` — debug status line reads `gb.cpu.reg`, `gb.cpu.ime`,
  `gb.cpu.halted`. Note this file has uncommitted edits; rebase carefully.
- `check/single-step/main.roc:450` — `g.ei_pending` → `g.cpu.ei_pending`.
- Gates (must stay green, unchanged): `roc check` / `roc test`, the SM83
  single-step suite (the sharpest gate here — it compares registers, IME, EI
  latch, cycles, and access placement per opcode), the Blargg passlist, the
  mooneye acceptance subset, `check/battery`, `check/acid2`.

## Non-Goals

- Splitting the harness/frontend API (`from_raw`, `battery`, breakpoints,
  `run_frame`) out of `GameBoy.roc`. That is the *third* job the file does and a
  defensible next slice, but bundling it here would make the diff unreadable
  against the single-step gate.
- Modeling the PPU's dedicated VRAM/OAM port. The review confirmed a real
  accuracy gap — `Bus.read` has no mode-based blocking, so the CPU can read VRAM
  during mode 3 where hardware returns `0xFF`, and nothing in the check suite
  catches it (acid2 does not test it; the mooneye locking ROMs are not in the
  gating passlist). It is recorded on the accuracy tail alongside the
  sub-instruction TIMA reload behavior and in the block diagram's deltas card;
  it is not a module-boundary problem and does not belong in this change.
- No PPU/APU register module. Correctly absent: CPU registers are the only
  registers on the console that are not memory-mapped, so they need a module to
  own them. LCDC/STAT/LY/palettes (`0xFF40`–`0xFF4B`) and NR10–NR52
  (`0xFF10`–`0xFF26`) live in `Bus.mem` because that is how the CPU reaches
  them; `Ppu`/`Apu` hold exactly what the hardware hides.
