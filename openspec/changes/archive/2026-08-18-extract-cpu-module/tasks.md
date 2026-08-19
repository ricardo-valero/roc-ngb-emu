# Tasks — Extract the CPU as a Root Module

## 1. Baseline

- [x] 1.1 Confirm the gate set is green *before* touching anything, so any
      later failure is attributable: `roc check package/main.roc`,
      `roc test package/main.roc`,
      `roc check/single-step/main.roc -- check/single-step/data/*.json`,
      `roc check/run.roc -- check/blargg/passlist`,
      `roc check/run.roc -- check/mooneye/passlist`,
      `roc check/acid2/main.roc`, `roc check/battery/main.roc`
- [x] 1.2 Record a baseline timing for `roc check/run.roc -- check/blargg/passlist`
      (wall clock) for the perf comparison in 4.2
- [x] 1.3 Stash or note the uncommitted `app/web/*` edits so step 3.1 does not
      clobber them

## 2. Create Cpu.roc and move the execution core

- [x] 2.1 Create `package/Cpu.roc` at the package root (parent of the existing
      `Cpu/` directory, as `Cartridge.roc` parents `Cartridge/`) with
      `Cpu := { reg : Register, ime : Bool, halted : Bool, ei_pending : Bool }`,
      `init : {} -> Cpu`, and imports of `Cpu/Register`, `Cpu/Register/Status`,
      `Cpu/Alu`, `Cpu/Instruction`, `Bus`
- [x] 2.2 Move the operand and bus-access helpers verbatim from `GameBoy.roc`,
      rethreading `GameBoy` → `Cpu` + `Bus` (design D4): `mem_read`,
      `mem_write`, `imm16`, `push16`, `pop16`, `sign_extend`, `carry_flag`,
      `apply_flags`, `src8`, `dst8`, `read16m`, `write16m`, `acc_result`,
      `rotate_a`, `flags_only`, `alu8`, `rmw`. Keep the `Trace`/`NoTrace`
      branch in `mem_read`
- [x] 2.3 Move `execute` and `execute_cb` verbatim (the largest block — no
      semantic edits, only the state-record rename)
- [x] 2.4 Add `Cpu.step_instruction : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }`
      — the old `run_bare`, including the EI-delay latch
- [x] 2.5 Add `Cpu.step : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }` —
      the interrupt poll (`IE & IF & 0x1F`), `dispatch` (push PC, clear the IF
      bit, vector `bit << 3 + 0x40`, 20 cycles), the HALT wake (4 cycles when
      nothing is pending), and otherwise `step_instruction` (design D1). It
      reports cycles and does **not** tick peripherals
- [x] 2.6 Add direct `Cpu` expects over `Bus.flat` for the EI-delay latch and
      the dispatch vector arithmetic (design D7 — the cheap two only)

## 3. Rewire GameBoy and the callers (one commit — the tree does not
   compile in between)

- [x] 3.1 Replace `GameBoy`'s `reg`/`ime`/`halted`/`ei_pending` fields with
      `cpu : Cpu`; update `init` (including the CGB `A = 0x11` write) and
      `from_raw`/`raw` to build and read `gb.cpu` (design D6)
- [x] 3.2 `GameBoy.step` becomes `r = gb.cpu.step(gb.bus)` then
      `finish({ ..gb, cpu: r.cpu, bus: r.bus }, r.cycles)`; delete `run`,
      `run_bare`, and `dispatch` from `GameBoy.roc`. Leave `finish` byte-identical
      (design D2)
- [x] 3.3 `GameBoy.step_instruction` delegates to `Cpu.step_instruction`;
      `access_trace` unchanged; `run_until`'s PC read becomes
      `gb.cpu.reg.read16(ProgramCounter)`
- [x] 3.4 Update the inline expects in `GameBoy.roc` for the `gb.cpu.*` field
      path only — no assertion values change (design D7)
- [x] 3.5 Update `check/single-step/main.roc:450` — `g.ei_pending` →
      `g.cpu.ei_pending`
- [x] 3.6 Update `app/web/main.roc:120-121` — `gb.reg` / `gb.ime` / `gb.halted`
      → `gb.cpu.reg` / `gb.cpu.ime` / `gb.cpu.halted`; diff against the stashed
      edits from 1.3 before committing
- [x] 3.7 Add `Cpu` to `package/main.roc`'s import list

## 4. Verify

- [x] 4.1 `roc check package/main.roc` and `roc test package/main.roc` pass
- [x] 4.2 `roc check/single-step/main.roc -- check/single-step/data/*.json`
      passes on the **full** 498-file suite (registers, IME, EI latch, cycles,
      and access placement — the gate for everything moved in §2)
- [x] 4.3 `roc check/run.roc -- check/blargg/passlist` and
      `roc check/run.roc -- check/mooneye/passlist` pass; compare the Blargg
      wall clock against the 1.2 baseline and note any regression (design —
      Risks: hot-record copy churn)
- [x] 4.4 `roc check/acid2/main.roc`, `roc check/sound/main.roc`, and
      `roc check/battery/main.roc` pass
- [x] 4.5 Build `app/ray.roc` and boot a game — video, audio, and input still
      work; build the web app and confirm the debug status line renders
- [x] 4.6 Confirm `GameBoy.roc` now reads as SoC composition + harness/frontend
      API only, with no `execute`/operand-decode code left in it

## 5. Dead annotation cleanup (separable commit)

- [x] 5.1 Remove the `ppu : Ppu` annotation line in `Ppu.init` (`Ppu.roc:16`)
      and the `apu : Apu` line in `Apu.init` (`Apu.roc:55`); leave `base : Bus`
      in `Bus.init` and the `gb : GameBoy` annotations alone (proposal — they
      have methods called on them before return and are load-bearing)
- [x] 5.2 `roc check package/main.roc` and `roc test package/main.roc` pass
