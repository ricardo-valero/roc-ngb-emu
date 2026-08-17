# Design — Extract the CPU as a Root Module

## Context

See proposal.md — Why. Constraints that shape the approach:

- Every execution path in `GameBoy.step` already has the same shape: do CPU
  work, then call `finish(gb, total_cycles)`. Interrupt dispatch ends in
  `finish(…, 20)`; the halted-with-nothing-pending path ends in
  `finish(gb, 4)`; `run` is `run_bare` followed by `finish(gb, cycles)`. That
  single exit point is what makes the extraction behavior-preserving by
  construction rather than by inspection.
- `finish` is the only thing in that path that is *not* CPU work: it halves
  cycles for double speed, ticks the bus (timer), the PPU, and the APU.
- The access trace is threaded through `mem_read`/`mem_write` so it stays in
  CPU-access order. Those accessors are CPU-path reads/writes, not bus API.
- The nested-type compiler bug (still live on the pinned nightly) makes the
  package root the safe placement for anything exposing nominal types — and
  `Cartridge.roc` + `Cartridge/Header.roc` already proves a root module can
  parent a same-named directory.
- `check/single-step` compares registers, IME, the EI latch, cycles, *and*
  memory-access placement across 498 opcode files. It is the tightest gate this
  repo has for exactly the code being moved.
- `app/web/main.roc` currently has uncommitted working-tree edits.

## Goals / Non-Goals

**Goals:**

- `Cpu.roc` at the package root owning CPU state and the full instruction
  step, symmetric with `Ppu.tick` / `Apu.tick`.
- `GameBoy.roc` left holding two jobs instead of three: SoC composition and
  the harness/frontend API.
- Byte-identical behavior, gated by the single-step suite.

**Non-Goals:**

- No change to instruction semantics, cycle tables, or trace ordering — this
  change earns its keep only if the gates are unchanged and green.
- No `Step.roc`-style threading context (roc-ngba-emu has one because ARM
  cycle costs are per-access; SM83 costs here are table-driven per
  instruction, so a context record would buy nothing).
- No re-litigating the harness API split or VRAM locking (proposal — Non-Goals).

## Decisions

**D1 — `Cpu.step` makes the whole CPU-side decision; `GameBoy` keeps
`finish`.**
`Cpu.step : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }` absorbs the
interrupt poll (`IE & IF & 0x1F`), `dispatch`, the HALT wake, the EI-delay
latch, and `run_bare`. `GameBoy.step` becomes:

```
r = gb.cpu.step(gb.bus)
finish({ ..gb, cpu: r.cpu, bus: r.bus }, r.cycles)
```

Rationale: IME, HALT, and the EI latch *are* CPU state, and dispatch is a CPU
microcode sequence (push PC, clear the IF bit, jump to the vector, 20 cycles)
that merely reads IE/IF through the bus — the same way every fetch does. The
proposal's phrase "GameBoy dispatches interrupts" describes the block diagram,
not the code: with `ime` and `halted` inside `Cpu`, a `step` that reports
cycles must already have decided whether a vector was taken.
Alternative — leave `dispatch` on `GameBoy` and have it poke `cpu.ime` —
rejected: it splits one decision across two modules and forces `Cpu` to expose
IME as a setter.

**D2 — Peripheral fan-out stays SoC-level, unchanged.**
`finish` is not moved and not renamed. It is the one function in the step path
that knows there are four units and a double-speed clock domain — which is
precisely `GameBoy`'s job. Its signature and body are untouched, so the
double-speed halving (the subtlest timing in the file, gated by the
217-NOP line-boundary expect) cannot drift.

**D3 — Return a record, not a tuple.**
`{ cpu, bus, cycles }` matches `Ppu.tick`'s `{ ppu, bus }` and reads at the
call site. `GameBoy.step` keeps its existing `(GameBoy, U64)` tuple return —
that is the public shape `check/run.roc` and the expects already destructure,
and changing it would add caller churn for no gain.

**D4 — The CPU-path bus accessors move with the CPU.**
`mem_read`, `mem_write`, `imm16`, `push16`, `pop16`, `sign_extend`,
`carry_flag`, `apply_flags`, `src8`, `dst8`, `read16m`, `write16m`,
`acc_result`, `rotate_a`, `flags_only`, `alu8`, `rmw`, `execute`, `execute_cb`
all become `Cpu` internals, rethreaded from `GameBoy` to `Cpu` + `Bus`. The
trace-vs-`NoTrace` branch in `mem_read` goes with them: the trace records the
*CPU's* accesses in program order, so it belongs on the CPU side of the
boundary even though the ring lives in `Bus`.

**D5 — One `cpu : Cpu` field; callers read `gb.cpu.*` directly.**
No delegating accessors (`GameBoy.pc()`, `GameBoy.ime()`) are added. There are
exactly two out-of-package readers — the web debug status line and the
single-step checker's `ei_pending` assertion — and `gb.cpu.ime` is more honest
about where the state lives than a forwarding method would be.

**D6 — The harness surface stays on `GameBoy`, delegating.**
`from_raw`, `raw`, `step_instruction`, and `access_trace` keep their names and
signatures on `GameBoy`; `from_raw` builds a `Cpu` from the raw fields, `raw`
reads back out of `gb.cpu`, and `step_instruction` delegates to
`Cpu.step_instruction` (the extracted `run_bare` — one instruction, no
interrupt poll, no peripheral ticks). `check/single-step/main.roc` therefore
changes by exactly one field path, keeping the gate's diff trivially auditable
while the code under it moves wholesale.

**D7 — Inline expects stay where their machine is built.**
The instruction-level expects in `GameBoy.roc` (LD/XOR/CB/stack/CALL-RET, EI
delay, HALT wake, interrupt dispatch, double speed) are all written against
`GameBoy.init(rom_with(…))` — a real cartridge on a real bus. They stay in
`GameBoy.roc`, edited only for the `gb.cpu.*` field path. New `Cpu`-level
expects are added only where they are cheap over `Bus.flat` (the EI-delay
latch and the dispatch vector arithmetic).
Alternative — move all of them into `Cpu.roc` and rewrite them against flat
memory — rejected: rewriting assertions in the same change that moves the code
under them is exactly how a behavior change hides inside a refactor.

## Risks / Trade-offs

- [The move silently perturbs cycle accounting, interrupt ordering, or access
  placement] → The SM83 single-step suite is the gate and it compares all
  three per opcode; run the full 498-file suite, not a spot-check, before
  calling this done. Blargg `instr_timing` and the mooneye halt/timer subset
  back it up.
- [Perf regression from nesting the hot record] → The per-step path currently
  copies four flat fields on the `GameBoy` record; after the change it copies
  one nested `Cpu`. roc-ngba-emu's `Cpu.roc` header records that copy/refcount
  churn on the hot record dominated its NES-era perf pass, so this is not
  hypothetical in this codebase. Mitigation: time a fixed headless run
  (`check/run.roc` over the Blargg passlist) before and after and compare; if
  it regresses, the fallback is flattening `Cpu`'s fields back into the step
  signature rather than the record — but do not pre-optimize for it.
- [`app/web/main.roc` edits get clobbered] → It has uncommitted changes;
  stash-and-reapply or edit in place, and diff before committing.
- [The tree is un-buildable between moving the code and fixing the callers] →
  Land the module move and the two caller updates as one commit (§2 below);
  only the dead-annotation cleanup is separable.

## Migration Plan

In-repo refactor, two gated stages:

1. `Cpu.roc` created, execution and state moved, `GameBoy` rewired, and the
   two external callers updated — one commit, since the tree does not compile
   in between. Verified by `roc check` / `roc test` plus the full gate set.
2. The two dead annotations removed — independent, trivially revertible.

Rollback = revert. No data migration; no save-format, ROM-format, or frontend
protocol surface is touched.

## Open Questions

None.
