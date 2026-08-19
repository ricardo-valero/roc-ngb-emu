# Design — Move the Access Trace from Bus to Cpu

## Context

See proposal.md — Why. Constraints that shape the approach:

- All CPU memory traffic already funnels through two chokepoints
  (`Cpu.mem_read`, `Cpu.mem_write`); the trace's correctness depends on that
  funneling and this change must not add a third door.
- The trace accumulates *across* steps (the harness steps, then reads
  `access_trace`), so it must live in durable `Cpu` state, not only in the
  per-instruction `St` threading record.
- `check/single-step` is the trace's only consumer and its sharpest gate: it
  compares per-opcode access placement, so any drift in contents or order
  fails loudly.
- The `St`/`Cpu` records are the hottest data in the program;
  extract-cpu-module's Blargg timing (10m22 post-extraction) is the perf
  baseline.

## Goals / Non-Goals

**Goals:**

- The bus memoryless; the CPU owning the instrument that transcribes its own
  accesses.
- `mem_read`/`mem_write` symmetric — one visible `match st.trace` each.
- Byte-identical trace semantics, gated by the single-step suite.

**Non-Goals:**

- No PPU trace, no whole-system bus analyzer (proposal — Non-Goals).
- No new tracing API; `GameBoy.access_trace` keeps its shape.

## Decisions

**D1 — `trace` is a `Cpu` field, mirrored into `St`.**
`Cpu := { reg, ime, halted, ei_pending, trace }` and
`St : { reg, ime, halted, ei_pending, trace, bus }`; `to_st`/`of_st` carry it
like every other CPU field. Alternative — keep it out of `Cpu` and pass a
trace accumulator separately — rejected: it must survive between
`step_instruction` calls, which is exactly what `Cpu` fields are for.

**D2 — Recording stays at the two chokepoints, written symmetrically.**
Both accessors `match st.trace`; the `NoTrace` arm is the exact current hot
path (a pure `bus.read`, an untraced `bus.write`). `Bus.trace_access` and
`Bus.read_traced` are deleted rather than kept as shims — they would be dead
code with an owner mismatch, the thing this change exists to remove.

**D3 — `Bus.flat` stops implying tracing; `from_raw` opts in.**
Flat layout and tracing were coupled only because both are harness
conveniences. Layout is a bus property; the probe is CPU instrumentation.
`from_raw` builds its `Cpu` with `trace: Trace([])`, so the one surface whose
contract mentions the trace ("access trace on") is the one that arms it.
`Cpu.roc`'s own expects over `Bus.flat` are unaffected (they assert registers
and cycles, not traces, and `Cpu.init` starts `NoTrace`).

**D4 — `GameBoy.access_trace` delegates unchanged.**
It reads `gb.cpu.trace` instead of `gb.bus.trace`; signature and name stay,
so `check/single-step` and the inline expects change zero lines. Consistent
with extract-cpu-module D6: the harness surface lives on `GameBoy` and
delegates.

## Risks / Trade-offs

- [Trace contents or order drift] → the single-step suite compares placement
  per opcode across 500 vector files; run the full suite, not a spot-check.
  The PUSH/POP trace-order expects in `GameBoy.roc` back it up.
- [Hot-record growth: `Cpu` and `St` gain a tag-union word] → compare Blargg
  wall clock against the 10m22 baseline; the extraction's measurement showed
  this class of change is noise-level, but measure anyway. Fallback if it
  regresses: none needed a priori — the field is one machine word in the
  `NoTrace` case.
- [A stray caller of the deleted Bus methods] → `roc check` across package
  and checks; grep for `trace_access`/`read_traced`/`bus.trace` before
  declaring done.

## Migration Plan

Single-stage, in-repo: one commit (field move + accessor rewrite + caller
updates land together; the tree does not compile in between), verified by
the full gate set. Rollback = revert. No save-format, ROM-format, or
frontend protocol surface is touched.

## Open Questions

None.
