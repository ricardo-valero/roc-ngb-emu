# sm83-single-step Design

## Context

See proposal.md for motivation. What shapes the approach:

- **The execution core lives in `GameBoy.roc`**, not the `Cpu/` modules:
  `Cpu/Instruction.roc` is a pure decoder, but operand fetch/store,
  cycle accounting, and the instruction dispatch all operate on the full
  `gb` record via `gb.mmu.read/write`. There is no standalone "CPU with
  a pluggable bus" to lift out — the harness must go *through*
  `GameBoy`, not around it.
- **The package exposes only `Header` and `GameBoy`** (`package/main.roc`);
  the check needs an exposed surface for raw-state construction and
  single-instruction stepping.
- **`Mmu` implements the real GB memory map** (cartridge banking, echo,
  I/O); the SM83 vectors assume flat 64 KiB RAM with no mapping.
- **Cycle accounting is aggregate**: execute returns per-instruction
  T-cycle totals; nothing records which access happened in which
  M-cycle. The vectors carry per-M-cycle bus activity.
- **The NES sibling's shape** (`roc-nes-emu/check/single-step/`):
  resumable pure-Roc fetch app; minimal recursive-descent JSON parser
  over the fixed machine-generated schema (no JSON dependency); per-file
  pass/fail with first-N failing-case detail; nonzero exit on any
  failure. Note the NES runner only *counts* the cycles array — this
  port goes further and compares access placement, which is its stated
  point.

## Goals / Non-Goals

**Goals:**

- Every SM83 vector file (256 base + CB-prefixed opcodes) executes
  through the same code paths games exercise — the real `GameBoy` step,
  not a parallel test-only interpreter.
- Access-placement comparison precise enough to make the wave-RAM
  window (apu-verification 09/10/12) plannable later.
- Zero cost and zero behavior change for normal emulation when the
  trace is off.

**Non-Goals:**

- Modeling the wave-RAM access window itself (a later APU change; this
  change only builds the instrument that measures placement).
- Interrupt/HALT vector coverage beyond what the SingleStepTests set
  exercises (the vectors are single-instruction; interrupt *dispatch*
  timing stays with the mooneye ROMs).
- A Gameboy-Doctor-style golden-trace check (separate wishlist item).
- Debugger UI around the access trace (the trace is core plumbing the
  debugger can reuse later).

## Decisions

1. **Harness enters through `GameBoy`, with a flat-Mmu mode.**
   A new exposed module (e.g. `package/Harness.roc`, mirroring the
   `core-debug` pattern of run_until/breakpoints) builds a `gb` whose
   `Mmu` is in a flat mode: 64 KiB backing list, reads and writes pass
   through verbatim (no banking, no echo, no I/O dispatch), constructed
   from `List(U8)` rather than a parsed cartridge. Alternative
   considered: parameterizing execute over an abstract bus (the NES
   `Bus.flat` shape) — rejected because `GameBoy.roc`'s execute is
   written against the concrete `Mmu` record and threading a type
   parameter through every operand helper is a large refactor for no
   behavioral gain; a flat variant inside `Mmu` is a few lines at its
   read/write entry points.

2. **The access trace lives in `Mmu`, as data, off by default.**
   `Mmu` gains an optional trace field (e.g. `[NoTrace, Trace(List({
   addr : U16, val : U8, dir : [R, W] }))]`); its read/write entry
   points append when tracing. The harness enables it; apps never see
   it. Alternative: a recording wrapper type around `Mmu` — same
   type-parameter problem as above. Alternative: always-on trace —
   rejected; a per-access list append in the hot path taxes every frame
   for data nobody reads. The disabled branch is a single tag check.
   The frozen frame/audio digests (`check/frame` analogue, blargg
   suites) are the guard that the disabled path stays bit-identical.

3. **Placement comparison is by access sequence, not M-cycle slots.**
   The vectors list one entry per M-cycle including idle cycles; the
   core doesn't model idle M-cycles individually. The runner compares
   (a) the ordered subsequence of actual memory accesses (addr, value,
   direction) against the vector's non-idle entries, and (b) total
   cycles against 4 × the full entry count (T-cycles per M-cycle).
   This catches wrong placement *order* and wrong access *count*
   without requiring the core to grow M-cycle-slot bookkeeping.
   If a vector's idle-slot positions ever matter (they shouldn't for
   memory semantics), that's the later M-cycle-accuracy change.

4. **Runner ports the NES parser wholesale, adapted to the SM83
   schema.** Same recursive-descent style over the fixed schema; the
   state object gains the SM83 register set (a f b c d e h l, sp, pc,
   ime, ie) and the cycles array is parsed fully (addr, val, flags
   string) instead of merely counted. First implementation task
   verifies the actual downloaded schema (field names, flag-string
   encoding, CB file naming) against a sample file before freezing the
   parser — the format details are confirmed from data, not from
   memory.

5. **Fetch mirrors the NES fetch app.** Same resumable
   skip-if-present loop over the SingleStepTests raw URLs, extended to
   the CB-prefixed file names (which contain a space — URL-encode it).
   Data lands in `check/single-step/data/`, gitignored like the NES
   repo's.

6. **Gating joins the existing check conventions.** The runner is a
   standalone slice (`check/single-step/main.roc`) like the NES one,
   not a `check/run.roc` passlist consumer — passlists gate ROMs by
   serial verdicts; this check's unit is vector files with field
   diffs. Exit semantics match the repo: any failing case → nonzero.

## Risks / Trade-offs

- [SM83 vector schema differs from the 65x02 schema in details
  (flag-string encoding, `ie` field, file naming)] → schema-verification
  task runs before the parser is written against a fetched sample; the
  parser targets what the data actually says.
- [The core's aggregate cycle timing may disagree with vectors on
  instructions whose sub-instruction placement was never observable
  before] → that's the check working as intended; triage failures into
  core fixes. If a class of placement failures is real hardware-model
  debt too large for this change, follow the repo's precedent:
  documented exclusion with a written reason, never a silent skip.
- [Trace list appends could allocate heavily if ever enabled during
  long runs] → the trace is per-step and the harness resets it each
  case; document that it's a single-step instrument, not a logging
  facility.
- [Flat-Mmu mode diverges from real-Mmu behavior, so single-step green
  doesn't imply mapped-memory green] → unchanged risk from the NES
  sibling; the ROM suites keep covering the mapped path.

## Open Questions

- Whether `ie` (0xFFFF) appears as a register field or a RAM byte in
  the vectors — resolved by the schema-verification task; the flat
  memory holds it either way.

## Implementation notes (post-apply)

What the schema-verification and green-the-suite phases actually found:

- **Schema**: 1000 cases per file (not ~10k as the 65x02 set); 244 base
  files (no 0xCB byte, none for the 11 illegals) + 256 CB files (named
  `cb XX.json` upstream, saved as `cb-XX.json` locally); `ie` is a state
  field (loaded into flat 0xFFFF); cycles flag strings are `r-m` /
  `-wm` / `---` with no null entries; `fb.json` final states carry an
  extra `ei` key — the EI-pending latch — which the runner compares
  against the core's `ei_pending`.
- **Harness surface**: implemented as `GameBoy` methods (`from_raw`,
  `raw`, `step_instruction`, `access_trace`) rather than a separate
  `package/Harness.roc` — nominal-type construction lives naturally in
  GameBoy.roc, and GameBoy is already the package's exposed module. The
  trace lives in `Mmu` as designed; CPU-path reads thread it through a
  `mem_read` helper (fast no-alloc path when tracing is off), which
  also forced `pop16` to read low-then-high as hardware does — the
  trace keeping the core honest exactly as intended.
- **Exclusions**: two, both stop-state conventions rather than bugs —
  `76.json` (HALT: vectors charge the halt state's own 2 idle M-cycles)
  and `10.json` (STOP: 1-byte stop-mode entry, 3 M-cycles, vs the
  core's speed-switch + padding-skip model). Every other file — 498 of
  500, 498,000 cases — passes with full placement comparison; no core
  fixes were needed beyond the read-order honesty above.
