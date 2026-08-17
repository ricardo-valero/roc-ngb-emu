# Design — Rename Mmu to Bus, Extract Timer and Joypad

## Context

See proposal.md — Why. Constraints that shape the approach:

- `Mmu` currently holds timer state (`div_counter`, `tima_counter`, `tick`
  at the old `Mmu.roc:871-910`, DIV write-reset in the write dispatch) and
  joypad state (`buttons`, `read_p1`, `button_nibble`, `no_buttons`,
  `set_buttons`); the DIV/TIMA/TMA/TAC and P1 register *bytes* live in
  `mem`, which only `read`/`write`/`poke` may touch (unique ownership).
- The nested-type compiler bug (still live on the pinned nightly) makes
  root-level modules the safe placement for anything exposing nominal
  types — `Timer.roc` and `Joypad.roc` go at the package root, matching
  `Cartridge.roc` and sibling roc-ngba-emu.
- `GameBoy.no_input` and frontends construct button records via
  `Mmu.no_buttons({})`; `check/battery` and both apps only use `GameBoy`'s
  API, so the blast radius is package-internal.

## Goals / Non-Goals

**Goals:**

- `Bus.roc` named for what it is; timer and joypad as root hardware-block
  modules; `GameBoy.bus` field name matching.
- Byte-identical behavior, gated by the existing check slices.

**Non-Goals:**

- No serial extraction (small, and entangled with the Blargg reporting
  channel — a later slice if ever).
- No APU sample-ring/event-queue ownership move (its own slice).
- No timer accuracy upgrades (sub-instruction DIV edges, TIMA reload
  quirks): mooneye's currently-gating subset defines the bar; the rest of
  its timer ROMs stay informative.

## Decisions

**D1 — Rename first, extract second (two commit stages, one change).**
The rename is ~430 mechanical occurrences across four files; doing it as
its own gated stage (check + test green in between) keeps the extraction
diffs readable. Alternative — extract under the old name and rename later —
was rejected: every extracted signature would churn twice.

**D2 — Timer is pure arithmetic; the bus keeps the register bytes.**
`Timer := { div_counter : U64, tima_counter : U64 }` with
`tick : Timer, { div : U8, tac : U8, tima : U8, tma : U8 }, U64 ->
{ timer : Timer, div : U8, tima : U8, irq : Bool }` (plus a DIV-reset
helper). The bus reads the four bytes, calls `tick`, pokes the results,
and raises IF bit 2 when `irq` is set. Rationale: moving the register
bytes out of `mem` would break the one-owner discipline and the raw
`poke`/`read_raw` paths for no behavioral gain; the arithmetic is the part
worth naming. Alternative — Timer owning its bytes — rejected for
ownership churn.

**D3 — Joypad owns the Buttons type and the matrix math only.**
`Joypad.roc` exposes `Buttons` (the record type alias), `none({})`, and
`p1 : Buttons, U8 -> U8` (select bits in, register value out). The bus
keeps `buttons : Joypad.Buttons` as a field and delegates `read_p1`.
`Mmu.no_buttons` callers switch to `Joypad.none`. The keypad has no
interrupt on this codebase yet, so no IRQ surface. Alternative — a
stateful Joypad record inside the bus — rejected: one Bool-record field
does not need a wrapper.

**D4 — Rename `GameBoy.mmu` → `bus` in the same change.**
Grep confirms no consumer outside `package/` touches the field; leaving a
`bus.mmu`-style mismatch would recreate the naming confusion this change
exists to remove.

## Risks / Trade-offs

- [Mechanical rename swallows a real change in review] → Stage the rename
  as a pure `git mv` + identifier-substitution commit with check/test green
  before any extraction lands on top.
- [Timer refactor perturbs the overflow-reload arithmetic] → The reload
  wrap (`tma + excess % (0x100 - tma)`) moves verbatim; Blargg
  `instr_timing` plus the Mmu timer expects (which move to Bus/Timer)
  gate it; mooneye's timer subset stays the acceptance bar.
- [Hot-path cost: tick now reads four bytes per call] → It already does —
  the reads move, they don't multiply; Blargg timing ROMs would surface a
  regression.

## Migration Plan

In-repo refactor in two gated stages (rename, then extractions), each
verified by `roc check` / `roc test` and the check slices. Rollback =
revert; no data migration.

## Open Questions

None.
