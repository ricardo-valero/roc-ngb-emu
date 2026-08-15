# sm83-single-step: per-opcode CPU vectors, ported from the NES sibling

## Why

The SM83 CPU is verified only end-to-end (Blargg serial ROMs, mooneye
acceptance): a regression says "checksum failed", not which opcode broke,
and nothing at all observes *where inside an instruction* memory accesses
land. roc-nes-emu's `check/single-step` closed exactly this gap for the
6502 with Tom Harte's SingleStepTests; the same project publishes SM83
vectors (~1k generated cases per opcode, initial state → final state plus
the per-M-cycle bus activity). Porting the check gives per-opcode,
field-by-field verification — and the bus-activity comparison is the
prerequisite for ever modeling the wave-RAM access window that keeps
`dmg_sound` singles 09/10/12 informative-only (see apu-verification).

## What Changes

- New `check/single-step/` slice: a pure-Roc fetch app (SM83 vectors from
  the SingleStepTests repo into a gitignored `data/`, resumable) and a
  runner that parses the fixed JSON schema with the same minimal
  recursive-descent approach as the NES check, executes each case through
  the pure core, and diffs registers, `ime`, touched memory, and cycle
  count field-by-field. All 500 opcode files (256 base + 244 CB) gate.
- The core grows a single-step test surface (exposed via the package):
  construct machine state from raw register values plus flat 64 KiB
  memory (the vectors assume no GB memory map), execute exactly one
  instruction, and read back registers, cycles, and memory.
- The memory bus grows an optional per-access trace (address, value,
  read/write, in access order), off in normal operation, so the runner
  can compare memory-access *placement* against each case's per-M-cycle
  bus activity — the sub-instruction honesty end-to-end ROMs can't see.
  This trace is also a building block for the wishlist debugger.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `cpu-verification`: adds the single-step vector suite — fetch flow,
  runner, and gating (every opcode file passes; the passing set never
  shrinks) — alongside the existing ROM-based ladder.
- `core-debug`: adds the single-step harness surface (raw-state
  construction over flat memory, one-instruction step, state readback)
  and the optional bus access trace.

## Impact

- New code: `check/single-step/{fetch.roc,main.roc}` (shape ported from
  roc-nes-emu's, adapted to the SM83 schema and this repo's `check/lib`
  conventions).
- Core: a small exposed harness module (package currently exposes only
  `Header` and `GameBoy`); `Mmu` gains a flat/trace-capable mode used
  only by checks — no behavior change for apps when disabled.
- Test data: ~500 JSON files fetched on demand, gitignored, like the NES
  repo's `check/single-step/data/`.
- Docs: README gains the fetch/run instructions; WISHLIST retires the
  "SM83 SingleStepTests" item.
