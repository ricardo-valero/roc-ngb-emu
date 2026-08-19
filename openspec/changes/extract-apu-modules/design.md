# Design — Shape the APU like the CPU

## Context

See proposal.md — Why. Constraints that shape the approach:

- The APU's hot path is the waveform timers in `advance_*`, which consume
  precomputed periods; the decoders run at event time, sequencer time
  (64–512 Hz), and per ~87-cycle sample batch in `mix` — all cold or warm,
  none per-cycle.
- `clock_sweep` writes the swept frequency back to bus registers
  (0xFF13/14), and `wave_out` reads wave RAM — some "channel" work is
  irreducibly bus-coupled.
- The frozen WAV digest (`check/sound`) is bit-exact over a full frame of
  mixed output: any semantic drift anywhere in channels, sequencer, or mixer
  fails it. Blargg `dmg_sound` (in the gating passlist) covers the register
  quirks.
- The nested-type compiler bug is still live on the pinned nightly:
  subdirectory modules should expose top-level nominal types and structural
  records/tags, never nested types referenced from outside (the
  `Instruction.roc` workaround documents this).

## Goals / Non-Goals

**Goals:**

- `Apu/` mirroring `Cpu/` role-for-role: `Channel` where `Register` sits,
  field decoders where `Status.Delta` sits, the sequencer schedule as data.
- `Apu.roc` reduced to bus-coupled orchestration and mixing.
- Sample-identical output, gated by the WAV digest.

**Non-Goals:**

- No event tag union, no behavior change, no PPU work (proposal —
  Non-Goals).

## Decisions

**D1 — Placement is decided by one rule, not case-by-case taste.**
A function moves to `Apu/Channel.roc` iff it is pure over `Channel` plus
scalar arguments; it moves to `Apu/Register.roc` iff it is a pure
byte→meaning decode; it stays in `Apu.roc` iff it touches `Bus`. Every
function in today's `Apu.roc` lands unambiguously under this rule (e.g.
`clock_sweep` and `wave_out` stay — bus writeback and wave-RAM reads;
`clock_envelope` moves — pure once the envelope byte is decoded at the call
site). The rule is checkable in review: `Channel.roc` and `Register.roc`
must not import `/Bus`.

**D2 — `Channel` becomes nominal with methods, like `Register`.**
`Channel := { enabled, length, timer, duty_pos, volume, env_timer, sweep_*,
wave_pos, lfsr, len_en, sweep_neg_used }.{ ... }`. Call sites read
`ch.advance_pulse(period, cycles)` the way `GameBoy` reads
`reg.write8(...)`. The type is top-level in its module, so the nested-type
bug is not in play; `Apu.roc` references it as a plain imported nominal,
exactly as `GameBoy` references `Register`.

**D3 — Decoders take bytes, not the bus.**
`Register.envelope : U8 -> { volume : U8, increase : Bool, period : U8 }`,
`Register.sweep : U8 -> { period : U8, negate : Bool, shift : U8 }`,
`Register.frequency : U8, U8 -> U16`, periods derived from it, NR43 decode,
duty + waveform table, length loads, NRx4 bit tests, `dac_on`, NR50/51
routing. Call sites keep `bus.read_raw(addr)` — the register *map*
(addresses) stays in `Apu.roc`, the register *format* (fields) moves. This
is the same division `Status.Delta` draws: notation in the module, plumbing
at the call site. Returns are structural records/tags to stay clear of the
nested-type bug.

**D4 — The frame-sequencer schedule becomes data.**
`sequencer_clocks : U8 -> { length : Bool, sweep : Bool, envelope : Bool }`
in `Apu.roc` (it is APU scheduling, not a register format), with the
Pandocs table transcribed per step and an expect asserting the full 8-step
schedule. `advance`'s if-chains become guard reads of that record.

**D5 — No decode/execute split for events.**
`handle_event`'s byte match keeps its one-line arms; they get shorter by
calling decoders, not longer by round-tripping through an `Event` tag
union. Alternative — mirror `Instruction.lookup`/`execute` — rejected: the
event space is ~20 entries with a `_ => apu` default, no reference table to
transcribe, and no exhaustiveness property worth asserting.

**D6 — Expects move only when they build no machine.**
The LFSR step expects, duty-ratio expect, and new decoder expects are pure
and move next to their functions. Every expect that builds a `Bus`
(`fresh({})`, trigger/length/sweep scenarios) stays in `Apu.roc` unchanged —
rewriting assertions in the change that moves the code under them is how
drift hides, same rationale as extract-cpu-module D7.

## Risks / Trade-offs

- [Semantic drift during the move] → the WAV digest is bit-exact over a
  full mixed frame and Blargg `dmg_sound` covers register quirks; both must
  pass unchanged. The ~20 inline expects gate the pieces individually.
- [Perf: decoded records instead of inline masks] → decode sites run at
  event/sequencer/sample-batch rates, not per cycle; the hot timers are
  untouched. Compare Blargg wall clock against the 10m11 baseline anyway.
- [Nightly quirks with nominal `Channel` methods] → the pattern (nominal
  record + methods in a subdirectory module, consumed by a sibling) is
  exactly `Cpu/Register.roc`, already proven. Keep returns structural.

## Migration Plan

Single-stage, in-repo: one commit (both new modules + the `Apu.roc` rewire
+ `main.roc` imports land together; the tree does not compile in between),
verified by the full gate set. Rollback = revert. No save-format or
frontend surface is touched.

## Open Questions

None.
