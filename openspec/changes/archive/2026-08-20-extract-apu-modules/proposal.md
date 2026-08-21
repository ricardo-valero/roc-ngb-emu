# Shape the APU like the CPU: Channel and Register Modules

## Why

`Cpu/` earns its readability from a role split: `Register.roc` owns the state
the bus can't reach, `Alu.roc` holds pure math that reads like the reference
material, and `Status.Delta` captures the opcode table's *notation* in types,
so `Alu.roc` transcribes "Z 0 H -" instead of bit-twiddling. The APU has the
same roles smeared through one 607-line file:

- The `Channel` record — timers, duty walker, LFSR, sweep shadow, envelope
  counters — is exactly the CPU-register situation: state the hardware hides
  from the bus. It lives as a structural alias at the top of `Apu.roc` with
  its pure operations (`advance_*`, `clock_length`, `clock_envelope`,
  `clock_lfsr`) scattered below.
- The NRxx field decodes are the missing `Status.Delta` analog: envelope
  volume/direction/period, sweep period/negate/shift, the 11-bit frequency
  split, NR43's divisor/shift/width, duty, length loads, NR50/51 routing —
  all read as `env.shr_zf_wrap(4)` and `nr10.bitwise_and(0x08)` idioms at
  every use site, with comments standing in for the register map.
- The frame sequencer's step schedule (lengths on even steps, sweep on 2 and
  6, envelope on 7) is a literal table in Pandocs, written here as if-chains
  inside `advance`.

There is deliberately **no** `Apu/Instruction.roc` in this plan. The CPU
consumes a program — an unbounded external stream where any of 256 bytes may
appear — so a total, table-transcribing decoder pays for itself with
exhaustiveness expects. The APU consumes a fixed register file re-read
continuously; its event space is ~20 entries with no reference table to
transcribe. The honest Instruction analogs are small: the field decoders and
the sequencer schedule as data.

## What Changes

- New `package/Apu/Channel.roc` — nominal `Channel` owning the hidden
  per-channel state and every operation pure over it: `blank`,
  `advance_pulse`/`advance_wave`/`advance_noise`, `clock_lfsr`,
  `clock_length`, `clock_envelope`, `dac_gate`, `pulse_out`, `noise_out`.
  Parallel to `Cpu/Register.roc`: hardware-hidden state gets a module to own
  it.
- New `package/Apu/Register.roc` — pure byte→meaning decoders for the NRxx
  fields: envelope (`{ volume, increase, period }`), sweep
  (`{ period, negate, shift }`), 11-bit frequency and the derived
  pulse/wave/noise periods, duty (with the waveform table), length loads,
  NRx4's trigger/length-enable bits, DAC-on, NR50/NR51 routing. Decoders
  take bytes; call sites keep their `bus.read_raw` — the bus never enters
  this module.
- `Apu.roc` keeps everything that touches the `Bus` — `tick`, event
  drain/batching, `handle_event`, the triggers, `clock_sweep` (it writes the
  swept frequency back to 0xFF13/14), wave RAM reads, NR52 status, and the
  mixer — now written through the decoders, and the frame-sequencer schedule
  becomes a step→clocks table mirroring Pandocs.
- Expects that need no machine (LFSR steps, duty ratios, new decoder checks)
  move or land next to their functions; expects that build a `Bus` stay in
  `Apu.roc`.
- `package/main.roc` imports the two new modules so their expects run.
- No behavior change: sample-for-sample identical output, gated by the
  frozen WAV digest and Blargg's `dmg_sound` register test.

## Capabilities

### New Capabilities

None — structural refactor.

### Modified Capabilities

None. The APU's behavioral requirements (channel semantics, frame-sequencer
timing, mixing) are unchanged; only which module holds which piece moves.
This change sets `skip_specs: true` in its `.openspec.yaml` accordingly.

## Impact

- New `package/Apu/Channel.roc`, `package/Apu/Register.roc`;
  `package/main.roc` import list updated.
- `package/Apu.roc` — shrinks to orchestration + mixer + its bus-building
  expects (expect roughly 350–400 lines).
- No callers outside the package change: `GameBoy.roc` consumes only
  `Apu.init`/`tick`, which keep their signatures.
- Gates (must stay green, unchanged): `roc check` / `roc test` (the APU's
  ~20 inline expects are the unit gate), `check/sound` (frozen WAV digest —
  the sharpest gate: any drift in channel or mixer behavior lands in the
  waveform), Blargg passlist including `dmg_sound` with a wall-clock
  comparison (baseline 10m11), mooneye, acid2, battery, both app builds.

## Non-Goals

- No `Apu/Instruction.roc` / event tag union (see Why — nothing to
  transcribe, no exhaustiveness payoff; `handle_event`'s one-line arms stay
  fused with their actions and get *more* readable through the decoders).
- No behavior work: the batching strategy (`pending`, the 128-cycle flush),
  the sweep quirks, and the DMG/CGB power-off difference move verbatim.
- No PPU restructuring — if this lands well, the PPU's analogous slice
  (LCDC/OAM-attribute decoders) is a separate future change.
