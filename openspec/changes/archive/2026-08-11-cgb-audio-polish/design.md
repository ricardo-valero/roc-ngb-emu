# Design: cgb-audio-polish

## Context

The APU is event-driven off the bus: `Mmu.apu_events` carries channel
triggers (0–3) and power-off (0xF0), drained by `Apu.tick` which batches up
to 128 cycles between real advances. Registers live in the bus with
read-back masks; the APU reads them via `read_raw` when it needs values.
The failing `dmg_sound` singles show the gaps: side effects that must happen
at write time, and quirks that depend on the frame-sequencer position at the
write.

## Goals / Non-Goals

- Goals: pass `02`, `03`, `05`, `07`, `08`, `11`; silence the audible
  wrongness in real games (stuck DACs, wrong note lengths); keep the batched
  architecture.
- Non-goals: sub-instruction accuracy (`09`, `10`, `12` — the wave-RAM
  access window). `cgb_sound` ROM acquisition stays future work; CGB
  behavior is implemented from documentation now.

## Decisions

### Address-coded events
`apu_events` entries become the low byte of the written register address
(0x10–0x25), replacing the 0–3 trigger codes; 0xF0 stays power-off and 0xF1
is added for power-on. The APU decodes channel and action from the address
and reads the written value via `read_raw` — events need no payload. Only
registers with write-time side effects are reported: NR10, NRx1, NRx2, NR30,
NRx4 (trigger is NRx4 with bit 7 set in the register).

### Old-value tracking lives in the APU
Quirks that need the previous register value (length-enable 0→1, negate
cleared after use) can't read it from the bus — the write already landed. The
channel record gains `len_en : Bool` (last seen length-enable) and
`sweep_neg_used : Bool` (a sweep calculation ran in negate mode since the
last trigger).

### Split tick: pending cycles advance before this step's events
Events pushed during the current CPU step describe a write that happened
*after* the previously batched cycles. `tick` becomes: advance by pending,
apply events, then batch this step's cycles. The frame-sequencer position a
quirk observes is then exact to within one instruction — enough for every
Blargg alignment except the four intentionally-informative singles.

### Batch flush at frame-sequencer steps
Blargg's `sync_apu` polls NR52 for the length-clock edge and budgets only
~290 cycles of slack; a 128-cycle batch makes the status byte up to a batch
late and blows that budget. The batch therefore also flushes when it would
cross an 8192-cycle sequencer boundary — one extra compare in `tick`, and
NR52 becomes accurate to within an instruction exactly where the tests
measure. This is also what makes `07-len sweep period sync` pass.

### First-half predicate
Lengths clock on even frame-sequencer steps. `fs_step` holds the last step
fired, so "the next step won't clock length" ⇔ `fs_step` is even (the
initial state, step 0 not yet fired, correctly reports true).

### Trigger stops reloading length from NRx1
Hardware loads the counter when NRx1 is written; trigger only reloads an
expired (zero) counter to max. Today's trigger re-reads NRx1 every time —
wrong whenever a game rewrites NRx1 mid-note and retriggers later.

### Power-off preserves DMG lengths in place
`handle_event` gets the model from the `Mmu` it already receives: on DMG the
blank-out keeps each channel's `length`; on CGB it clears everything. While
powered off on DMG, the bus accepts NRx1 writes storing only the length bits
(duty bits read back 0; read masks hide length bits anyway) and reports the
event so the APU reloads the counter.

## Risks / Trade-offs

- The sound-check golden may shift (length timing changes affect sample
  output). Protocol: if the digest mismatches, delete, re-render, verify by
  ear, commit the new digest.
- `11-regs after power` exercises read-back masks as much as power logic; if
  it still fails after the power rework, the fix iterates on the mask table,
  not this design.
- Address-coded events change the `apu_events` contract; all producers and
  the decoder move in one commit, and the existing Mmu/Apu expects that
  assert event codes are updated with them.
