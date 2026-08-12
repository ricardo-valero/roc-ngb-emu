# Proposal: cgb-audio-polish

## Why

CGB games now run but their audio is audibly wrong — increment 4 of the CGB
arc. The `check-blargg` informative `dmg_sound` singles name the defects
precisely: the APU applies register writes lazily (length only loads at
trigger, a DAC turned off mid-note keeps sounding), misses the length-counter
edge-clocking rules, and treats power-off as a full reset when the DMG
preserves length counters. These are the same quirks games lean on for
envelope and note-length effects, so fixing them is both conformance and the
audible fix.

## What Changes

- APU register events widen from "trigger + power off" to the register file:
  the bus reports writes to NR10, NRx1, NRx2/NR30, and NRx4 (and power on),
  and the APU applies their side effects at write time.
- Length counters load when NRx1 is written (any time), not at trigger;
  trigger only reloads an expired counter to max.
- Turning a DAC off (NRx2/NR30) silences its channel immediately.
- Length-counter edge clocking: enabling length in the first half of the
  length period clocks it once; a trigger that reloads an expired counter in
  that window reloads to max−1.
- Sweep negate quirk: clearing the negate bit after a negate-mode calculation
  disables CH1.
- Power behavior split by model: DMG preserves length counters through
  power-off and accepts length writes while off; CGB clears them. Power-on
  resets the frame sequencer and duty positions.
- Event timing: the APU advances pending cycles *before* applying this step's
  register events, so quirks see the frame-sequencer position at the write.
- The APU batch flushes when it crosses a frame-sequencer step, so NR52
  status updates within an instruction of a length clock (the sync method
  every timing-sensitive Blargg test builds on).
- `dmg_sound` singles `02-len ctr`, `03-trigger`, `05-sweep details`,
  `07-len sweep period sync`, `08-len ctr during power`, `11-regs after
  power` promote from informative to gating in `check/blargg/passlist`.
  `09`, `10`, `12` stay informative: they require the sub-instruction
  wave-RAM access window that the batched design intentionally does not
  model.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `apu-core`: register writes gain immediate side effects (length reload,
  DAC gating, sweep negate disable, edge clocking); power off/on becomes
  model-aware.
- `apu-verification`: the gating `dmg_sound` set grows by five singles; the
  four cycle-exact singles are documented as intentionally informative.

## Impact

- `package/Mmu.roc`: APU event generation (address-coded events, power-on
  event, DMG length writes while powered off).
- `package/Apu.roc`: channel state (`len_en`, `sweep_neg_used`), event
  decoding, trigger rework, power handling, tick restructure.
- `check/blargg/passlist`: five promotions.
- `check/sound` golden: sample output may shift (length reload timing);
  re-bless with ear verification if the digest changes.
