# apu-core Specification

## Purpose
TBD - created by archiving change apu-core. Update Purpose after archive.
## Requirements
### Requirement: Frame sequencer and channel lifetimes
The APU SHALL advance a 512 Hz frame sequencer from CPU-step cycles, clocking length counters at 256 Hz, volume envelopes at 64 Hz, and CH1 sweep at 128 Hz. A channel SHALL report active in NR52's low bits while its length counter permits and its DAC is enabled, and SHALL deactivate when its length expires with length-enable set.

#### Scenario: Length expiry silences a channel
- **WHEN** CH2 is triggered with a length of 1 and length-enable set, and the machine runs past the next 256 Hz length clock
- **THEN** NR52 bit 1 reads 0

#### Scenario: Envelope steps at 64 Hz
- **WHEN** CH2 is triggered with initial volume 15 and a decreasing envelope of period 1
- **THEN** after one envelope clock the channel volume is 14

### Requirement: Pulse channels
CH1 and CH2 SHALL produce pulse waves with the four duty ratios (12.5%, 25%, 50%, 75%) at frequency `131072 / (2048 - x)` Hz from the 11-bit register value, with envelope-controlled volume. CH1's sweep SHALL recompute frequency at 128 Hz per NR10 (period, negate, shift) and SHALL disable the channel when the computed frequency exceeds 2047.

#### Scenario: Duty pattern
- **WHEN** CH2 plays duty 2 (50%) at a fixed frequency
- **THEN** one duty cycle of the generated samples is high for 4 of 8 steps

#### Scenario: Sweep overflow disables
- **WHEN** CH1 is triggered with a frequency and sweep shift whose next computed value exceeds 2047
- **THEN** NR52 bit 0 reads 0 after the overflow check

### Requirement: Wave channel
CH3 SHALL play the 32 4-bit samples of wave RAM (`0xFF30`–`0xFF3F`) at frequency `65536 / (2048 - x)` Hz per full pattern, gated by the NR30 DAC bit, with output volume shifted per NR32 (mute, 100%, 50%, 25%).

#### Scenario: Wave RAM is audible
- **WHEN** wave RAM holds a known pattern and CH3 is triggered with volume 100%
- **THEN** the generated samples follow the pattern's nibble sequence

### Requirement: Noise channel
CH4 SHALL clock a 15-bit LFSR (XOR of the low two bits fed to bit 14, additionally to bit 6 in 7-bit mode) at the NR43-selected rate, outputting the inverted low bit scaled by the envelope volume.

#### Scenario: LFSR sequence
- **WHEN** the LFSR starts from the all-ones state and is clocked repeatedly
- **THEN** the first output bits match the documented 15-bit sequence

### Requirement: Mixing and sample output
The APU SHALL mix active channels through NR51 (per-channel left/right routing) and NR50 (master volume per side) into an interleaved stereo sample buffer at 48 kHz, generated via a fractional cycle accumulator, drainable by the embedder without loss.

#### Scenario: Panning routes channels
- **WHEN** CH2 is routed only left via NR51
- **THEN** generated left samples carry the pulse and right samples are silent

#### Scenario: One frame of samples
- **WHEN** the machine runs one video frame (70,224 cycles)
- **THEN** draining yields approximately 800 stereo sample pairs (48000 / 59.7)

### Requirement: Immediate register side effects
APU register writes SHALL take effect at write time, not at the next
trigger: writing NRx1 SHALL reload the channel's length counter (whether or
not the channel is enabled); writing NRx2 (or NR30 for CH3) with the DAC
bits clear SHALL disable the channel immediately; writing NR10 with the
negate bit clear SHALL disable CH1 if any sweep calculation has used negate
mode since the last trigger.

#### Scenario: Length reload while playing
- **WHEN** CH2 is playing and NR21 is written with a new length
- **THEN** the length counter continues from the newly written value

#### Scenario: DAC off silences immediately
- **WHEN** CH2 is playing and NR22 is written with its upper five bits zero
- **THEN** NR52 bit 1 reads 0 without waiting for a length clock

#### Scenario: Negate quirk
- **WHEN** CH1 triggers with sweep negate set, a sweep calculation runs, and
  NR10 is then written with negate clear
- **THEN** NR52 bit 0 reads 0

### Requirement: Length-counter edge clocking
When a length clock will not occur at the next frame-sequencer step: an NRx4
write that raises length-enable from 0 to 1 with a non-zero counter SHALL
clock the counter once (disabling the channel if it reaches zero without a
trigger), and a trigger that reloads an expired counter with length-enable
set SHALL reload it to maximum minus one. Register events SHALL observe the
frame-sequencer position as of the write, with pending batched cycles
applied first.

#### Scenario: Enabling in the first half clocks once
- **WHEN** CH2 has a non-zero length counter, the next frame-sequencer step
  is a non-length step, and NR24 is written enabling length without a trigger
- **THEN** the counter decrements once immediately

#### Scenario: Trigger reload in the first half
- **WHEN** CH2's length counter is 0, the next frame-sequencer step is a
  non-length step, and NR24 is written with trigger and length-enable set
- **THEN** the counter reloads to 63 rather than 64

### Requirement: Model-aware power behavior
Powering the APU off SHALL clear the register file and channel state, except
that on DMG the length counters SHALL survive; on DMG, NRx1 length writes
SHALL still load the counters while powered off (the stored duty bits
reading back zero). On CGB, power-off SHALL clear length counters and reject
all register writes. Powering on SHALL reset the frame sequencer so its next
step is step 0 and SHALL reset the pulse duty and wave positions.

#### Scenario: DMG lengths survive power off
- **WHEN** CH2 has a running length counter, the APU is powered off then on,
  and CH2 is retriggered with length enabled
- **THEN** the counter continues from its preserved value

#### Scenario: CGB power-off clears lengths
- **WHEN** the same sequence runs on a CGB machine
- **THEN** the retriggered counter starts from a full reload

#### Scenario: Frame sequencer restarts on power-on
- **WHEN** the APU is powered off then on
- **THEN** the next frame-sequencer step to fire is step 0
