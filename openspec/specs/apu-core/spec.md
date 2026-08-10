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

