# apu-core Delta Spec

## ADDED Requirements

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
