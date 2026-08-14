# cartridge-banking Delta Spec

## ADDED Requirements

### Requirement: MBC3 real-time clock
On MBC3 cartridges whose header declares RTC hardware, writing `0x08`
through `0x0C` to the `0x4000` region SHALL map the corresponding clock
register (seconds, minutes, hours, day low, day high/carry/halt) into
`0xA000`–`0xBFFF` in place of a RAM bank, gated by the same RAM-enable
as cartridge RAM. Reads SHALL return the **latched** register values;
writes SHALL set the running clock (and the halt flag via day-high
bit 6). Writing `0x00` then `0x01` to `0x6000`–`0x7FFF` SHALL latch the
running clock into the readable registers atomically. The running clock
SHALL track the wall-clock `now` supplied to the core each frame, with
one-second resolution; when the halt flag is set the clock SHALL not
advance. The day counter SHALL be 9 bits with an overflow carry bit that
stays set until cleared by a write.

#### Scenario: Latch protocol
- **WHEN** the game writes `0x00` then `0x01` to `0x6000` and reads the
  seconds register via `0x4000`-region select `0x08`
- **THEN** it reads the clock value captured at the latch, and re-reads
  without a new latch return the same value even as `now` advances

#### Scenario: Clock advances with wall time
- **WHEN** frames are run with `now` values 61 seconds apart and the
  clock is latched after each
- **THEN** the minutes register has advanced by 1 and the seconds
  register by 1

#### Scenario: Halt freezes the clock
- **WHEN** the game sets the halt bit, frames run with `now` advancing,
  and the clock is latched
- **THEN** the clock registers equal their values from when halt was set

#### Scenario: RTC selection does not disturb RAM
- **WHEN** the game writes to RAM bank 0, selects clock register `0x08`,
  then re-selects RAM bank 0
- **THEN** the RAM contents read back unchanged
