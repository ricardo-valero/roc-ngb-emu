# battery-persistence Delta Spec

## Purpose

The core's battery surface: extraction and injection of battery-backed
cartridge state (cart RAM and MBC3 RTC) as `.sav`-compatible bytes, so
frontends can persist game saves across sessions in the format every
established emulator and flashcart reads.

## ADDED Requirements

### Requirement: Battery extraction
The core SHALL expose the battery-backed state of the loaded cartridge as
bytes: the cartridge RAM, sized exactly to the header-declared RAM size
(not the internal allocation). For a cartridge whose header declares no
RAM and no RTC, the result SHALL be empty. For an MBC3 RTC cartridge, a
48-byte RTC footer SHALL be appended after the RAM bytes: little-endian
32-bit fields holding the running clock registers (seconds, minutes,
hours, day low, day high/carry/halt), the latched copies, and a 64-bit
UNIX timestamp of when the state was produced.

#### Scenario: RAM masked to header size
- **WHEN** a cartridge declaring 8 KiB of RAM has been written at
  `0xA000` and the battery bytes are extracted
- **THEN** the result is exactly 8192 bytes and contains the written
  values at their RAM offsets

#### Scenario: No battery hardware
- **WHEN** battery bytes are extracted from a ROM-only cartridge
- **THEN** the result is empty

#### Scenario: RTC footer appended
- **WHEN** battery bytes are extracted from an MBC3 RTC cartridge with
  32 KiB of RAM
- **THEN** the result is 32768 + 48 bytes and the footer's timestamp
  field equals the `now` most recently supplied to the core

### Requirement: Battery injection
The core SHALL accept battery bytes at load and restore cart RAM and RTC
state from them. It SHALL accept all three circulating layouts: bare RAM
(no footer), RAM + 44-byte footer (32-bit timestamp), and RAM + 48-byte
footer (64-bit timestamp). A RAM payload smaller than the header-declared
size SHALL be zero-padded; a larger one SHALL be truncated; injection
SHALL never fail on size grounds.

#### Scenario: Round trip is identity
- **WHEN** battery bytes are extracted, injected into a freshly loaded
  copy of the same ROM, and extracted again with the same `now`
- **THEN** the two extractions are byte-identical

#### Scenario: Legacy 44-byte footer accepted
- **WHEN** a save with a 44-byte RTC footer is injected
- **THEN** the clock registers and timestamp are restored, and the next
  extraction writes the 48-byte footer form

### Requirement: RTC catch-up on load
When battery bytes with an RTC footer are injected, the core SHALL
advance the running clock by the elapsed wall time — the difference
between the current `now` and the footer's timestamp — including day
rollover into the day-counter and its carry bit, unless the halt flag is
set in the saved state (a halted clock SHALL not advance).

#### Scenario: Elapsed time advances the clock
- **WHEN** a save whose footer timestamp is 90 seconds in the past is
  injected and the clock registers are then latched and read
- **THEN** the clock reads 1 minute 30 seconds ahead of the saved
  register values

#### Scenario: Halted clock stays put
- **WHEN** a save with the RTC halt flag set and an old timestamp is
  injected, latched, and read
- **THEN** the clock registers equal the saved values exactly
