# apu-verification Specification

## Purpose
TBD - created by archiving change apu-core. Update Purpose after archive.
## Requirements
### Requirement: Headless WAV capture
The project SHALL provide a headless runner that executes a ROM for N frames and writes the generated samples as a 16-bit PCM stereo WAV file at 48 kHz.

#### Scenario: WAV dump
- **WHEN** the runner executes a sound-producing ROM for N frames with an output path
- **THEN** a valid RIFF/WAVE file is written whose duration matches N frames of emulated time

### Requirement: Register-conformance oracle
Blargg's `dmg_sound` `01-registers` ROM SHALL pass. The headless runner SHALL detect the memory-reporting protocol these ROMs use (signature `0xDE 0xB0 0x61` at `$A001` with a status byte at `$A000`, `0x80` while running) in addition to serial reporting.

#### Scenario: Register test passes
- **WHEN** `01-registers` runs to completion under the runner
- **THEN** the `$A000` status byte is 0 and the process exits 0

### Requirement: Frozen audio regression digest
A check app SHALL render a deterministic WAV from a fixed ROM and frame count and compare its digest against a value frozen after one-time ear verification, failing on mismatch. The remaining `dmg_sound` singles SHALL be runnable in an informative, non-gating report.

#### Scenario: Digest check
- **WHEN** the audio check app runs with the emulator unchanged
- **THEN** it reports PASS against the frozen digest

