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
Blargg's `dmg_sound` singles `01-registers`, `02-len ctr`, `03-trigger`, `04-sweep`, `05-sweep details`, `06-overflow on trigger`, `07-len sweep period sync`, `08-len ctr during power`, and `11-regs after power` SHALL pass and gate `check-blargg` via the passlist. The headless runner SHALL detect the memory-reporting protocol these ROMs use (signature `0xDE 0xB0 0x61` at `$A001` with a status byte at `$A000`, `0x80` while running) in addition to serial reporting. Singles `09`, `10`, and `12` SHALL remain informative: they measure the sub-instruction wave-RAM access window that the batched APU intentionally does not model.

#### Scenario: Gating singles pass
- **WHEN** `check-blargg` runs
- **THEN** the nine gating `dmg_sound` singles report PASS and any regression among them fails the check

#### Scenario: Cycle-exact singles stay informative
- **WHEN** `09-wave read while on` fails under the runner
- **THEN** the suite still exits 0, reporting the failure as informative

### Requirement: Frozen audio regression digest
A check app SHALL render a deterministic WAV from a fixed ROM and frame count and compare its digest against a golden value, failing on mismatch. The golden SHALL use compare-or-create semantics: when no golden exists, the check writes the digest and keeps the rendered WAV for one-time ear verification (blessing = delete and re-run); when the digest exists and the render mismatches, the actual WAV is kept for listening comparison. The remaining `dmg_sound` singles SHALL be runnable in an informative, non-gating report.

#### Scenario: Digest check
- **WHEN** the audio check app runs with the emulator unchanged and a golden present
- **THEN** it reports PASS against the frozen digest

#### Scenario: Blessing a golden
- **WHEN** the check runs with no golden digest present
- **THEN** it writes the digest, keeps the WAV for ear verification, and reports that a new golden was created rather than passing silently

#### Scenario: Mismatch is reviewable
- **WHEN** the check fails against an existing golden
- **THEN** the rendered WAV is kept so the difference can be heard

