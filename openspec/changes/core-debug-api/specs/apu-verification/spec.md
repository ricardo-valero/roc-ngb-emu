# apu-verification Delta Spec

## MODIFIED Requirements

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
