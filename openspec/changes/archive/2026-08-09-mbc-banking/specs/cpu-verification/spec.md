# cpu-verification Delta Specification

## ADDED Requirements

### Requirement: Combined Blargg ROM passes
The combined `cpu_instrs.gb` (a 64 KiB MBC1 ROM that bank-switches between subtests) SHALL pass through the headless runner, and the suite script SHALL include it alongside the 11 individual ROMs.

#### Scenario: Combined ROM in the suite
- **WHEN** the Blargg suite runs
- **THEN** twelve results are reported and the combined ROM's serial output contains `Passed`
