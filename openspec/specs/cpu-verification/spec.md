# cpu-verification Specification

## Purpose
TBD - created by archiving change complete-cpu-blargg. Update Purpose after archive.
## Requirements
### Requirement: Headless Blargg runner
The project SHALL provide a headless command-line runner that loads a Game Boy ROM, executes the machine within a bounded cycle budget, and exits with status 0 when the captured serial output contains `Passed`, and a nonzero status when it contains `Failed` or the budget is exhausted.

#### Scenario: Passing ROM
- **WHEN** the runner executes a Blargg `cpu_instrs` individual ROM that the emulator handles correctly
- **THEN** serial output contains `Passed` and the process exits 0

#### Scenario: Failing ROM
- **WHEN** the runner executes a ROM whose serial output reports `Failed`
- **THEN** the process exits nonzero and prints the captured serial output

#### Scenario: Hang guard
- **WHEN** the cycle budget is exhausted with neither verdict on the serial port
- **THEN** the process exits nonzero and prints the captured serial output

### Requirement: Full individual-ROM suite passes
All eleven Blargg `cpu_instrs` individual test ROMs SHALL pass through the headless runner, and a script SHALL run the whole suite (fetching ROMs into an untracked local directory when absent) reporting per-ROM results.

#### Scenario: Suite run
- **WHEN** the suite script is executed with the toolchain devshell available
- **THEN** each of the eleven ROMs is run and reported, and the script exits 0 only if all pass

