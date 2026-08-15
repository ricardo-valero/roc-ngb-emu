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

### Requirement: Combined Blargg ROM passes
The combined `cpu_instrs.gb` (a 64 KiB MBC1 ROM that bank-switches between subtests) SHALL pass through the headless runner, and the suite script SHALL include it alongside the 11 individual ROMs.

#### Scenario: Combined ROM in the suite
- **WHEN** the Blargg suite runs
- **THEN** twelve results are reported and the combined ROM's serial output contains `Passed`

### Requirement: Timing and halt conformance ladder
Beyond `cpu_instrs`, the suite SHALL run Blargg's `instr_timing` and `mem_timing` ROMs and the mooneye-gb halt and timer acceptance ROMs (halt_ime0_ei, halt_ime0_nointr_timing, halt_ime1_timing; tim00/01/10/11 and their div-trigger variants, div_write, rapid_toggle, tima_reload, tima_write_reloading, tma_write_reloading). ROMs are fetched by the existing fetch flow and remain untracked. ROMs that do not yet pass SHALL be reported informatively without gating, and promoted to gating as they pass; the passing set SHALL never shrink.

#### Scenario: Passing timing ROM gates
- **WHEN** the suite runs and a timing ROM previously recorded as passing is executed
- **THEN** a failure of that ROM fails the suite

#### Scenario: Not-yet-passing ROM reports
- **WHEN** the suite runs a ladder ROM not yet recorded as passing
- **THEN** its pass/fail status is reported without failing the suite


### Requirement: SM83 single-step vector suite
The project SHALL provide a pure-Roc fetch app that downloads the
SingleStepTests SM83 vectors (one JSON file per opcode, base and
CB-prefixed) into an untracked data directory, skipping files already
present so an interrupted fetch resumes, and a pure-Roc runner that
executes every case of the given files through the core — initial CPU
registers and flat memory in, one instruction stepped — and diffs the
result field-by-field against the expected final state: each register,
the interrupt-master-enable flag, every touched memory address, and the
total cycle count. All fetched opcode files SHALL pass and gate — any
failing case exits nonzero, so the passing set never shrinks — except
files excluded in the runner with a written reason (repo precedent:
blargg passlist exclusions), which are reported and never gate.

#### Scenario: Passing opcode file
- **WHEN** the runner executes an opcode file whose cases all match
- **THEN** it reports the file as ok with its case count and exits 0

#### Scenario: Failing case names the field
- **WHEN** a case's outcome differs from the expected final state
- **THEN** the runner exits nonzero and reports the file, case index,
  and each mismatched field with got/want values

#### Scenario: Cycle undercount caught
- **WHEN** an instruction executes with correct final state but a cycle
  count differing from the vector's bus-activity length
- **THEN** the case fails with a cycles mismatch

#### Scenario: Interrupted fetch resumes
- **WHEN** the fetch app runs again after a partial download
- **THEN** already-present files are skipped and only missing files are
  fetched

#### Scenario: Excluded file reports its reason
- **WHEN** the runner is given a vector file recorded as excluded
- **THEN** it prints the exclusion and its written reason, does not
  execute the file's cases, and does not fail the run

### Requirement: Memory-access placement comparison
For each case, the runner SHALL compare the core's recorded bus
activity against the vector's per-M-cycle activity: the sequence of
memory accesses in order, each with address, value, and read/write
direction. A case whose final state matches but whose access sequence
differs SHALL fail.

#### Scenario: Misplaced access caught
- **WHEN** an instruction reaches the correct final state but performs
  a memory access in a different M-cycle order, at a different address,
  or with a different direction than the vector records
- **THEN** the case fails and the report shows the first diverging
  access, got and want
