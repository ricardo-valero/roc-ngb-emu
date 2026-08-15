# cpu-verification Delta Spec

## ADDED Requirements

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
