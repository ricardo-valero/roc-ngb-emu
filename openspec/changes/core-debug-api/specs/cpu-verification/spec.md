# cpu-verification Delta Spec

## ADDED Requirements

### Requirement: Timing and halt conformance ladder
Beyond `cpu_instrs`, the suite SHALL run Blargg's `instr_timing` and `mem_timing` ROMs and the mooneye-gb halt and timer acceptance ROMs (halt_ime0_ei, halt_ime0_nointr_timing, halt_ime1_timing; tim00/01/10/11 and their div-trigger variants, div_write, rapid_toggle, tima_reload, tima_write_reloading, tma_write_reloading). ROMs are fetched by the existing fetch flow and remain untracked. ROMs that do not yet pass SHALL be reported informatively without gating, and promoted to gating as they pass; the passing set SHALL never shrink.

#### Scenario: Passing timing ROM gates
- **WHEN** the suite runs and a timing ROM previously recorded as passing is executed
- **THEN** a failure of that ROM fails the suite

#### Scenario: Not-yet-passing ROM reports
- **WHEN** the suite runs a ladder ROM not yet recorded as passing
- **THEN** its pass/fail status is reported without failing the suite
