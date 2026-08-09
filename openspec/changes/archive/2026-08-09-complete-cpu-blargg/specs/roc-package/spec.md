# roc-package Delta Specification

## MODIFIED Requirements

### Requirement: Inline tests pass with unchanged behavior
All inline `expect` tests SHALL pass under the pinned toolchain, and their assertions SHALL encode hardware-correct SM83 behavior. Legacy-branch behavior preservation no longer constrains test values: where pre-migration assertions conflict with documented hardware semantics (e.g. list-fold ALU flag results), the assertions SHALL be replaced with hardware-correct ones.

#### Scenario: Running the test suite
- **WHEN** `roc test` is run on the package modules
- **THEN** every inline `expect` passes

#### Scenario: Hardware correctness supersedes legacy parity
- **WHEN** a legacy assertion disagrees with documented SM83 flag behavior
- **THEN** the test asserts the documented hardware behavior, not the legacy value
