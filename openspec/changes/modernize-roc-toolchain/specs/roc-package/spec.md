# roc-package

## ADDED Requirements

### Requirement: Sources compile under the new compiler
All Roc source files (`package/**/*.roc` and `examples/*.roc`) SHALL use the new-compiler syntax and pass `roc check` under the pinned toolchain from the devshell.

#### Scenario: Checking the package
- **WHEN** `roc check` is run against the package entry point and each module
- **THEN** it completes with no errors

#### Scenario: Checking the example
- **WHEN** `roc check examples/cartridge.roc` is run
- **THEN** it completes with no errors

### Requirement: Inline tests pass with unchanged behavior
All pre-existing inline `expect` tests SHALL be preserved through the migration with their meaning intact (values and assertions unchanged; only syntax updated) and SHALL pass under the new toolchain.

#### Scenario: Running the test suite
- **WHEN** `roc test` is run on the package modules
- **THEN** every inline `expect` passes

#### Scenario: Behavior preservation
- **WHEN** the migrated code is diffed against `legacy`
- **THEN** differences are limited to syntax (lambdas, casing, operators, match/headers), not logic, constants, or test expectations
