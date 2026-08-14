# core-debug Delta Spec

## MODIFIED Requirements

### Requirement: Run-until stepping with reason codes
The core SHALL provide a `run_until` entry point that advances emulation
and returns the machine together with a stop reason: frame completed, or
breakpoint hit. `run_frame` SHALL remain available with unchanged
frame-stepping behavior (return at VBlank entry with a complete
framebuffer). Both entry points SHALL take a per-frame input record
containing the button states and the wall-clock time `now` (seconds,
UNIX epoch); the core SHALL treat `now` as data — pure state in, state
out — and use it only for battery/RTC behavior, so callers that pass a
constant `now` get bit-identical emulation across runs.

#### Scenario: Frame completion
- **WHEN** `run_until` is called with no breakpoint set
- **THEN** it returns at VBlank entry with a full framebuffer and the
  frame-completed reason

#### Scenario: Existing frame stepping unchanged
- **WHEN** `run_frame` is called with the widened input record on a
  non-RTC cartridge
- **THEN** its observable behavior (cycles, framebuffer, serial output)
  is identical regardless of the `now` values supplied

#### Scenario: Determinism under fixed time
- **WHEN** the same ROM is run twice with identical button sequences and
  a constant `now`
- **THEN** the resulting machines are observably identical (framebuffer,
  serial, battery bytes)
