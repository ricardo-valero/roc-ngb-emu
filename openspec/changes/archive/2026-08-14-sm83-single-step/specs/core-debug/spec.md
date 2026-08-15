# core-debug Delta Spec

## ADDED Requirements

### Requirement: Single-step harness surface
The core SHALL expose a test surface that constructs machine state from
raw values — every CPU register, the interrupt-master-enable flag, and
an arbitrary flat 64 KiB memory image with no Game Boy address mapping —
executes exactly one instruction, and returns the resulting registers,
interrupt-master-enable flag, elapsed cycles, and memory for readback.

#### Scenario: Injected state round-trips
- **WHEN** state is constructed from raw register values and memory
  bytes and read back without stepping
- **THEN** every register, flag, and memory byte reads back exactly as
  injected

#### Scenario: Flat memory has no mapping
- **WHEN** the stepped instruction writes to an address that the real
  memory map would remap, mirror, or protect (e.g. ROM, echo RAM, I/O)
- **THEN** the byte is stored at that address verbatim and reads back

#### Scenario: One instruction exactly
- **WHEN** the surface steps injected state
- **THEN** exactly one instruction executes — no interrupt dispatch,
  timer, PPU, or APU side effects beyond that instruction's own

### Requirement: Bus access trace
The core SHALL offer an optional per-access memory trace: when enabled,
every memory read and write performed during stepping is recorded in
execution order as address, value, and direction; when disabled (the
default), behavior and outputs are unchanged from today.

#### Scenario: Trace records ordered accesses
- **WHEN** an instruction that performs multiple memory accesses is
  stepped with the trace enabled
- **THEN** the trace lists those accesses in the order performed, each
  with address, value, and read/write direction

#### Scenario: Disabled trace changes nothing
- **WHEN** the core runs with the trace disabled
- **THEN** emulation output is identical to the pre-trace core
