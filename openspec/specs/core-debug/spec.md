# core-debug Specification

## Purpose

The core's debugging surface: run-until stepping with machine-readable stop reasons, program-counter breakpoints, and pure-function debug renders of PPU state — so any frontend (headless, windowed, future web debugger) can inspect the machine without privileged hooks.

## Requirements

### Requirement: Run-until stepping with reason codes
The core SHALL provide a `run_until` entry point that advances emulation and returns the machine together with a stop reason: frame completed, or breakpoint hit. `run_frame` SHALL remain available with unchanged frame-stepping behavior (return at VBlank entry with a complete framebuffer).

#### Scenario: Frame completion
- **WHEN** `run_until` is called with no breakpoint set
- **THEN** it returns at VBlank entry with a full framebuffer and the frame-completed reason

#### Scenario: Existing frame stepping unchanged
- **WHEN** `run_frame` is called as before this change
- **THEN** its observable behavior (cycles, framebuffer, serial output) is identical to `run_until` stopping for a completed frame

### Requirement: Program-counter breakpoint
The core SHALL accept a program-counter breakpoint; when execution reaches that address, `run_until` SHALL stop before completing the frame and report the breakpoint reason, leaving the machine resumable from exactly that state.

#### Scenario: Breakpoint hit
- **WHEN** a breakpoint is set at an address the ROM executes mid-frame
- **THEN** `run_until` returns with the breakpoint reason and the CPU's program counter equals the breakpoint address

#### Scenario: Resume after hit
- **WHEN** `run_until` is called again on a machine stopped at a breakpoint
- **THEN** execution continues past the breakpoint and eventually reports a completed frame

### Requirement: Debug renders are core functions
The core SHALL render the full 256×256 background map, the complete tile-data set, and the OAM sprites as pixel buffers via pure functions of the machine state, using the same rendering rules as the scanline PPU (palette, tile addressing modes). Hosts SHALL NOT need to interpret VRAM to display these views.

#### Scenario: Background map render
- **WHEN** the background-map render is requested on a machine whose PPU has drawn a frame
- **THEN** a 256×256 pixel buffer is returned in which the visible 160×144 viewport region matches the screen framebuffer content at the scroll position

#### Scenario: Tile data render
- **WHEN** the tile-data render is requested
- **THEN** a pixel buffer covering all tiles in VRAM is returned, laid out on a fixed grid

#### Scenario: OAM render
- **WHEN** the OAM render is requested
- **THEN** a pixel buffer showing each of the 40 sprite slots is returned, honoring sprite palettes and flips

### Requirement: Headless debug dump
The project SHALL provide a headless runner that executes a ROM for N frames and writes the debug renders (background map, tile data, OAM) as image files, so debug output is inspectable and testable without any windowed or web frontend.

#### Scenario: Dump produces images
- **WHEN** the debug runner executes a ROM for N frames with an output directory
- **THEN** valid image files for background map, tile data, and OAM are written with the documented dimensions

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
