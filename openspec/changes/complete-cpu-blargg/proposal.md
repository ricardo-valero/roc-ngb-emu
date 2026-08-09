# Complete CPU + Blargg Verification

## Why

The package currently decodes a fraction of the SM83 instruction set into an ADT and stops there: there is no execute step, no memory bus, and the ALU's multi-operand `add`/`sub` API is structurally unable to model ADC/SBC half-carry correctly (carry-in participates in the nibble arithmetic; a summed fold loses it). Nothing can run a ROM. Completing the CPU against a hard, binary pass/fail oracle — Blargg's `cpu_instrs` test ROMs, which report results over the serial port and need no graphics — is the foundation every later component (PPU, timer scheduling, joypad, APU) builds on.

## What Changes

- Rewrite the ALU with explicit carry-in operations (`add`/`adc`/`sub`/`sbc` as two-operand + carry forms), plus the missing operations: DAA, rotates/shifts (RLC/RRC/RL/RR/SLA/SRA/SRL/SWAP), bit ops (BIT/SET/RES), and the `ADD SP, e8` / `LD HL, SP+e8` flag semantics (Z=0, N=0, H/C from low-byte arithmetic). **BREAKING**: existing `Alu.add`/`Alu.sub` list-based signatures and their inline `expect` values are replaced with hardware-correct ones.
- Complete instruction decode: all 256 base opcodes plus the 256 CB-prefixed opcodes, keeping the ADT approach. CB opcodes and regular base-table blocks are decoded arithmetically from bit fields rather than exhaustive match arms where structure allows.
- Attach machine-cycle counts to every instruction, including dual counts (taken / not-taken) for conditional jumps, calls, and returns.
- Add an execute/step function: `step : GameBoy -> (GameBoy, Cycles)` — fetch, decode, execute, flag/register/memory writeback — over a machine-state record designed so Roc's unique-reference in-place mutation applies to the memory list.
- Add a flat memory bus (MMU): 64 KiB address space with ROM loading from cartridge bytes, work RAM, HRAM, IO-register stubs, and serial-port capture (SB/SC registers) sufficient for Blargg output.
- Implement interrupts: IME, IE/IF registers, EI/DI/RETI/HALT semantics, and the five interrupt vectors with correct dispatch cost.
- Add a headless CLI runner (`examples/`) that loads a Blargg `cpu_instrs` ROM, runs the machine, captures serial output, and exits pass/fail — usable in CI.
- Independent spike (no package impact): confirm a roc-ray hello-window builds and runs against a Roc nightly version-aligned with our Nix flake pin; record the working (roc-ray release, nightly) pair.

## Capabilities

### New Capabilities

- `cpu-core`: SM83 CPU emulation — ALU with hardware-correct flag semantics, full base + CB instruction decode with cycle counts, execute/step semantics, and interrupt handling.
- `memory-bus`: Flat 64 KiB address space — ROM mapping, RAM regions, IO-register access including serial-port capture; the read/write interface the CPU (and later components) target.
- `cpu-verification`: Headless Blargg `cpu_instrs` harness — running a test ROM to completion and judging pass/fail from captured serial output.

### Modified Capabilities

- `roc-package`: The "Inline tests pass with unchanged behavior" requirement is migration-scoped (diff-vs-legacy) and is superseded: inline tests SHALL assert hardware-correct behavior, and legacy-preservation no longer constrains ALU semantics.

## Impact

- **Code**: `package/Cpu/Alu.roc` rewritten; `package/Cpu/Instruction.roc` completed and extended with cycle counts; new modules for CPU step/state, MMU, and interrupts; new `examples/` headless runner. `package/Cartridge/Header.roc` reused for ROM loading.
- **Tests**: ALU `expect` values change to hardware-correct ones; substantial new inline `expect` coverage; Blargg ROMs become the integration oracle (ROM files fetched locally, not committed).
- **Dependencies**: none new for the core. The roc-ray spike may adjust the flake's pinned nightly if version-pairing requires it (flagged, not silently changed).
- **Known workaround carried forward**: the structural-copy workaround for nested-type imports in `Instruction.roc` remains until the compiler supports qualified nested types through subdirectory imports.
