# Design: Complete CPU + Blargg Verification

## Context

The package has partial SM83 decode into a nominal `Instruction` ADT (`package/Cpu/Instruction.roc`), a small ALU (`package/Cpu/Alu.roc`) whose list-fold `add`/`sub` API cannot express ADC/SBC carry-in flag semantics, register/status types, and a cartridge-header parser. There is no execute step, no memory, no interrupts, and nothing that runs a ROM. Toolchain is the new Zig-based Roc compiler pinned via the Nix flake; a known nightly limitation forces structural copies of nested types in subdirectory imports.

Inspiration target is elmboy (playable DMG emulator in a pure functional language). Unlike Elm, Roc's unique-reference optimization allows in-place mutation of a linearly-owned `List(U8)`, so the pure `step : GameBoy -> GameBoy` style need not pay Elm's persistent-data-structure tax.

## Goals / Non-Goals

**Goals:**
- Hardware-correct SM83 CPU: full base + CB opcode decode, correct flag semantics, machine-cycle accounting, interrupts.
- A flat memory bus sufficient for CPU test ROMs (ROM, WRAM, HRAM, IO stubs, serial capture, minimal timer).
- Headless pass/fail verification against Blargg's `cpu_instrs` individual test ROMs, runnable in CI.
- De-risk the future rendering milestone with a roc-ray hello-window spike (isolated; no package impact).

**Non-Goals:**
- PPU, APU, joypad, real-time frame pacing (later changes; roc-ray integration beyond the spike).
- MBC bank switching — avoided by running the individual `cpu_instrs` ROMs, which fit in the flat 32 KiB ROM space, instead of the combined MBC1 ROM.
- Cycle-accuracy beyond machine-cycle instruction costs (no sub-instruction memory timing); Game Boy Color; save states.
- Performance tuning. Correctness first; benchmark only after Blargg passes.

## Decisions

### D1: Keep the ADT decode → interpret split
Decode `U8 -> Instruction` stays a pure table; a separate `execute` interprets the ADT against machine state. Alternative (elmboy's): opcode → fused closure table, faster but opaque. The ADT is testable in isolation, self-documenting, and the double dispatch is acceptable until proven otherwise (revisit after Blargg passes, with measurements).

### D2: Arithmetic decode for regular opcode blocks
All 256 CB opcodes decode from bit fields (bits 7–6 op family, 5–3 bit-index/sub-op, 2–0 operand in the fixed order B,C,D,E,H,L,(HL),A) — no 256-arm match. Base-table ALU blocks (0x80–0xBF) and the load quadrant (0x40–0x7F) use the same operand-order arithmetic. Irregular regions (0x00–0x3F, 0xC0–0xFF) remain explicit match arms.

### D3: ALU takes explicit operands and carry-in
`add : U8, U8 -> Output`, `adc : U8, U8, Bool -> Output`, same pattern for `sub`/`sbc`. Half-carry computed on the 3-way nibble sum including carry-in. The existing `List(U8)`-fold API is deleted (its inline expects are replaced with hardware-correct cases, including carry-in-tips-the-nibble edge cases). DAA implemented from N/H/C flags per the canonical table; rotates/shifts/SWAP/BIT/SET/RES added; `ADD SP,e8`-style ops set Z=0, N=0 and compute H/C from unsigned low-byte arithmetic.

### D4: Cycles are reported by execute, not stored beside decode
Conditional instructions cost different cycles taken vs. not-taken, and only execute knows the branch outcome. So `step` returns `(GameBoy, cycles)` with execute as the single source of truth; the decode table stays purely structural. Alternative (cycles in the table plus a "penalty" field) spreads the same fact across two places.

### D5: Machine state is one flat record; memory is one `List(U8)`
`GameBoy : { a, f, b, c, d, e, h, l : U8, pc, sp : U16, mem : List(U8), ime : Bool, halted : Bool, ei_pending : Bool, serial_out : List(U8), timer counters }` (shape indicative). A single 64 KiB list, owned linearly through `step`, lets the unique-reference optimization mutate in place — the design constraint is *never alias `mem`* (no captured references; reads return copies of bytes). Region semantics (ROM read-only, IO side effects) live in `read`/`write` functions that dispatch on address, not in separate structures. Alternative (per-region records) reads nicer but multiplies plumbing and risks accidental copies.

### D6: Serial capture and minimal timer live behind the bus
Writes to SC (0xFF02) with bit 7 set append the SB byte (0xFF01) to `serial_out` — that is the whole Blargg reporting channel. A minimal timer (DIV 0xFF04, TIMA 0xFF05, TMA 0xFF06, TAC 0xFF07) advances from the cycles returned by `step` and raises the timer interrupt on TIMA overflow — required because `cpu_instrs` test 02 exercises EI/HALT via the timer interrupt.

### D7: Interrupt semantics, standard model first
IME with `EI` taking effect after the following instruction (`ei_pending`), `DI` immediate, `RETI` atomic, HALT wakes on pending interrupt (IE&IF≠0) regardless of IME, dispatch costs 20 cycles (push PC, jump to vector, clear IF bit, clear IME). The HALT bug (IME=0 with pending interrupt → PC fails to increment) is implemented only if a Blargg subtest demands it; noted as an open question rather than gold-plated upfront.

### D8: Verification harness = individual ROMs, serial-string oracle
A basic-cli example loads one ROM, steps the machine with a cycle budget (timeout guard), and scans `serial_out` for `Passed`/`Failed`, exiting 0/1. ROMs are fetched by a script into an untracked `roms/` directory (Blargg ROMs are freely redistributable, but keeping binaries out of git keeps the repo clean). A wrapper script runs all 11 individual ROMs for CI.

### D9: roc-ray spike is quarantined
Lives outside `package/` (its own directory with its own app header), pinned to a roc-ray release whose required nightly matches — or explicitly updates — the flake pin. Deliverable is a recorded working (roc-ray release, roc nightly) pair and a window on screen; any flake change is a visible, deliberate commit.

## Risks / Trade-offs

- [Nightly compiler bugs beyond the known nested-import issue] → Keep the structural-copy workaround pattern; isolate any new workaround with a comment and a tracking note; the flake pin means breakage arrives only when we choose to bump.
- [Interpreter too slow to finish Blargg ROMs in reasonable wall time] → Correctness milestone tolerates slow runs (minutes); performance work is explicitly deferred with D1 revisit as the lever.
- [Accidental `mem` aliasing silently degrades to copy-per-step] → Convention: only `read`/`write` touch `mem`; no closures capture it. If runs are pathologically slow, this is the first suspect.
- [EI-delay / HALT-bug subtleties fail specific subtests] → Implement the standard model (D7), then iterate per failing subtest — Blargg names the failing case on the serial port, which localizes the fix.
- [roc-ray release ↔ nightly mismatch with our flake] → Treat the pair as pinned-together; if no release matches our nightly, the spike outcome is "bump flake to X" as a proposal, not a silent change.

## Open Questions

- Does any `cpu_instrs` subtest require the HALT bug or finer EI timing than the one-instruction delay? (Resolve empirically at task "all 11 ROMs pass".)
- STOP (0x10): treated as NOP-equivalent for now — sufficient for `cpu_instrs`?
- Whether the nightly's `match` on computed ranges makes arithmetic decode (D2) as clean as intended, or bit-mask guards are needed instead.
