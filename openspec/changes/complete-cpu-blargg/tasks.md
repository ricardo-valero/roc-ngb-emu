## 1. ALU rewrite (hardware-correct flags)

- [x] 1.1 Replace list-fold `add`/`sub` with two-operand `add`/`sub`/`compare` and explicit carry-in `adc`/`sbc`; update inline expects to hardware-correct values including carry-in edge cases (D3)
- [x] 1.2 Implement `inc`/`dec` review, 16-bit `add16`, and `ADD SP, e8`-style signed-immediate op with Z=0, N=0, low-byte H/C
- [x] 1.3 Implement DAA from N/H/C flags with inline expects covering add-path, sub-path, and carry cases
- [x] 1.4 Implement rotate/shift family (RLC, RRC, RL, RR, SLA, SRA, SRL, SWAP) and bit ops (BIT, SET, RES) with inline expects

## 2. Instruction decode completion

- [x] 2.1 Extend the `Instruction` ADT to cover the full base set (resolve the TODO variants: `AddSPSignedImmediate`, `LdHLSPPlusSignedImmediate`, `LdSPHL`, `JpIndirectHL`) and add CB instruction forms
- [x] 2.2 Implement arithmetic decode for the load quadrant (0x40–0x7F) and ALU block (0x80–0xBF) using the B,C,D,E,H,L,(HL),A operand order (D2)
- [x] 2.3 Complete explicit match arms for the irregular regions (0x00–0x3F, 0xC0–0xFF), mapping the 11 illegal opcodes to `Illegal`
- [x] 2.4 Implement CB-prefixed decode arithmetically from bit fields (op family / bit index / operand)
- [x] 2.5 Add exhaustive decode expects: no base or CB opcode yields `Unknown`; exactly the 11 illegal opcodes yield `Illegal`

## 3. Memory bus

- [x] 3.1 Create the MMU module: 64 KiB `List(U8)` with `read`/`write` dispatching region semantics (ROM read-only, WRAM, HRAM, IE/IF, IO stubs) per D5; only `read`/`write` touch `mem`
- [x] 3.2 ROM loading from cartridge bytes with DMG post-boot register/IO state (PC=0x0100, documented IO defaults)
- [x] 3.3 Serial capture: SC bit-7 write appends SB byte to `serial_out` log, with expects
- [x] 3.4 Minimal timer: DIV/TIMA/TMA/TAC advanced from step cycles; TIMA overflow reloads TMA and sets IF timer bit, with expects

## 4. CPU step and interrupts

- [x] 4.1 Define the `GameBoy` state record (registers, mem, IME, halted, ei_pending, serial_out, timer counters) per D5
- [x] 4.2 Implement `step : GameBoy -> (GameBoy, Cycles)` — fetch/decode/execute with writeback for loads, ALU ops, stack ops, jumps/calls/returns/restarts; execute reports taken/not-taken cycle costs (D4)
- [x] 4.3 Implement HALT, STOP-as-NOP, EI/DI with one-instruction EI delay, RETI
- [x] 4.4 Implement interrupt dispatch: IE&IF check, 20-cycle dispatch, IF-bit and IME clearing, vectors 0x40–0x60; HALT wake without IME
- [x] 4.5 Inline expects for step semantics: LD immediate, conditional JR cycle split, interrupt dispatch, EI delay, HALT wake

## 5. Blargg verification harness

- [x] 5.1 Fetch script for Blargg `cpu_instrs` individual ROMs into untracked `roms/` (gitignored)
- [x] 5.2 Headless runner example: load ROM, run with cycle budget, scan serial log for `Passed`/`Failed`, exit 0/1, print serial output
- [x] 5.3 Suite script running all 11 individual ROMs with per-ROM reporting
- [x] 5.4 Iterate on failing subtests until all 11 ROMs pass (resolve HALT-bug / EI-timing open questions empirically as surfaced)

## 6. roc-ray spike (independent)

- [x] 6.1 Identify the roc-ray release whose required Roc nightly aligns with (or nearest to) our flake pin; record the pair
- [x] 6.2 Build and run a hello-window app in an isolated directory; document outcome and any required flake bump as a note (no silent flake changes)

## 7. Wrap-up

- [x] 7.1 `roc check` and `roc test` green across package and examples; update README with Blargg run instructions
- [x] 7.2 Verify all delta-spec scenarios have corresponding passing tests or runner behavior; mark change ready for archive
