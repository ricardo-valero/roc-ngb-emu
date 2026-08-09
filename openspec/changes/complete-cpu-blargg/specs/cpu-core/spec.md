# cpu-core Delta Specification

## ADDED Requirements

### Requirement: ALU operations implement hardware-correct flag semantics
The ALU SHALL provide two-operand arithmetic operations (`add`, `sub`, `compare`) and explicit carry-in variants (`adc`, `sbc`) whose Zero, Subtract, Half-carry, and Carry flags match SM83 hardware behavior, with half-carry computed over the full nibble arithmetic including the carry-in bit.

#### Scenario: Carry-in produces the half-carry
- **WHEN** `adc` is evaluated with operands `0x0F` and `0x00` and carry-in true
- **THEN** the result is `0x10` with Half-carry set and Carry clear

#### Scenario: Three-way nibble overflow
- **WHEN** `adc` is evaluated with operands `0xFF` and `0xFF` and carry-in true
- **THEN** the result is `0xFF` with Half-carry set and Carry set

#### Scenario: Borrow with carry-in
- **WHEN** `sbc` is evaluated with operands `0x00` and `0x00` and carry-in true
- **THEN** the result is `0xFF` with Subtract, Half-carry, and Carry set

### Requirement: ALU covers the full SM83 operation set
The ALU SHALL implement DAA (driven by the N, H, and C flags), the rotate/shift family (RLC, RRC, RL, RR, SLA, SRA, SRL, SWAP), bit operations (BIT, SET, RES), and signed-immediate stack-pointer arithmetic with Z=0, N=0, and H/C computed from unsigned low-byte arithmetic.

#### Scenario: DAA after BCD addition
- **WHEN** `0x15 + 0x27` is followed by DAA
- **THEN** the accumulator holds `0x42` with Carry clear

#### Scenario: DAA after BCD subtraction
- **WHEN** `0x20 - 0x13` (setting N) is followed by DAA
- **THEN** the accumulator holds `0x07`

#### Scenario: SRA preserves the sign bit
- **WHEN** SRA is applied to `0x81`
- **THEN** the result is `0xC0` with Carry set

#### Scenario: ADD SP, e8 flag semantics
- **WHEN** `ADD SP, e8` is evaluated with SP = `0xFFF8` and e8 = `+8`
- **THEN** SP becomes `0x0000` and Zero is clear, Subtract is clear, and H/C reflect low-byte arithmetic (both set)

### Requirement: Instruction decode covers all base and CB-prefixed opcodes
The decoder SHALL map every base opcode `0x00`–`0xFF` and every CB-prefixed opcode `0x00`–`0xFF` to a defined instruction, mapping the SM83 illegal opcodes (0xD3, 0xDB, 0xDD, 0xE3, 0xE4, 0xEB, 0xEC, 0xED, 0xF4, 0xFC, 0xFD) to `Illegal`, with no opcode decoding to `Unknown`.

#### Scenario: Exhaustive decode
- **WHEN** every value `0x00`–`0xFF` is decoded as a base opcode and as a CB-prefixed opcode
- **THEN** no decode yields `Unknown`, and exactly the eleven illegal base opcodes yield `Illegal`

#### Scenario: CB structural decode
- **WHEN** CB opcode `0x7E` is decoded
- **THEN** it yields BIT with bit index 7 and the `(HL)` indirect operand

### Requirement: Execution steps the machine with machine-cycle accounting
The CPU SHALL provide a step function that fetches, decodes, and executes one instruction against machine state, returning the updated state and the machine cycles consumed, where conditional jumps, calls, and returns report different costs for taken and not-taken paths.

#### Scenario: Basic fetch-decode-execute
- **WHEN** memory at PC holds `0x3E 0x2A` (`LD A, 0x2A`) and the machine steps once
- **THEN** the accumulator holds `0x2A`, PC has advanced by 2, and 8 cycles are reported

#### Scenario: Conditional cycle split
- **WHEN** `JR NZ, e8` executes with Zero set, and again with Zero clear
- **THEN** the not-taken step reports 8 cycles and the taken step reports 12 cycles

### Requirement: Interrupt semantics
The CPU SHALL implement IME with EI taking effect after the instruction following EI, immediate DI, RETI restoring IME atomically, HALT resuming when any enabled interrupt is pending regardless of IME, and interrupt dispatch that consumes 20 cycles, clears IME and the serviced IF bit, pushes PC, and jumps to the corresponding vector (0x40, 0x48, 0x50, 0x58, 0x60).

#### Scenario: Dispatch to a vector
- **WHEN** IME is set, the VBlank bits of IE and IF are set, and the machine steps
- **THEN** PC is pushed to the stack, PC becomes `0x0040`, IME is cleared, the IF VBlank bit is cleared, and 20 cycles are reported

#### Scenario: EI delay
- **WHEN** EI executes while an enabled interrupt is already pending
- **THEN** the instruction immediately after EI executes before the interrupt is dispatched

#### Scenario: HALT wakes without IME
- **WHEN** the CPU is halted with IME clear and an enabled interrupt becomes pending
- **THEN** execution resumes at the instruction after HALT without dispatching to a vector
