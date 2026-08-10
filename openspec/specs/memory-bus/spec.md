# memory-bus Specification

## Purpose
TBD - created by archiving change complete-cpu-blargg. Update Purpose after archive.
## Requirements
### Requirement: Flat 64 KiB address space with region semantics
The memory bus SHALL expose byte read and write over addresses `0x0000`–`0xFFFF`: the cartridge regions (`0x0000`–`0x7FFF` ROM, `0xA000`–`0xBFFF` cartridge RAM) are mapped by the selected memory bank controller — ROM reads are bank-mapped and writes below `0x8000` act as MBC register writes (ignored for ROM-only cartridges) — with writable work RAM, high RAM, IE/IF registers, and IO-register access dispatched through the bus interface.

#### Scenario: ROM data is not writable
- **WHEN** a byte is written to address `0x1234` on a ROM-only cartridge
- **THEN** a subsequent read of `0x1234` returns the original ROM byte

#### Scenario: Work RAM round-trip
- **WHEN** `0x5A` is written to `0xC123`
- **THEN** reading `0xC123` returns `0x5A`

### Requirement: ROM loading from cartridge bytes
The bus SHALL initialize from a cartridge ROM image of any supported size (up to the controller's addressable maximum), retaining the full image for bank mapping, and post-boot IO/register state SHALL match the documented DMG post-boot values so ROMs run without a boot ROM.

#### Scenario: Loading a test ROM
- **WHEN** a 32 KiB ROM image is loaded
- **THEN** reads across `0x0000`–`0x7FFF` return the image bytes and PC starts at `0x0100`

#### Scenario: Loading a banked ROM
- **WHEN** a 64 KiB MBC1 image is loaded
- **THEN** all four banks are reachable through the `0x4000` region by bank selection

### Requirement: Serial output capture
The bus SHALL capture serial transfers: when a value with bit 7 set is written to SC (`0xFF02`), the current SB byte (`0xFF01`) SHALL be appended to an observable serial output log.

#### Scenario: Blargg reporting channel
- **WHEN** `0x50` is written to `0xFF01` followed by `0x81` to `0xFF02`
- **THEN** the serial output log ends with byte `0x50` ("P")

### Requirement: Minimal timer
The bus SHALL implement DIV (`0xFF04`), TIMA (`0xFF05`), TMA (`0xFF06`), and TAC (`0xFF07`): DIV increments at 16384 Hz and resets to 0 on any write; TIMA increments at the TAC-selected rate while TAC enables it, and on overflow reloads from TMA and sets the timer bit in IF.

#### Scenario: TIMA overflow requests an interrupt
- **WHEN** TIMA is `0xFF` with the timer enabled and enough cycles elapse for one increment
- **THEN** TIMA becomes the TMA value and the timer interrupt bit is set in IF

#### Scenario: DIV write resets
- **WHEN** any value is written to `0xFF04`
- **THEN** reading `0xFF04` returns `0x00`

### Requirement: OAM DMA transfer
Writing a value to `0xFF46` SHALL copy the 160 bytes at `value << 8` through `value << 8 + 0x9F` into OAM (`0xFE00`–`0xFE9F`). The copy completes immediately from the CPU's perspective (cycle-accurate DMA bus blocking is out of scope).

#### Scenario: DMA copies a sprite table
- **WHEN** a 160-byte sprite table is prepared at `0xC000` and `0xC0` is written to `0xFF46`
- **THEN** reads of `0xFE00`–`0xFE9F` return the bytes from `0xC000`–`0xC09F`

### Requirement: Joypad register
The bus SHALL hold externally-set button state and implement P1/JOYP (`0xFF00`): writes retain only the group-select bits 4–5; reads return the select bits with the low nibble computed active-low from the selected group — bit 4 low selects the d-pad (right, left, up, down on bits 0–3), bit 5 low selects actions (A, B, Select, Start on bits 0–3), both low combines the groups, neither low reads `0xF`.

#### Scenario: Action group read
- **WHEN** the A button is set pressed and `0x10` is written to `0xFF00` (select actions)
- **THEN** reading `0xFF00` returns a value whose bit 0 is clear and bits 1–3 are set

#### Scenario: D-pad group read
- **WHEN** the down button is set pressed and `0x20` is written to `0xFF00` (select d-pad)
- **THEN** reading `0xFF00` returns a value whose bit 3 is clear

#### Scenario: No group selected
- **WHEN** `0x30` is written to `0xFF00` with buttons pressed
- **THEN** the low nibble of `0xFF00` reads `0xF`

### Requirement: APU register semantics
Reads of `0xFF10`–`0xFF26` SHALL apply the documented read-back OR masks (write-only and unused bits read as 1, unmapped slots read `0xFF`); NR52 SHALL read as `0x70` plus the power bit and live channel-status bits. Writing NR52's power bit to 0 SHALL clear `0xFF10`–`0xFF25` and gate writes to that range until power is restored. Wave RAM (`0xFF30`–`0xFF3F`) SHALL read and write freely. Channel trigger writes (NRx4 bit 7 while powered) SHALL be delivered to the APU as events.

#### Scenario: Read-back masks
- **WHEN** `0x00` is written to NR11 (`0xFF11`) and it is read back
- **THEN** the value is `0x3F` (duty bits clear, length bits masked to 1)

#### Scenario: Power-off clears and gates
- **WHEN** register values are set, `0x00` is written to NR52, and NR11 is written afterwards
- **THEN** `0xFF10`–`0xFF25` read as their masked zero values and the post-power-off write has no effect

#### Scenario: Trigger event delivery
- **WHEN** a value with bit 7 set is written to NR24 (`0xFF19`)
- **THEN** the APU observes a CH2 trigger event on its next tick

