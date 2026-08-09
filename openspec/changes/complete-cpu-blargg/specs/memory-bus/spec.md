# memory-bus Delta Specification

## ADDED Requirements

### Requirement: Flat 64 KiB address space with region semantics
The memory bus SHALL expose byte read and write over addresses `0x0000`–`0xFFFF`, mapping cartridge ROM bytes read-only at `0x0000`–`0x7FFF` (writes ignored in this change; no MBC banking), with writable work RAM, high RAM, IE/IF registers, and IO-register access dispatched through the bus interface.

#### Scenario: ROM is read-only
- **WHEN** a byte is written to address `0x1234`
- **THEN** a subsequent read of `0x1234` returns the original ROM byte

#### Scenario: Work RAM round-trip
- **WHEN** `0x5A` is written to `0xC123`
- **THEN** reading `0xC123` returns `0x5A`

### Requirement: ROM loading from cartridge bytes
The bus SHALL initialize from a cartridge ROM image of up to 32 KiB, placing its bytes at `0x0000`–`0x7FFF`, and post-boot IO/register state SHALL match the documented DMG post-boot values so ROMs run without a boot ROM.

#### Scenario: Loading a test ROM
- **WHEN** a 32 KiB ROM image is loaded
- **THEN** reads across `0x0000`–`0x7FFF` return the image bytes and PC starts at `0x0100`

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
