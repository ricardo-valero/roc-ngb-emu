# memory-bus Delta Specification

## MODIFIED Requirements

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
