# memory-bus Delta Specification

## ADDED Requirements

### Requirement: OAM DMA transfer
Writing a value to `0xFF46` SHALL copy the 160 bytes at `value << 8` through `value << 8 + 0x9F` into OAM (`0xFE00`–`0xFE9F`). The copy completes immediately from the CPU's perspective (cycle-accurate DMA bus blocking is out of scope).

#### Scenario: DMA copies a sprite table
- **WHEN** a 160-byte sprite table is prepared at `0xC000` and `0xC0` is written to `0xFF46`
- **THEN** reads of `0xFE00`–`0xFE9F` return the bytes from `0xC000`–`0xC09F`
