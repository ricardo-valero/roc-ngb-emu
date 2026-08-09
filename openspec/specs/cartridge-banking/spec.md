# cartridge-banking Specification

## Purpose
TBD - created by archiving change mbc-banking. Update Purpose after archive.
## Requirements
### Requirement: Header-driven controller selection
The bus SHALL select the memory bank controller from cartridge header byte `0x0147`: ROM-only types behave as an unbanked 32 KiB cartridge, MBC1 types use MBC1 semantics, MBC3 types use MBC3 semantics.

#### Scenario: ROM-only unchanged
- **WHEN** a ROM-only image is loaded and any value is written below `0x8000`
- **THEN** ROM reads are unaffected

### Requirement: Banked ROM mapping
`0x0000`–`0x3FFF` SHALL read from ROM bank 0 and `0x4000`–`0x7FFF` from the selected bank (bank number × `0x4000` into the full image), with the effective bank masked to the ROM's bank count. Writes to `0x2000`–`0x3FFF` select the bank: MBC1 keeps 5 bits, MBC3 keeps 7 bits, and a raw value of 0 SHALL map to bank 1 in both.

#### Scenario: Bank switch changes the window
- **WHEN** an MBC1 image whose banks contain distinct markers at offset 0 is loaded and bank 2 is selected via a write to `0x2000`
- **THEN** reading `0x4000` returns bank 2's marker while reading `0x0000` still returns bank 0's

#### Scenario: Bank 0 translates to 1
- **WHEN** `0x00` is written to `0x2000`
- **THEN** the `0x4000` region reads from bank 1

### Requirement: MBC1 secondary register and mode
Writes to `0x4000`–`0x5FFF` SHALL set the 2-bit secondary register, combined as bits 5–6 of the effective `0x4000`-region bank. Writes to `0x6000`–`0x7FFF` SHALL select the banking mode: in mode 1 the secondary register also banks the `0x0000` region (`bank2 << 5`) and cartridge RAM; in mode 0 both stay at bank 0.

#### Scenario: Secondary register extends the bank number
- **WHEN** an MBC1 image larger than 512 KiB has bank `0x21` selected via `rom_bank = 1`, `bank2 = 1`
- **THEN** the `0x4000` region reads from bank `0x21`

### Requirement: Cartridge RAM
`0xA000`–`0xBFFF` SHALL access cartridge RAM only while enabled (a value with low nibble `0xA` written to `0x0000`–`0x1FFF`); disabled reads return `0xFF` and disabled writes are dropped. MBC3 banks RAM by the `0x4000`-region register (0–3); MBC1 banks RAM by the secondary register in mode 1 only. RAM contents persist for the emulation session.

#### Scenario: Enable gate
- **WHEN** `0x55` is written to `0xA000` before enabling RAM, then `0x0A` is written to `0x0000` and `0x55` is written to `0xA000` again
- **THEN** the first read of `0xA000` (while disabled) returns `0xFF` and after the enabled write it returns `0x55`

#### Scenario: MBC3 RAM banking
- **WHEN** distinct values are written to `0xA000` under RAM banks 0 and 1
- **THEN** switching back to each bank reads back its own value

