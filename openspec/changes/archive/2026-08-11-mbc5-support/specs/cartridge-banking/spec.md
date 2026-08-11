# cartridge-banking Delta Spec

## ADDED Requirements

### Requirement: MBC5 banking
Cartridges reporting MBC5 (header types 0x19–0x1E) SHALL bank ROM with a 9-bit bank number — low eight bits written at 0x2000–0x2FFF, the ninth bit at 0x3000–0x3FFF — with **no** zero-to-one translation (writing 0 maps bank 0 into the switchable region), wrapping to the ROM's actual bank count. RAM SHALL be enabled by writing 0x0A-in-low-nibble at 0x0000–0x1FFF and banked by the low four bits written at 0x4000–0x5FFF. Writes at 0x6000 and above SHALL have no effect.

#### Scenario: Nine-bit bank selection
- **WHEN** the low bank register is written and the high-bit register is set on a ROM large enough (or wrapped on a smaller one)
- **THEN** reads at 0x4000–0x7FFF come from the combined 9-bit bank number modulo the bank count

#### Scenario: Bank zero is selectable
- **WHEN** 0 is written to the low bank register with the high bit clear
- **THEN** reads at 0x4000–0x7FFF come from bank 0 (not bank 1)

#### Scenario: RAM banking
- **WHEN** RAM is enabled and different RAM banks are selected between writes and reads at 0xA000–0xBFFF
- **THEN** each bank holds its own data independently
