# memory-bus Delta Specification

## ADDED Requirements

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
