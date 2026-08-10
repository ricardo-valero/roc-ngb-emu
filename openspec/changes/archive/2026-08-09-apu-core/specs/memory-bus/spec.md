# memory-bus Delta Specification

## ADDED Requirements

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
