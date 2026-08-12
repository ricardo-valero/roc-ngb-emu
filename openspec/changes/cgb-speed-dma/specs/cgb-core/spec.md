# cgb-core Delta Spec

## ADDED Requirements

### Requirement: Double-speed mode
In CGB mode, executing STOP with KEY1's prepare bit set SHALL toggle double-speed mode and clear the prepare bit; KEY1 bit 7 SHALL report the current speed. In double-speed mode the CPU and timers SHALL run at twice the rate of the PPU and APU.

#### Scenario: Speed switch
- **WHEN** the prepare bit is armed and STOP executes
- **THEN** KEY1 reads with bit 7 set and the prepare bit clear; a second armed STOP returns to normal speed

#### Scenario: Video runs at half rate
- **WHEN** the machine is in double-speed mode
- **THEN** the same number of CPU cycles advances the PPU half as many dots as at normal speed

### Requirement: VRAM DMA
In CGB mode, HDMA1–4 (0xFF51–0xFF54) SHALL set a masked source (low 4 bits ignored) and VRAM destination (masked into 0x8000–0x9FF0, honoring the selected VRAM bank); HDMA1–4 SHALL read 0xFF. Writing HDMA5 with bit 7 clear SHALL perform an immediate general-purpose copy of (length+1)×16 bytes; writing with bit 7 set SHALL arm an HBlank transfer of 16 bytes per visible scanline. HDMA5 SHALL read the remaining block count minus one with bit 7 clear while an HBlank transfer is active, and 0xFF otherwise; writing bit 7 clear while active SHALL cancel the transfer.

#### Scenario: General-purpose DMA
- **WHEN** HDMA5 is written with bit 7 clear and length n
- **THEN** (n+1)×16 bytes appear at the VRAM destination in the selected bank and HDMA5 reads 0xFF

#### Scenario: HBlank DMA counts down
- **WHEN** an HBlank transfer is armed and the PPU enters HBlank
- **THEN** 16 bytes are copied per scanline, HDMA5 counts down, and it reads 0xFF after the final block

#### Scenario: DMG inertness
- **WHEN** a DMG cart accesses HDMA1–5
- **THEN** reads are 0xFF and writes have no effect
