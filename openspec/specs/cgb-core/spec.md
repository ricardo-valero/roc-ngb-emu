# cgb-core Specification

## Purpose

Game Boy Color machine behaviors layered over the DMG core: console detection, banked memory, color palette RAM, and the CGB register file, grown incrementally (this change: memory infrastructure; later: color rendering, double speed, HDMA).

## Requirements

### Requirement: Console detection
A cartridge whose header CGB flag is set (0x80 or 0xC0) SHALL run in CGB mode, and the CPU SHALL boot with `A = 0x11`; a cartridge without the flag SHALL run in DMG mode with `A = 0x01` and behavior identical to the pre-CGB core.

#### Scenario: CGB cart detected
- **WHEN** a ROM with CGB flag 0x80 is loaded
- **THEN** the accumulator reads 0x11 at the entry point

#### Scenario: DMG cart unaffected
- **WHEN** a ROM without the CGB flag is loaded
- **THEN** the accumulator reads 0x01 and all CGB registers are inert

### Requirement: Banked VRAM
In CGB mode, VBK (0xFF4F) bit 0 SHALL select between two 8 KiB VRAM banks at 0x8000–0x9FFF, each retaining its own contents; VBK SHALL read back as 0xFE OR the selected bank.

#### Scenario: Banks hold independent data
- **WHEN** a byte is written at 0x8000 with VBK=0 and a different byte at 0x8000 with VBK=1
- **THEN** switching VBK back and forth reads each bank's own byte

#### Scenario: VBK readback
- **WHEN** 1 is written to VBK
- **THEN** VBK reads 0xFF, and after writing 0 it reads 0xFE

### Requirement: Banked WRAM
In CGB mode, SVBK (0xFF70) bits 0–2 SHALL select the WRAM bank at 0xD000–0xDFFF among banks 1–7, with 0 selecting bank 1; 0xC000–0xCFFF SHALL remain bank 0 regardless.

#### Scenario: Bank switch preserves contents
- **WHEN** bytes are written at 0xD000 under SVBK=1 and SVBK=2
- **THEN** each bank reads back its own byte after switching

#### Scenario: Zero selects one
- **WHEN** SVBK is written 0 after writing a byte under SVBK=1
- **THEN** 0xD000 reads the bank-1 byte

### Requirement: Palette RAM ports
In CGB mode, BCPS/BCPD (0xFF68/0xFF69) and OCPS/OCPD (0xFF6A/0xFF6B) SHALL address two independent 64-byte palette memories: the specifier holds a 6-bit index and an auto-increment bit (bit 7) that advances the index after each data write; data reads and writes SHALL access the byte at the current index.

#### Scenario: Auto-increment write sequence
- **WHEN** BCPS is set to 0x80 and two bytes are written to BCPD
- **THEN** palette bytes 0 and 1 hold them, and BCPS's index reads 2

#### Scenario: Independent memories
- **WHEN** the same index is written via BCPD and OCPD with different values
- **THEN** each data port reads back its own memory's byte

### Requirement: CGB registers inert on DMG
On DMG carts, VBK, SVBK, BCPS/BCPD, OCPS/OCPD, KEY1, and OPRI SHALL read 0xFF and drop writes.

#### Scenario: DMG inertness
- **WHEN** a DMG cart writes and reads these registers
- **THEN** reads are 0xFF and no banking or palette state changes

### Requirement: Color rendering
In CGB mode the PPU SHALL render background, window, and sprites in color: tile attributes from VRAM bank 1 select per-tile palette (0–7), tile-data bank, horizontal/vertical flip, and BG-over-OBJ priority; pixel colors come from the palette RAM as RGB555. Priority SHALL follow CGB rules: LCDC bit 0 acts as master BG priority (clear = sprites always in front), per-tile and per-sprite priority bits apply only against nonzero BG colors, and sprite-vs-sprite priority follows OAM index order (or X order when OPRI selects DMG behavior). Rendering SHALL match the cgb-acid2 test ROM's published reference, held as a golden digest.

#### Scenario: cgb-acid2 oracle
- **WHEN** cgb-acid2 runs to its stable frame
- **THEN** the framebuffer digest matches the blessed golden (verified visually against the published reference at bless time)

#### Scenario: DMG rendering unchanged in substance
- **WHEN** dmg-acid2 runs on the RGB555 framebuffer
- **THEN** the image is the same picture in grayscale RGB555 (golden re-blessed once for the representation change, visually verified)

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
