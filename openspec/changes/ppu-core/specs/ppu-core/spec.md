# ppu-core Delta Specification

## ADDED Requirements

### Requirement: PPU timing and mode progression
The PPU SHALL advance with the machine cycles reported by CPU steps: 456 dots per scanline across 154 lines (LY `0`–`153`), visible lines sequencing through OAM scan (mode 2, 80 dots), drawing (mode 3), and HBlank (mode 0), with lines 144–153 in VBlank (mode 1). LY SHALL be readable at `0xFF44` and STAT (`0xFF41`) SHALL report the current mode and LY=LYC coincidence.

#### Scenario: LY progresses through a frame
- **WHEN** the machine runs for 70,224 cycles from the start of a frame
- **THEN** LY has wrapped through 0–153 exactly once and returned to its starting line

#### Scenario: Mode is visible in STAT
- **WHEN** a visible line begins
- **THEN** STAT reports mode 2, then mode 3, then mode 0 within that line, and mode 1 during lines 144–153

### Requirement: PPU interrupts
The PPU SHALL set the VBlank interrupt (IF bit 0) when entering line 144, and SHALL set the STAT interrupt (IF bit 1) when an enabled STAT source triggers: mode 0/1/2 entry per STAT enable bits 3/4/5, and LY=LYC coincidence per enable bit 6 with the coincidence flag (STAT bit 2) kept current.

#### Scenario: VBlank interrupt fires once per frame
- **WHEN** LY transitions from 143 to 144
- **THEN** IF bit 0 is set exactly once for that transition

#### Scenario: LYC coincidence
- **WHEN** LYC is set to 40 with STAT bit 6 enabled and LY reaches 40
- **THEN** STAT bit 2 is set and IF bit 1 is set

### Requirement: Background rendering
The PPU SHALL render the background layer per scanline honoring LCDC: BG enable (bit 0), tile map select (bit 3), tile data addressing mode (bit 4, `0x8000` unsigned / `0x8800` signed), SCX/SCY scrolling with wraparound, and the BGP palette mapping 2-bit tile colors to shades.

#### Scenario: Tile pattern reaches the framebuffer
- **WHEN** VRAM holds a known tile pattern mapped at the scroll origin and a frame is rendered
- **THEN** the framebuffer's top-left pixels equal the tile's BGP-mapped shades

#### Scenario: Scroll wraps
- **WHEN** SCX/SCY place the visible window across the 256-pixel map edge
- **THEN** the framebuffer shows the map wrapping around, not garbage

### Requirement: Window rendering
The PPU SHALL render the window layer when LCDC bit 5 is set, positioned by WX-7/WY, using the LCDC bit 6 tile map, with an internal line counter that increments only on scanlines where the window rendered.

#### Scenario: Window overlays background
- **WHEN** the window is enabled at WX=7, WY=64
- **THEN** scanlines 64+ show window tiles from the window's own line 0 onward, and lines above 64 show background

### Requirement: Sprite rendering
The PPU SHALL render OAM sprites when LCDC bit 1 is set: 8×8 or 8×16 per LCDC bit 2 (tile index bit 0 masked in 8×16 mode), X/Y flip attributes, OBP0/OBP1 palettes with color 0 transparent, at most 10 sprites per scanline (selected in OAM order), DMG priority (smaller X wins, OAM order breaks ties), and the BG-over-OBJ attribute placing the sprite behind nonzero background colors.

#### Scenario: Basic sprite appears
- **WHEN** OAM holds one sprite at screen position (8, 16) with a known tile
- **THEN** the framebuffer shows the tile's OBP-mapped, non-transparent pixels at (0..7, 0..7)

#### Scenario: Scanline sprite limit
- **WHEN** eleven sprites share a scanline
- **THEN** only the first ten in OAM order render on that line

#### Scenario: BG-over-OBJ priority
- **WHEN** a sprite with the priority attribute overlaps background pixels of nonzero color
- **THEN** those pixels show the background; the sprite shows only over background color 0

### Requirement: Framebuffer and frame stepping
The emulator SHALL expose the rendered 160×144 framebuffer as row-major post-palette shades (0–3), and SHALL provide a frame-stepping operation that runs the machine until the next VBlank entry (bounded by a step budget so a wedged ROM cannot hang the caller).

#### Scenario: One frame advances the picture
- **WHEN** frame stepping is invoked on a ROM that draws to the screen
- **THEN** it returns at VBlank entry with a 23,040-entry framebuffer of values ≤ 3
