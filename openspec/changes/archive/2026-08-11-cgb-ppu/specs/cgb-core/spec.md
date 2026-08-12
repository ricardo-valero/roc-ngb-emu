# cgb-core Delta Spec

## ADDED Requirements

### Requirement: Color rendering
In CGB mode the PPU SHALL render background, window, and sprites in color: tile attributes from VRAM bank 1 select per-tile palette (0–7), tile-data bank, horizontal/vertical flip, and BG-over-OBJ priority; pixel colors come from the palette RAM as RGB555. Priority SHALL follow CGB rules: LCDC bit 0 acts as master BG priority (clear = sprites always in front), per-tile and per-sprite priority bits apply only against nonzero BG colors, and sprite-vs-sprite priority follows OAM index order (or X order when OPRI selects DMG behavior). Rendering SHALL match the cgb-acid2 test ROM's published reference, held as a golden digest.

#### Scenario: cgb-acid2 oracle
- **WHEN** cgb-acid2 runs to its stable frame
- **THEN** the framebuffer digest matches the blessed golden (verified visually against the published reference at bless time)

#### Scenario: DMG rendering unchanged in substance
- **WHEN** dmg-acid2 runs on the RGB555 framebuffer
- **THEN** the image is the same picture in grayscale RGB555 (golden re-blessed once for the representation change, visually verified)
