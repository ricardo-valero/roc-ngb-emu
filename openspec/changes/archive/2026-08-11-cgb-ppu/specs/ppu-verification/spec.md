# ppu-verification Delta Spec

## MODIFIED Requirements

### Requirement: dmg-acid2 rendering oracle
Rendering SHALL be verified against acid2 test ROMs for both console models: dmg-acid2 (DMG mode) and cgb-acid2 (CGB mode), each held as a golden framebuffer digest derived from the RGB555 framebuffer, with compare-or-create semantics: when no golden digest exists, the check writes one (blessing a new golden = delete and re-run) together with a human-viewable image of the blessed frame — verified visually against the published reference at bless time; when the digest exists and the render mismatches, the check fails and writes the actual frame as an image for visual comparison against the blessed one.

#### Scenario: Reference render
- **WHEN** an acid2 ROM runs to its stable frame and the framebuffer is captured with a golden digest present
- **THEN** the output matches the blessed digest

#### Scenario: Blessing a golden
- **WHEN** the check runs with no golden digest present
- **THEN** it writes the digest and a viewable image of the frame, and reports that a new golden was created rather than passing silently

#### Scenario: Mismatch is reviewable
- **WHEN** the check fails against an existing golden
- **THEN** the actual frame is written as a viewable image alongside the blessed one
