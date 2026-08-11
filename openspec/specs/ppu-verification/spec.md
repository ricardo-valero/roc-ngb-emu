# ppu-verification Specification

## Purpose
TBD - created by archiving change ppu-core. Update Purpose after archive.
## Requirements
### Requirement: Headless frame capture
The project SHALL provide a headless command-line runner that loads a ROM, executes a requested number of frames, and writes the final framebuffer as a PPM image with the four DMG shades tone-mapped to grayscale.

#### Scenario: Frame dump produces a valid image
- **WHEN** the runner executes a ROM for N frames with an output path
- **THEN** a 160×144 PPM file is written whose pixels use exactly the four gray levels

### Requirement: dmg-acid2 rendering oracle
Rendering SHALL be verified against the dmg-acid2 test ROM: a fetch script obtains the ROM (untracked, alongside the Blargg ROMs), and the rendered frame SHALL match the published reference image — confirmed visually once, then checked as a framebuffer digest by the suite. The digest check SHALL use compare-or-create semantics: when no golden digest exists, the check writes one (blessing a new golden = delete and re-run) together with a human-viewable image of the blessed frame; when the digest exists and the render mismatches, the check fails and writes the actual frame as an image for visual comparison against the blessed one.

#### Scenario: Reference render
- **WHEN** dmg-acid2 runs to its stable frame and the framebuffer is captured with a golden digest present
- **THEN** the output matches the frozen reference digest

#### Scenario: Blessing a golden
- **WHEN** the check runs with no golden digest present
- **THEN** it writes the digest and a viewable image of the frame, and reports that a new golden was created rather than passing silently

#### Scenario: Mismatch is reviewable
- **WHEN** the check fails against an existing golden
- **THEN** the actual frame is written as a viewable image alongside the blessed one

### Requirement: No CPU verification regression
PPU integration (real LY progression replacing the stub, new interrupt sources) SHALL NOT regress CPU verification: the full Blargg `cpu_instrs` suite continues to pass.

#### Scenario: Blargg suite after PPU integration
- **WHEN** the Blargg suite runs on the integrated emulator
- **THEN** all 11 individual ROMs still pass

