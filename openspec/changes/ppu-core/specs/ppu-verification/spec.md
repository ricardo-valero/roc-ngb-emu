# ppu-verification Delta Specification

## ADDED Requirements

### Requirement: Headless frame capture
The project SHALL provide a headless command-line runner that loads a ROM, executes a requested number of frames, and writes the final framebuffer as a PPM image with the four DMG shades tone-mapped to grayscale.

#### Scenario: Frame dump produces a valid image
- **WHEN** the runner executes a ROM for N frames with an output path
- **THEN** a 160×144 PPM file is written whose pixels use exactly the four gray levels

### Requirement: dmg-acid2 rendering oracle
Rendering SHALL be verified against the dmg-acid2 test ROM: a fetch script obtains the ROM (untracked, alongside the Blargg ROMs), and the rendered frame SHALL match the published reference image — confirmed visually once, then frozen as a framebuffer digest checked by the suite.

#### Scenario: Reference render
- **WHEN** dmg-acid2 runs to its stable frame and the framebuffer is captured
- **THEN** the output matches the frozen reference digest

### Requirement: No CPU verification regression
PPU integration (real LY progression replacing the stub, new interrupt sources) SHALL NOT regress CPU verification: the full Blargg `cpu_instrs` suite continues to pass.

#### Scenario: Blargg suite after PPU integration
- **WHEN** the Blargg suite runs on the integrated emulator
- **THEN** all 11 individual ROMs still pass
