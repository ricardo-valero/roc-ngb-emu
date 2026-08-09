# play-app Delta Specification

## ADDED Requirements

### Requirement: Windowed play of an embedded ROM
The project SHALL provide a roc-ray application that embeds the ROM at `rom/play.gb` at build time, runs one emulator frame per render tick at capped 60fps pacing, and presents the 160×144 framebuffer scaled in a window using point-filtered texture rendering with the four DMG shades tone-mapped to a visible palette.

#### Scenario: Sample ROM on screen
- **WHEN** `rom/play.gb` exists (seeded from dmg-acid2 by the fetch app) and the play app is built and launched
- **THEN** a window opens showing the ROM's rendered output, updating at display rate, and Esc exits

### Requirement: Keyboard input reaches the joypad
The play app SHALL map held keyboard state to the emulator's button record each frame: arrow keys to the d-pad, X to A, Z to B, Enter to Start, Backspace to Select. Raylib key codes SHALL NOT appear outside the play app.

#### Scenario: Input round-trip
- **WHEN** a game ROM polls the joypad while a mapped key is held
- **THEN** the corresponding button reads as pressed (active-low) through P1
