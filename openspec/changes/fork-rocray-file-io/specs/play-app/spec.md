## ADDED Requirements

### Requirement: Windowed play of a runtime-loaded ROM
The project SHALL provide a roc-ray application that reads the ROM as binary bytes from disk at startup — no build-time embedding — resolving the path as: first program argument, else `rom/play.gbc`; runs one emulator frame per render tick at capped 60fps pacing; and presents the 160×144 framebuffer scaled in a window using point-filtered texture rendering.

#### Scenario: Sample ROM on screen
- **WHEN** a ROM exists at the resolved path and the play app is launched
- **THEN** a window opens showing the ROM's rendered output, updating at display rate, and Esc exits

#### Scenario: Swapping ROMs needs no rebuild
- **WHEN** the user relaunches the already-built app with a different ROM path argument, or replaces the file at the default path
- **THEN** the new ROM plays, with no recompilation of the app

#### Scenario: Missing ROM fails actionably
- **WHEN** no file exists at the resolved path
- **THEN** the app exits with an error that names the path it tried and how to provide a ROM, rather than opening a blank window

## REMOVED Requirements

### Requirement: Windowed play of an embedded ROM
**Reason**: Build-time embedding cost a rebuild per ROM swap and blocked runtime ROM selection; the forked roc-ray platform now provides binary file reads.
**Migration**: Superseded by "Windowed play of a runtime-loaded ROM" — drop any `rom/play.gb` seeding step and launch the built app with the ROM on disk.
