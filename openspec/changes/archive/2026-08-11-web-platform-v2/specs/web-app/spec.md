# web-app Delta Spec

## Purpose

The browser frontend: play any user-supplied Game Boy ROM in a web page with keyboard input, rendered frames, and sound, on the roc-web platform.

## ADDED Requirements

### Requirement: Runtime ROM loading
The web app SHALL load ROMs at runtime, not build time: a default ROM is fetched alongside the page, and the user can replace it at any moment via a file picker or drag-and-drop. Loading a ROM SHALL reset the emulator to a fresh machine running those bytes.

#### Scenario: Default ROM plays
- **WHEN** the page loads with its default ROM available
- **THEN** the emulator boots and runs it without a rebuild having been required

#### Scenario: User supplies a ROM
- **WHEN** the user picks or drops a `.gb` file onto the page
- **THEN** the emulator resets and runs the supplied bytes

### Requirement: Keyboard input reaches the joypad
Keyboard state SHALL reach the emulator with the same mapping as the native app (arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select), expressed in the app as per-key queries rather than platform-fixed bit positions.

#### Scenario: Input round-trip
- **WHEN** the user holds Start at the game's title screen
- **THEN** the game responds exactly as it does in the native app

### Requirement: Rendered frames via a configurable backend
The app SHALL declare its screen dimensions, display scale, and renderer preference (auto, WebGPU, WebGL, or Canvas2D) in its config; the page SHALL honor them, fall back in order under auto, and surface which backend is active.

#### Scenario: Auto backend selection
- **WHEN** the page runs in a browser without WebGPU
- **THEN** frames render via the next available backend and the status line names it

### Requirement: Audio output
APU samples (48 kHz interleaved stereo) SHALL reach the speakers, staying in sync with gameplay within normal buffering tolerance.

#### Scenario: Sound plays
- **WHEN** a game that produces audio runs with the page audible
- **THEN** its sound is heard without gross drift from the on-screen action
