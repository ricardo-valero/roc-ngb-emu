# play-app Delta Spec

## ADDED Requirements

### Requirement: Battery saves persist across sessions
For a battery-backed cartridge, the app SHALL load `<rom-path>.sav` at
startup if it exists (tolerating a missing or short file) and SHALL
write the current battery bytes back to that path on exit and whenever
the game disables cartridge RAM after writing to it (the RAM-enable
falling edge — the "game just saved" signal). Write-back SHALL be
skipped entirely for cartridges with no battery-backed state.

#### Scenario: Save survives a restart
- **WHEN** a game saves in-game, the app exits, and the same ROM is
  launched again
- **THEN** the game's load screen offers the saved progress

#### Scenario: Mid-session save reaches disk
- **WHEN** the game writes cart RAM and then disables RAM
- **THEN** `<rom-path>.sav` on disk contains those writes without
  waiting for exit

#### Scenario: No save file, clean start
- **WHEN** a battery-backed ROM is launched with no `.sav` alongside it
- **THEN** the game boots with empty save data and no error
