# web-app Delta Spec

## ADDED Requirements

### Requirement: Battery saves persist in the browser
For a battery-backed cartridge, the web app SHALL persist battery bytes
in browser storage keyed by the ROM (surviving reload and browser
restart) and restore them when the same ROM is loaded again — for the
default fetched ROM and for user-dropped ROMs alike. Persistence SHALL
trigger on the RAM-enable falling edge after cart-RAM writes and when
the page becomes hidden (tab switch, close attempt); it SHALL NOT rely
solely on unload events.

#### Scenario: Save survives a reload
- **WHEN** a game saves in-game and the page is reloaded with the same
  ROM
- **THEN** the game's load screen offers the saved progress

#### Scenario: Distinct ROMs keep distinct saves
- **WHEN** two different battery-backed ROMs are played and each saves
- **THEN** reloading each ROM restores its own save, not the other's

#### Scenario: Tab hide flushes
- **WHEN** the game has written cart RAM and the tab is switched away
  without the game disabling RAM
- **THEN** the battery bytes in storage include those writes
