# Design: Play App

## Context

The core is pure and platform-free; basic-cli examples drive it headless. roc-ray 0.9.0 (spike-verified with our pinned nightly) provides the window, input, and mutable-pixel textures. Its host exposes only UTF-8 `read_file!` — no binary reads and no CLI args — so a `.gb` ROM cannot be loaded at runtime.

## Goals / Non-Goals

**Goals:** a window running any user-supplied DMG ROM at 60fps with working keyboard input; joypad semantics correct enough for real games (which poll P1).
**Non-Goals:** joypad interrupt, audio, save states, speed controls, ROM picker UI, MBC banking (32 KiB ROMs still).

## Decisions

### D1: ROM embedded at build time via file ingestion
`import "../rom/play.gb" as rom : List(U8)` — verified supported by the pinned nightly (relative paths only). `fetch-roms` seeds `rom/play.gb` from dmg-acid2 so the app always builds; swapping games is `cp game.gb rom/play.gb` + rebuild. Alternatives rejected: base64-in-text via `read_file!` (ugly, slow), upstream host PR (out of scope).

### D2: `Buttons` is a plain record defined next to the bus
`{ up, down, left, right, a, b, start, select : Bool }` lives in `Mmu` (the consumer); raylib key codes appear only in `example/play.roc`. The core stays importable from basic-cli, tests, and roc-ray alike.

### D3: P1 semantics — select bits stored, low nibble computed on read
Writes to `0xFF00` keep only bits 4–5 (plus the always-1 upper bits). Reads assemble the low nibble active-low from the button state: bit 4 low selects the d-pad group (right/left/up/down on bits 0–3), bit 5 low selects the action group (a/b/select/start). Both selected → groups AND together (hardware ORs the pulled-down lines); neither → `0xF`. The joypad interrupt is omitted: games poll, and dmg-acid2/Tetris-class titles never rely on it.

### D4: One emulator frame per render tick
`render!` reads keys → `Buttons`, calls `run_frame` once, uploads the framebuffer to a 160×144 texture (`update!` with a `List(Color)`), draws it scaled 4× with Point filtering (crisp pixels), `Capped(60)` pacing. DMG shades map to the classic green LCD palette. Emulation runs at exactly display rate — no cycle banking across ticks; at 60fps the drift from true 59.73Hz is imperceptible and elmboy does the same.

## Risks / Trade-offs

- [Embedded ROM means rebuild-to-swap] → Acceptable for a sample app; revisit if/when roc-ray grows binary file reads.
- [`run_frame`'s step budget + per-tick texture upload too slow for 60fps] → The interpreter ran Blargg ROMs at far beyond real-time speed headless; if pacing stutters, profile before redesigning.

## Open Questions

- None blocking; joypad interrupt deferred until a target game demonstrably needs it.
