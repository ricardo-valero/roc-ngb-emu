# Wishlist

Aspirations beyond the active OpenSpec changes, roughly ordered by pull.
Each becomes a proposal when its time comes (refreshed 2026-08-13, after
a long look at sibling
[roc-nes-emu](https://github.com/ricardo-valero/roc-nes-emu)).

## The feature-complete gate (persistence)

Battery saves and the MBC3 RTC landed (2026-08-13, change
`battery-saves-rtc`): `.sav` files with the 48-byte RTC footer, wall
time as per-frame data, IndexedDB on the web. One item remains:

- **Save states** — requires serializing `GameBoy`, a real design
  problem (no generic serialization in the nightly; wasmboy's
  memory-snapshot trick doesn't apply to Roc values). Likely shape: an
  explicit versioned `to_bytes`/`from_bytes` in the core; then the web
  app persists to IndexedDB and the wasmboy-style 400-iteration
  determinism check becomes runnable.

## Lessons from the NES sibling

roc-nes-emu has mostly replayed this repo's roadmap (pure-Roc checks,
play app, mapper banking), but its CPU verification leapfrogged ours:

- **Mooneye breadth** — our passlist gates 6 ROMs (halt + timer); the
  acceptance suite has ~100 more (PPU timing, OAM DMA, MBC, serial).
  Grow the passlist the way the NES blargg-ppu check does: every ROM
  either gates or is excluded with a written reason.
- **Golden-trace check (maybe)** — the NES nestest check diffs every
  instruction against a canonical log, so a regression names the exact
  PC rather than "checksum failed". The GB analogue is the Gameboy
  Doctor log format. Cheap to build after SingleStepTests; only worth
  it if regressions start costing debugging time.

## The big dreams

- **Debugger as complete as wasmboy** — the recon catalog is the spec:
  CPU state + disassembler, BG/tile/OAM/palette viewers, per-channel
  audio scopes, hex memory viewer, breakpoints and stepping, dockable
  panes. The core side already exists (`run_until`, PC breakpoints,
  debug renders, `Harness`); the work is a roc-web debug channel (host
  exports or a debug variant of the app contract) plus the panes as a
  web UI. Big, and cleanly incremental — one pane at a time.
- **One app over both platforms** — `app/ray.roc` and
  `app/web/main.roc` are already near-twins (same config-and-`render!`
  shape, same `host.key_down` buttons). raylib itself compiles to the
  web, so a wasm32 roc-ray could put the *same app* in a window and a
  browser; alternatively the two contracts converge until the twin
  files are one. Revisit once the persistence gate is closed.
- **Link cable (someday)** — serial between two instances (or a
  loopback peer) is what Pokémon trading actually needs. Transport is
  an unsolved question on both platforms; park it until someone asks.

## Done since the 2026-08-11 list

- SM83 SingleStepTests (2026-08-14, change `sm83-single-step`):
  `check/single-step` runs all 498 published vector files (1000 cases
  each) through the new core harness (`GameBoy.from_raw` /
  `step_instruction` over flat memory) with full memory-access
  *placement* comparison via the new `Mmu` access trace — HALT and
  STOP excluded with written reasons. The wave-RAM access window
  (dmg_sound 09/10/12) is now a plannable item: the instrument that
  measures sub-instruction placement exists.
- Battery saves + MBC3 RTC (2026-08-13, change `battery-saves-rtc`):
  the core's `battery`/`with_battery` surface (`.sav` + 44/48-byte RTC
  footer, catch-up on load), `{ buttons, now }` per-frame input, the
  save-event counter, `unix_time!` in the roc-ray fork, a battery/clock
  contract in roc-web (pre-release: local checkout until v0.4.0), and
  the ROM-less `check/battery` slice.

- CGB audio polish (2026-08-11, PR #17) and CGB color correction
  (2026-08-11, PR #18 — roc-web `with_color_correction(Cgb)`).
- The roc-ray fork landed both slices: binary file I/O + `args!`
  (2026-08-13, change `fork-rocray-file-io`) and the PCM audio stream
  (2026-08-13, change `fork-rocray-audio-stream` — one by-ear
  verification task still open), so the native app has game audio.
- Web audio shipped too: the APU's 48 kHz output reaches the browser
  via an AudioWorklet, so both frontends now have sound — the "Web
  Audio in roc-web" wishlist item is done.
- Checks migrated to pure Roc (2026-08-13, change `pure-roc-checks`),
  retiring the nix harness; the NES repo is adopting the same shape.

## Known limitations (documented, not planned)

- **Bootleg/pirate mappers** (Vast Fame, BBD — e.g. the Taiwan "Pokémon
  Diamond" Telefang bootleg): the headers lie and the carts use
  proprietary banking; supporting them means per-family reverse-
  engineered mappers (SameBoy documents some). Out of scope unless a
  specific title earns it.
- **Rare licensed mappers** (MBC2, MBC6, MBC7, HuC1, HuC3, MMM01) —
  add when a title earns it; MBC2's built-in nibble RAM is the small,
  well-specified one to start with.
- **SGB features** (borders, palettes, multiplayer) — a different
  console wearing a Game Boy mask; not planned.
