# Wishlist

Aspirations beyond the active OpenSpec changes, roughly ordered by pull.
Each becomes a proposal when its time comes (2026-08-11).

## Near-term (named gaps with known shapes)

- **MBC3 RTC** — a fake always-ticking clock unblocks the licensed games
  that poll it at boot.
- **Wave-RAM access window** — the last three informative `dmg_sound`
  singles (09/10/12) need sub-instruction APU/CPU phase accuracy the
  batched APU intentionally skips; revisit only if a game audibly cares.

Done since this list was written: CGB audio polish (2026-08-11, PR #17 —
write-time APU side effects, length edge clocking, model-aware power;
dmg_sound 02/03/05/07/08/11 now gate) and CGB color correction
(2026-08-11, PR #18 — roc-web v0.3.0 `with_color_correction(Cgb)`,
near's matrix in the fragment shader; grays are fixed points so DMG is
unaffected).

## The big four (user wishlist)

- **Debugger as complete as wasmboy** — the recon catalog is the spec:
  CPU state + disassembler, BG/tile/OAM/palette viewers, per-channel
  audio scopes, hex memory viewer, breakpoints and stepping, dockable
  panes. The core side already exists (`run_until`, PC breakpoints,
  debug renders, `Harness`); the work is a roc-web debug channel (host
  exports or a debug variant of the app contract) plus the panes as a
  web UI. Big, and cleanly incremental — one pane at a time.
- **Save states** — requires serializing `GameBoy`, a real design
  problem (no generic serialization in the nightly; wasmboy's
  memory-snapshot trick doesn't apply to Roc values). Likely shape: an
  explicit `to_bytes`/`from_bytes` in the core, versioned; then the web
  app persists to IndexedDB and the wasmboy-style 400-iteration
  determinism check becomes runnable.
- **Fork roc-ray: binary file read/write + PCM audio** — supersedes the
  parked `rocray-file-io` upstream-contribution plan. The original
  objection to forking (owning a multi-target platform release pipeline)
  has weakened enormously: we now ship roc-web — hosts, bundles,
  releases, nightly pairing — so the muscle exists. Would give the
  native app runtime ROM loading, battery saves, and a speaker.
- **roc-ray targeting wasm (maybe)** — raylib itself compiles to the
  web; a wasm32 roc-ray would put the *same app* in a window and a
  browser. Worth revisiting only after the fork exists; note the
  endgame it hints at: `app/ray.roc` and `app/web/main.roc` are already
  near-twins, and one shared app over both platforms is the natural
  conclusion.

## Known limitations (documented, not planned)

- **Bootleg/pirate mappers** (Vast Fame, BBD — e.g. the Taiwan "Pokémon
  Diamond" Telefang bootleg): the headers lie and the carts use
  proprietary banking; supporting them means per-family reverse-
  engineered mappers (SameBoy documents some). Out of scope unless a
  specific title earns it.
