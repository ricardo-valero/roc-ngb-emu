# Design: battery-saves-rtc

## Context

See proposal.md for motivation. Current state that shapes the approach:

- `GameBoy` is one pure value; `Mmu.cart_ram` is a flat 128 KiB
  `List(U8)` ("session lifetime"), allocated at MBC5's max and masked by
  banking, so extraction must mask to the header-declared size.
- MBC3 exists (`mbc : [None, Mbc1, Mbc3, Mbc5]`, `bank2` selects RAM
  banks 0–3) but has no RTC state and ignores `0x6000`-region writes.
- Both frontends call `gb.run_frame(buttons)` with a bare buttons
  record; `check/` harnesses use `GameBoy.no_buttons({})`.
- The roc-ray fork's host snapshot exposes `timestamp_nanos` — a
  **monotonic** clock, unusable for RTC epoch math. roc-web's JS host
  has `performance.now()` in the same role. Neither platform currently
  surfaces UNIX time; both are ours to extend (the `args!` recipe).
- `write_bytes!`/`read_bytes!` exist in the fork (change
  `fork-rocray-file-io`); the web app has no storage layer yet.

## Goals / Non-Goals

**Goals:**

- One on-disk format, byte-compatible with the ecosystem: `.sav` = raw
  cart RAM at header size, RTC carts append the 48-byte footer (accept
  44-byte and bare forms on load).
- Wall-clock time enters the core exclusively as data, preserving
  purity and replay determinism (fixed `now` → bit-identical runs).
- Save loss is bounded by the RAM-disable heuristic plus a
  platform-appropriate backstop, not by clean-exit luck.

**Non-Goals:**

- BESS save states (follow-up change; the battery surface here is a
  strict subset of what BESS's MBC-RAM/RTC blocks will need).
- Sub-second RTC accuracy or drift correction; MBC30; HuC3 RTC.
- A generic serialization story for `GameBoy`.

## Decisions

**1. `now` rides the per-frame input record.** Widen the entry-point
input from bare buttons to `{ buttons, now : U64 }` (UNIX seconds).
MBC3's latch protocol makes per-frame freshness sufficient: registers
are only observable after a latch, and RTC resolution is one second vs.
~16 ms staleness. Alternatives rejected: an effectful clock in the core
(breaks purity and the pure-Roc check story); ticking the RTC from
emulated cycles (drifts from wall time when emulation isn't real-time,
and session catch-up needs an epoch anyway).

**2. RTC state is `{ regs, latched, last_now }`, advanced lazily.** The
running clock is materialized only at its observation points — the
latch write and battery extraction — by advancing `regs` by
`now - last_now` (skipped while halted). No per-frame RTC work, and
catch-up-on-load is the same code path with a large delta.

**3. The battery surface is two functions on `GameBoy`.**
`battery : GameBoy -> List(U8)` and
`with_battery : GameBoy, List(U8) -> GameBoy`. Battery presence comes
from the header's cartridge-type byte (battery-backed types only —
extraction is empty otherwise, injection a no-op). RAM size comes from
a header accessor (add if missing). Size tolerance (pad/truncate) lives
in `with_battery` so frontends never validate.

**4. Flush signal: a save-event counter, not a boolean.** The core
increments a `U64` when cart RAM is disabled after having been written
since the last enable (the "game just saved" edge); frontends diff it
across frames and flush on change. A counter survives multiple saves
per frame and needs no reset handshake back into the pure value.
Alternative rejected: frontends polling `ram_enable` directly — misses
the "was actually written" half of the heuristic and puts MBC knowledge
in two apps.

**5. Platform clocks are additions we own.** Ray: a `unix_time!` host
*effect* in the fork (the `args!` recipe) rather than a new snapshot
field — as-built deviation: the snapshot record's ABI layout is
compiler-chosen and hand-editing it is riskier than the proven effect
path. Web: the contract went deeper than the lib — roc-web's platform
now passes `now` through `render_frame(keys, now)` into the Host, and
`load!` takes `(rom, sav)` with a `push_battery!` effect back out
(pushed every frame, flush-flagged on the save edge, so the page's
retained copy is always current for visibility flushes). Pre-release
state: the web app references the local roc-web checkout until a
v0.4.0 bundle is cut.

**6. Web storage is IndexedDB keyed by ROM identity, not filename.**
Key = header title + global checksum (the same pair BESS's INFO block
uses), so the default fetched ROM and a dropped copy of the same game
share a save, and dropped files (which have no stable path) still key
stably. localStorage rejected: 5 MB quota vs 128 KiB saves is fine, but
binary round-tripping through strings is the kind of corruption bait
IndexedDB avoids.

## Risks / Trade-offs

- [Input-record widening breaks every `run_frame` caller] → mechanical:
  both apps, `check/lib`, examples; `no_buttons` becomes a
  `no_input`-style helper with `now = 0`, keeping checks deterministic
  by construction.
- [Games that save without disabling RAM miss the edge signal] → ray
  flushes on exit too; web flushes on `visibilitychange`; both bound
  loss to the current play session's tail, same as mainstream
  emulators.
- [User's system clock jumps (DST is fine — epoch; but manual changes)]
  → accept; identical behavior to VBA/BGB/SameBoy, and the footer
  timestamp makes it self-correcting at next load.
- [48-byte footer little-endian dword layout is fiddly] → the
  round-trip check gates it byte-for-byte; the 44-byte acceptance path
  is covered by a fixture, not left to chance.

## Migration Plan

Land core (battery surface + RTC + input widening) with checks first —
the repo builds and all checks pass before either frontend changes.
Then ray (fork clock + load/flush), then web (lib clock + IndexedDB).
Each step leaves the previous ones shippable; rollback is dropping the
top slice.

## Open Questions

- Adopt aaaaaa123456789's `rtc3test` ROM into `check/` as a gating or
  informative RTC check? Deferrable — the pure checks specified here
  cover the format and catch-up math; rtc3test adds hardware-quirk
  depth later without changing this design.
