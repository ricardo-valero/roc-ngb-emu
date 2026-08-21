# Report — Latest Nightly + Iter Spike

Branch: `nightly-iter` · pin bumped `nightly-2026-08-15-f70f90a` →
`nightly-2026-08-20-9e3980a` (roc-overlay PR #8 head, 2026-08-20).

## Nightly verdict

The five-day compiler jump is a non-event for this codebase:

- Tier 1: `roc check` clean, all 310 package expects pass, **unmodified**.
- Tier 2: all five check harnesses compile; battery 7/7, sound WAV digest
  bit-exact, single-step full suite `all ok`. Blargg baseline: see below.
- Tier 3 (apps): not attempted, excluded by design — platform pairing.

One quirk, not a bug: `roc check package/Cpu/Instruction.roc` *standalone*
fails with "logical modules `Cpu/Instruction` and `Instruction` resolve to
the same source file" — checking a subdirectory module directly gives it
two names. Checking through `package/main.roc` is fine. Harmless; worth
knowing before blaming a real error.

## Iter inventory

Hot (per-pixel / per-cycle — not converted; `while` + `var` stays):

| Site | Loops | Why hot |
|---|---|---|
| `Ppu.roc` | 21 | per-pixel render, OAM scans, debug renders |
| `Apu/Channel.roc` | 3 | waveform timers, per-cycle catch-up |
| `Apu.roc` sequencer/sample loops | 2 | `while acc >= threshold` count-downs — not sequence iteration at all; a fold is the wrong shape |
| `Bus.roc` HDMA/OAM-DMA copies | ~4 | per-transfer byte copies, warm |
| `GameBoy.run_until` | 1 | ~17.6k steps/frame orchestrator |

Cold (event/init/test-time — converted or judged):

| Site | Verdict |
|---|---|
| `Cpu/Instruction.count_bytes` | **Converted** — `Iter.fold(U8.to(0, 255), …)`; the "no stdlib range dependency" workaround deleted; exhaustiveness expects gate it |
| `Apu.apply_events` | **Converted** — plain `List.fold` over the drained events (Iter unnecessary; fold was always the shape) |
| `GameBoy.roc` 220-step double-speed expect | **Converted** — `Iter.fold(U8.to(1, 220), …)` |
| `Cartridge.test_rom` bank painter | **Converted** — `Iter.fold(U64.to(0, 63), …)` |
| `Cartridge.roc` RTC footer byte loops (×2) | **Skipped** — two loop-carried accumulators (shifting value + index); a tuple-accumulator fold reads worse than the `while`. Fit test failed, not a compiler limitation |
| `Bus.roc` power-off register sweep, palette init | **Skipped** — small fixed sweeps inside heavier bus logic; marginal readability either way, left alone to keep the spike diff reviewable |

No promised Iter feature was rejected by the nightly — every conversion
attempted compiled first try. Nothing to repro upstream.

## Conversions

Four commits, one per conversion (design D3), each gated by the 310
package expects. Timing: cold paths only — no hot conversion attempted
(see Blargg section for the 2.6 decision).

## Pure-push survey

The latest builtins' complete type roster: `Encoding, Str, Hasher,
Crypto, Iter, Stream, List, Box, Dict, Set, Num` (+ `Bool`, numeric
types). **No Queue, Deque, Ring, channel, or any push-shaped structure
exists.** The nearest thing is the sink interface on
`Iter.collect : Iter(item) -> output where [output.from_iter : …]` —
which is still pull-driven (the collector drains an iterator), and
`List.with_capacity` + `append`/`set`, which is exactly what `Bus.roc`
already builds on.

The map this codebase now occupies:

| | Pure | Effectful |
|---|---|---|
| Pull | `Iter` (core-safe, now in use) | `Stream` (banned from `package/` — one `=>` infects every caller) |
| Push | accumulate into threaded state — the `Bus` accumulators | platform queues (miniaudio stream, AudioWorklet ring) — deliberately absent from the language |

### Could we in-house a pure push structure?

Yes — a `Ring(item)` nominal (preallocated `List`, write index, `push`
via `set` to keep appends in place, `drain` returning items + reset ring)
is maybe 40 lines and would type-check today. **Recommendation: don't.**
Design D5's test fails: the three accumulators only superficially rhyme —

- `samples` + `sample_count`: **bounded ring**, preallocated,
  overwrite-by-index precisely so in-place mutation never reallocates
  mid-frame;
- `serial_out`: **unbounded append log**, never drained by the core;
- `apu_events`: **drain-on-read queue**, emptied by `take_apu_events`
  each step.

One abstraction serving all three needs a config surface (bounded?
drains? resets?) larger than the three concrete implementations
combined, and the unique-ownership comments that make the in-place
mutation auditable today would move behind a generic API where they're
harder to verify. Each form is ~10 lines where it lives. The *pattern*
(pure push = accumulate into the state you thread; the platform owns the
genuinely concurrent queue) is now documented here and in the
hardware-map discussion; that's the durable artifact. Revisit only if a
fourth accumulator appears and two of the four are genuinely the same
shape.

Task 3.3 (prototype) is therefore **skipped by recommendation** — the
report is the deliverable, as the proposal allowed.

## Blargg timing

| Configuration | Wall clock |
|---|---|
| `main` pin (`nightly-2026-08-15`), reference | 10m16 |
| Branch baseline: new nightly, pre-conversion (D4) | **9m52.2** |
| Branch + all conversions incl. Iter OAM scan | **9m44.6** |

Two findings. The compiler bump alone is ~4% faster. And the one
sanctioned hot-path experiment (task 2.6, the DMG OAM scan → 
`Iter.fold(U16.to(0, 39), …)` with `bus` captured in the closure,
40 iterations × 144 lines × 60 fps) measured ~1.3% *faster* than the
branch baseline — within noise, decisively not a regression. **Kept.**
The closure-per-step overhead the design feared does not materialize at
this call density; the compiler evidently specializes the fold. The
per-pixel loops (160×144 × multiple reads each) remain unconverted —
that's a future experiment with this one as precedent, not a blanket
license.

Final Tier 2 state with all conversions: single-step `all ok`, sound
digest bit-exact, acid2 DMG+CGB pass, battery 7/7, Blargg 22 gating.

## Merge-to-main requirements

- roc-web and roc-ray must publish releases paired with the chosen
  nightly; bump platform URLs, vendored lib, `.roc-version`, and the
  flake pin together (the pairing rule in the flake comment).
- Re-run the full gate set including app builds at that point.
- The Iter conversions themselves carry no merge risk — they're gated by
  package expects that run on any nightly.
