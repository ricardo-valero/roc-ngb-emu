# Design: APU Core

## Context

Every cycle-driven component (timer, PPU) advances from `GameBoy.finish` with the cycles each step reports, keeps its hidden state in its own nominal record, and stores game-visible registers in the bus. The APU follows the identical shape. Two bus wrinkles are new: APU registers have *read-back masks* (write-only bits read as 1 — the exact thing Blargg's `01-registers` tests), and channel *trigger* writes (NRx4 bit 7) are events, not state, so the bus needs to hand them to the APU.

roc-ray 0.9.0 cannot play raw PCM (Audio.roc offers `load_sound!`/`load_music!`/`gen_sound!`/`gen_tone!` only), so this change produces samples without a speaker path; the upstream ask is recorded in the deferred `rocray-file-io` change.

## Goals / Non-Goals

**Goals:** hardware-plausible synthesis of all four channels — right pitches, envelopes, lengths, duty ratios, noise character, stereo routing — verified mechanically where possible (`01-registers`, WAV digests, inline expects) and by ear once per change.

**Non-Goals:** full `dmg_sound` conformance (obscure trigger/zombie/power quirks); live playback; cycle-exact channel phase against hardware captures; the analog high-pass filter (samples are DC-centered by construction).

## Decisions

### D1: Same integration pattern as the PPU
`Apu := { frame_seq state, per-channel state, sample accumulator, samples : List(F32) }` with `tick : Apu, Mmu, U64 -> { apu : Apu, mmu : Mmu }` called from `finish`. The APU reads NRxx from the bus, writes NR52's status bits back via `poke`. Alternative (APU owns its registers, bus forwards) rejected: two sources of truth and a bigger bus diff.

### D2: Triggers travel through a bus-side event queue
`Mmu` gains `apu_events : List(U8)` — writes to NRx4 with bit 7 set (and NR52 power toggles) append a channel/event code; `Apu.tick` drains the queue before advancing time. Mirrors how `serial_out` already works. Alternative (APU diffs register state each tick) rejected: trigger-on-rewrite semantics get lost.

### D3: Registers stay in `mem`; masks applied at `read`
A static OR-mask table for `0xFF10–0xFF26` lives in the bus (`NR11` → `|0x3F`, `NR30` → `|0x7F`, unused slots → `0xFF`, etc.); NR52 reads as `0x70 | power << 7 | status bits` maintained by the APU. Power-off (NR52 bit 7 → 0) zeroes `0xFF10–0xFF25` and gates writes there until power returns (length counters exempt on DMG — noted, simplest form first). Wave RAM (`0xFF30–3F`) reads/writes freely (access-during-playback quirks out of scope).

### D4: Sample generation via fractional accumulator at 48 kHz
`acc += cycles * 48000; while acc >= 4194304 { emit stereo pair; acc -= 4194304 }`. Channels are evaluated at emission time from their current timers (no intra-sample supersampling); at 48 kHz vs. the 131 kHz maximum pulse rate this aliases slightly on the very highest pitches — accepted, universally done by non-band-limited emulators, inaudible in practice. Samples are F32 in [-1, 1]: each DAC maps 0–15 to ±1/4 per channel, NR51 routes, NR50 scales.

### D5: Channel state machines, straight from Pan Docs
- Pulse: 11-bit frequency timer (period `(2048-f)*4`), 8-step duty walker (12.5/25/50/75%), volume envelope (64 Hz, add/subtract, period 0 = hold), length (64 steps).
- CH1 sweep: shadow frequency, period/negate/shift at 128 Hz, overflow (>2047) disables the channel; overflow check on trigger and on each recompute.
- CH3: DAC enable (NR30 bit 7), 256-step length, volume shift (0/1/2/4 → mute/100/50/25%), 32-nibble wave RAM walker at period `(2048-f)*2`.
- CH4: 15-bit LFSR (XOR of bits 0/1 into bit 14), 7-bit mode (also into bit 6), divisor table `{8,16,...,112}` shifted by NR43.
- A channel is "active" (NR52 status) while its length permits, its DAC is on, and (CH1) sweep hasn't overflowed.

### D6: Verification ladder
1. Inline expects at the component level: duty sequences, envelope stepping, length expiry disabling the status bit, sweep overflow, the first LFSR outputs, the full read-back mask table.
2. `dmg_sound` `01-registers` must pass. These ROMs report via a signature at `$A000` (`0xDE 0xB0 0x61`, status byte ≠ 0x80 when done) rather than serial — `example/blargg.roc` gains that as a second completion detector (peeking cart RAM through the bus), which also future-proofs the runner for Blargg's other memory-reporting suites.
3. `example/wav.roc` renders audio deterministically; after a one-time ear check the WAV digest is frozen into a check app (acid2 pattern). Remaining `dmg_sound` singles run in an informative (non-failing) report so progress is visible without gating on quirk conformance.

## Risks / Trade-offs

- [APU tick on the hot path slows the suite] → Work per tick is small (counters + one sample per ~87 cycles); measure combined-ROM wall time before/after; escalation is batching sample evaluation, not skipping ticks.
- [WAV digest is brittle to any synthesis tweak] → That's its job (acid2 precedent); re-freeze consciously after each intentional audio change, with the ear check.
- [`01-registers` may drag in quirks beyond masks (power behavior)] → Power-off clearing/gating is already in scope (D3); if it demands more, implement what it names — it's the conformance floor worth meeting.
- [F32 sample math in a nightly compiler] → F32 is exercised by roc-ray examples already; fall back to I16 integer synthesis if F32 surprises appear.

## Open Questions

- Whether `01-registers` requires length-counter-write exemptions during power-off (DMG-specific) — resolved empirically at that task.
- Sample buffer draining contract for the future play app (pull per frame vs. ring) — decided when the upstream PCM API exists; `take_samples` keeps it simple until then.
