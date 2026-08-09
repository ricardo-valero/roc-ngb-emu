# APU Core (Headless)

## Why

The APU is the last empty box on the elmboy-parity scorecard: games play, but silently. It's also the least oracle-friendly component — ultimately ears judge it — so the plan applies the discipline that worked for the CPU and PPU: build the sound core headless with mechanical oracles (register-conformance test ROM, deterministic WAV digests, dense inline expects), and defer live playback, which is blocked anyway: roc-ray 0.9.0's audio API is file/tone-based with no raw PCM streaming (verified against `platform/Audio.roc`), so speaker output joins the deferred upstream-contribution change.

## What Changes

- New `package/Apu.roc`: all four DMG channels — pulse with sweep (CH1), pulse (CH2), programmable wave (CH3), LFSR noise (CH4) — driven by a 512 Hz frame sequencer (length 256 Hz, envelope 64 Hz, sweep 128 Hz), ticked from `GameBoy.finish` with step cycles like the timer and PPU.
- Sample synthesis: channels mix through NR50/NR51 (stereo panning, master volume) into an interleaved stereo buffer at 48 kHz via a fractional cycle accumulator; the buffer is drained by consumers (`take_samples`), sized for one frame at a time.
- Bus semantics for `0xFF10–0xFF3F`: register read-back OR masks (write-only bits read as 1), NR52 power-off clearing and write-gating the register file, NR52 low bits reporting live channel-active status, wave RAM access.
- Trigger events (NRx4 bit 7 writes) reach the APU through a small pending-event queue on the bus, drained each tick.
- New `example/wav.roc`: run a ROM for N frames, write the generated samples as a 16-bit PCM WAV — the audio twin of the PPM frame dump.
- Verification: Blargg's `dmg_sound` `01-registers` ROM passes (via a runner variant that polls the `$A000` result signature these ROMs use instead of serial); a WAV digest is frozen after ear-verification as the regression oracle; remaining `dmg_sound` singles run informatively (reported, not blocking — they test hardware quirks well beyond audible correctness). All existing suites stay green.
- Out of scope: live audio in the play app (upstream PCM-stream API first — added to the deferred `rocray-file-io` change as a second host ask), full `dmg_sound` conformance (zombie mode, obscure trigger quirks), the high-pass DC-blocking filter beyond a simple DC offset convention, GBC double-speed behaviors.

## Capabilities

### New Capabilities

- `apu-core`: four-channel DMG sound synthesis — frame sequencer, channel state machines, mixing to a stereo PCM buffer.
- `apu-verification`: headless WAV capture, the `dmg_sound` register-conformance oracle, and digest-based regression.

### Modified Capabilities

- `memory-bus`: adds APU register semantics — read-back masks, NR52 power/status behavior, wave RAM, and trigger-event delivery.

## Impact

- **Code**: new `package/Apu.roc`; `package/Mmu.roc` (masks, NR52, trigger queue); `package/GameBoy.roc` (APU field + tick, `take_samples`); new `example/wav.roc`; `nix/fetch-roms.nix` (+`dmg_sound` singles), new or extended check app for the register ROM and WAV digest.
- **Tests**: channel-level expects (duty patterns, envelope steps, length expiry, sweep overflow, LFSR sequence, mask table); two new nix-app oracles; existing 173 expects, 12-ROM Blargg, check-acid2 untouched.
- **Performance**: APU tick joins the per-step hot path; sample generation is ~87 cycles apart. Watch Blargg suite wall time; the combined ROM already takes ~2.5 min.
