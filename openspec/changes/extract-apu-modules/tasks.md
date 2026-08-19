# Tasks — Shape the APU like the CPU

## 1. Baseline

- [x] 1.1 Confirm `roc check package/main.roc`, `roc test package/main.roc`,
      and `roc check/sound/main.roc` are green before touching anything
- [x] 1.2 Note the Blargg baseline for the perf comparison: 10m11 wall clock
      (measured post-trace-move, move-access-trace-to-cpu task 3.3)

## 2. Create the modules (one commit with §3 — the tree does not compile in between)

- [x] 2.1 Create `package/Apu/Register.roc`: pure byte→meaning decoders —
      `envelope`, `sweep`, `frequency` (+ pulse/wave/noise periods),
      NR43 decode (divisor/shift/width7), duty + the waveform table
      (`pulse_wave`), length loads (`len64`, NR31), NRx4 bit tests
      (trigger, length-enable), `dac_on`, NR50/51 routing and master
      volume. No `/Bus` import (design D1, D3)
- [x] 2.2 Add decoder expects in `Apu/Register.roc`: envelope/sweep field
      extraction, frequency reassembly, duty ratios (moved from `Apu.roc`),
      NR43 divisor table edge (`0 -> 8`)
- [x] 2.3 Create `package/Apu/Channel.roc`: nominal `Channel` with `blank`,
      `advance_pulse`/`advance_wave`/`advance_noise`, `clock_lfsr`,
      `clock_length`, `clock_envelope` (taking the decoded envelope),
      `dac_gate`, `pulse_out` (taking the decoded duty), `noise_out`.
      No `/Bus` import (design D1, D2)
- [x] 2.4 Move the pure expects with their functions: the four LFSR step
      expects into `Apu/Channel.roc` (design D6)

## 3. Rewire Apu.roc

- [x] 3.1 Import `/Apu/Channel` and `/Apu/Register`; delete the moved
      functions and the local `Channel` alias; keep everything bus-coupled
      (`tick`, `apply_events`, `advance`, `handle_event`, triggers,
      `nrx4`, `clock_sweep`, `wave_out`, `status_byte`, `dac`/`mix`/`route`,
      power handling) rewritten through the decoders (design D1, D3, D5)
- [x] 3.2 Replace the frame-sequencer if-chains with
      `sequencer_clocks : U8 -> { length : Bool, sweep : Bool, envelope : Bool }`
      plus an expect asserting the full 8-step Pandocs schedule (design D4)
- [x] 3.3 Keep every machine-building expect in `Apu.roc` byte-identical
      (design D6); update only call-site spelling where functions moved
- [x] 3.4 Add `Apu/Channel` and `Apu/Register` to `package/main.roc`'s
      non-exposed import list
- [x] 3.5 Grep `Apu/Channel.roc` and `Apu/Register.roc` for `Bus` — zero
      hits (the D1 review rule)

## 4. Verify

- [x] 4.1 `roc check package/main.roc` and `roc test package/main.roc` pass
      (expect count grows by the new decoder/schedule expects)
- [x] 4.2 `roc check/sound/main.roc` passes — the frozen WAV digest is the
      sharpest gate for this change
- [x] 4.3 `roc check/run.roc -- check/blargg/passlist` passes (includes
      `dmg_sound`); compare wall clock against the 10m11 baseline and note
      any regression
- [x] 4.4 `roc check/run.roc -- check/mooneye/passlist`,
      `roc check/acid2/main.roc`, and `roc check/battery/main.roc` pass
- [x] 4.5 `roc build app/ray.roc --output=ray` and
      `roc build app/web/main.roc --output=app/web/play.wasm` build clean
- [x] 4.6 Confirm `Apu.roc` now reads as orchestration + mixer only, with
      no channel-state math or field bit-twiddling left in it
