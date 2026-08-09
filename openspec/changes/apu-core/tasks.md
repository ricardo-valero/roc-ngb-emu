## 1. Bus: APU register semantics

- [ ] 1.1 Read-back OR-mask table for `0xFF10`–`0xFF26` (unmapped slots `0xFF`), NR52 computed read (`0x70` | power | status), wave RAM passthrough; expects for the full mask table (D3)
- [ ] 1.2 NR52 power-off: clear `0xFF10`–`0xFF25`, gate writes while off; expects
- [ ] 1.3 `apu_events` queue on `Mmu`: NRx4 bit-7 writes (while powered) and power toggles append events; expects (D2)

## 2. APU skeleton and frame sequencer

- [ ] 2.1 `package/Apu.roc`: `Apu` record, `tick` draining events and advancing the 512 Hz frame sequencer from cycles; wire into `GameBoy.finish` (D1); all existing suites green
- [ ] 2.2 Length counters for all four channels with NR52 status bits; expects for length expiry (D5)

## 3. Pulse channels

- [ ] 3.1 CH2: frequency timer, duty walker, envelope; sample accumulator at 48 kHz emitting interleaved stereo F32 (D4); expects for duty pattern and envelope stepping
- [ ] 3.2 CH1: CH2 plus sweep (shadow frequency, negate, shift, overflow-disables on trigger and recompute); expects

## 4. Wave and noise

- [ ] 4.1 CH3: DAC gate, wave RAM walker, volume shift; expects against a known wave pattern
- [ ] 4.2 CH4: LFSR (15/7-bit), divisor table; expects for the documented initial sequence

## 5. Mixing and output

- [ ] 5.1 NR51 panning, NR50 master volume, DAC on/off contribution; `GameBoy.take_samples`; expects for panning and per-frame sample count

## 6. Verification harness

- [ ] 6.1 `example/wav.roc`: run N frames, write 16-bit PCM stereo WAV (RIFF header by hand, like the PPM)
- [ ] 6.2 `fetch-roms` adds `dmg_sound` rom_singles; `example/blargg.roc` gains the `$A000` signature/status completion detector alongside serial
- [ ] 6.3 `01-registers` passes; iterate on masks/power behavior as it reports
- [ ] 6.4 Ear-check a rendered WAV (known ROM, fixed frames), then freeze its digest into a `check-sound` nix app; informative runner for the remaining `dmg_sound` singles
- [ ] 6.5 Measure combined-ROM suite wall time before/after APU tick; note the delta

## 7. Wrap-up

- [ ] 7.1 All suites green (Blargg 12/12, check-acid2, check-sound, package expects); README updated (sound core, no speaker output yet); add the PCM-stream host ask to the deferred `rocray-file-io` proposal
- [ ] 7.2 Delta-spec scenarios verified; ready for archive
