# Tasks: cgb-audio-polish

- [x] 1. Mmu: address-coded APU events — report NR10/NRx1/NRx2/NR30/NRx4
      writes as the address low byte, add the 0xF1 power-on event, allow DMG
      NRx1 length writes while powered off (length bits only); update the
      Mmu expects that assert event codes.
- [x] 2. Apu: decode address events; add `len_en`/`sweep_neg_used` to
      Channel; NRx1 reloads length at write time; NRx2/NR30 DAC-off disables
      immediately; NR10 negate-clear quirk; trigger reloads only an expired
      counter.
- [x] 3. Apu: split tick (advance pending before this step's events);
      length-counter edge clocking on NRx4 writes (enable 0→1 clock,
      trigger reload to max−1); mark `sweep_neg_used` in both sweep
      calculation sites.
- [x] 4. Apu: model-aware power — DMG keeps lengths through power-off, CGB
      clears; power-on resets frame sequencer and duty/wave positions.
- [x] 5. Package expects for each new requirement scenario (length reload,
      DAC off, negate quirk, edge clocking, DMG/CGB power split, FS reset);
      full suite green.
- [x] 6. Run check-blargg: `02`, `03`, `05`, `08`, `11` pass; promote them
      in `check/blargg/passlist`; iterate on failures.
- [x] 7. Run check-sound: re-bless the golden with ear verification if the
      digest shifted. Run check-acid2 and check-mooneye for no regressions.
