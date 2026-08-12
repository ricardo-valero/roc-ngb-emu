# apu-verification Delta Spec

## MODIFIED Requirements

### Requirement: Register-conformance oracle
Blargg's `dmg_sound` singles `01-registers`, `02-len ctr`, `03-trigger`,
`04-sweep`, `05-sweep details`, `06-overflow on trigger`, `07-len sweep
period sync`, `08-len ctr during power`, and `11-regs after power` SHALL
pass and gate `check-blargg` via the passlist. The headless runner SHALL
detect the memory-reporting protocol these ROMs use (signature `0xDE 0xB0
0x61` at `$A001` with a status byte at `$A000`, `0x80` while running) in
addition to serial reporting. Singles `09`, `10`, and `12` SHALL remain
informative: they measure the sub-instruction wave-RAM access window that
the batched APU intentionally does not model.

#### Scenario: Gating singles pass
- **WHEN** `check-blargg` runs
- **THEN** the nine gating `dmg_sound` singles report PASS and any
  regression among them fails the check

#### Scenario: Cycle-exact singles stay informative
- **WHEN** `09-wave read while on` fails under the runner
- **THEN** the suite still exits 0, reporting the failure as informative
