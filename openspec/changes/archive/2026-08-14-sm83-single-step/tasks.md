# sm83-single-step Tasks

## 1. Fetch and schema verification

- [x] 1.1 Write `check/single-step/fetch.roc` (port of the NES fetch
  app): resumable download of all SM83 vector files — base opcodes plus
  CB-prefixed names (URL-encode the space) — into gitignored
  `check/single-step/data/`; add the gitignore entry
- [x] 1.2 Fetch a sample (e.g. `00`, a load, a CB op, `76` HALT) and
  record the actual schema in a comment block: state field names, `ie`
  as field vs RAM byte, cycles-entry shape and flag-string encoding,
  case count per file; adjust design assumptions if the data disagrees

## 2. Core harness surface

- [x] 2.1 Add the flat mode to `Mmu`: construct from a 64 KiB
  `List(U8)`, reads/writes pass through verbatim with no mapping;
  `roc test package/main.roc` covers round-trip and a
  would-be-remapped address (echo/ROM/I/O) storing verbatim
- [x] 2.2 Add the optional access trace to `Mmu` (`NoTrace` default /
  `Trace` recording ordered {addr, val, direction}); expects cover
  ordering and that `NoTrace` is the default everywhere apps construct
  an `Mmu`
- [x] 2.3 Add exposed harness module (`package/Harness.roc` or
  equivalent) — build state from raw registers + ime + flat memory,
  step exactly one instruction via the `GameBoy` execution path with no
  interrupt dispatch or PPU/APU/timer coupling beyond the instruction
  itself, read back registers/ime/cycles/memory/trace; expose it from
  `package/main.roc`; expects cover the round-trip-without-step scenario
- [x] 2.4 Run the frozen digests and ROM suites (`check/run.roc` blargg
  + mooneye passlists, frame/audio digests) to confirm the disabled
  trace and untouched app paths are bit-identical

## 3. Runner

- [x] 3.1 Write `check/single-step/main.roc`: recursive-descent parser
  for the verified SM83 schema (state fields, ram pairs, full cycles
  entries), ported from the NES runner's style with parser expects
- [x] 3.2 Case execution through the harness: diff every register, ime,
  touched RAM, and total cycles (4 × cycles-entry count); report file,
  case index, and got/want per mismatched field; cap detail at first
  few failing cases per file; nonzero exit on any failure
- [x] 3.3 Placement comparison: diff the harness trace against the
  vector's non-idle cycle entries in order (addr, val, direction);
  report the first diverging access

## 4. Green the suite

- [x] 4.1 Run the base-opcode files; triage failures into core fixes
  (placement or timing bugs the ROM suites couldn't see)
- [x] 4.2 Run the CB-prefixed files; triage likewise
- [x] 4.3 If any failure class is documented hardware-model debt too
  large for this change, record the exclusion with a written reason in
  the check (repo precedent: blargg passlist exclusions); otherwise all
  files gate

## 5. Docs and wrap-up

- [x] 5.1 README: fetch/run instructions for the new check, alongside
  the existing check sections
- [x] 5.2 WISHLIST: retire the "SM83 SingleStepTests" item (move to the
  done list); note the wave-RAM window is now plannable
- [x] 5.3 `openspec validate --change sm83-single-step` and full check
  sweep before hand-off
