# Tasks — Latest Nightly + Iter Spike

## 1. Branch and toolchain

- [x] 1.1 Create branch `nightly-iter` from current `main`
- [x] 1.2 Find the latest roc nightly (roc-overlay HEAD or latest paired
      rev) and bump `flake.nix`'s roc-overlay pin on the branch; update
      the pin comment to say this branch intentionally breaks the
      platform pairing
- [x] 1.3 Tier 1 gate: `roc check package/main.roc` and
      `roc test package/main.roc` on the new nightly — triage and record
      any breakage in `report.md` (breakage is a finding, not a failure;
      if the package won't compile at all, write the report and park)
- [x] 1.4 Probe Tier 2: try `roc check` on `check/single-step/main.roc`,
      `check/run.roc`, `check/sound/main.roc`, `check/acid2/main.roc`,
      `check/battery/main.roc`; record which build and run — this
      determines the gate set for everything below
- [x] 1.5 If Blargg runs: record the branch's own Blargg baseline
      (design D4 — the compiler bump alone changes codegen)

## 2. Iter inventory and adoption (one commit per conversion)

- [x] 2.1 Inventory hand-rolled iteration across `package/`: every
      `var`/`while` loop and recursive fold, classified hot (per-pixel /
      per-cycle) vs cold (event-time, init-time, test-time); record the
      table in `report.md`
- [x] 2.2 Convert `Cpu/Instruction.roc`'s `count_bytes` to an integer
      range iterator + `Iter.fold`; delete the "no stdlib range
      dependency" comment; exhaustiveness expects (0 Unknown, 11
      Illegal, CB family counts) gate it
- [x] 2.3 Convert `Apu.apply_events`'s index loop to iteration over the
      drained event list; package expects gate it
- [x] 2.4 Convert the cold sites from the 2.1 inventory that read better
      with Iter (expect helpers, init-time folds); skip any site where
      the nightly rejects promised Iter usage — record the repro instead
      (design D3)
- [x] 2.5 Tier 1 green after each conversion; Tier 2 suites re-run at
      the end if available
- [x] 2.6 Hot paths only if Blargg runs on the branch: convert ONE
      candidate (e.g. the OAM scan), compare against the 1.5 branch
      baseline, keep or revert; record the number either way

## 3. Pure-push survey

- [x] 3.1 Survey the latest `Builtin.roc` for push-shaped structures
      (queue, deque, ring, sink/`from_iter` targets); record findings
- [x] 3.2 Write the pure-push section of `report.md`: the
      pull/push × pure/effectful map, what exists, and a recommendation
      on an in-house structure for `Bus.samples`+`sample_count`,
      `serial_out`, and `apu_events` — applying design D5's test (a
      bounded ring, an unbounded log, and a drain-on-read queue may not
      be one abstraction) and the unique-ownership in-place constraint
- [x] 3.3 Only if 3.2 recommends it: prototype the module with package
      expects; wire it under one of the three accumulators; sound digest
      gates it if Tier 2 is available, otherwise package expects + a
      note that main-merge needs the full gate

## 4. Wrap up

- [x] 4.1 Finish `report.md`: nightly verdict, conversions landed vs
      skipped (with repros), timing numbers if any, pure-push
      recommendation, and what a future merge-to-main change requires
      (paired platform releases per the flake comment)
- [x] 4.2 Reduce any compiler bugs found to standalone repros without
      emulator provenance (design D6)
- [x] 4.3 Commit the branch; leave `main` untouched
