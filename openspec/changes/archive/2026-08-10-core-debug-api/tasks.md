# Tasks — core-debug-api

## 1. Run-until and breakpoints

- [x] 1.1 Add breakpoint field to `GameBoy` with set/clear functions; `run_until` returning `(GameBoy, Reason)` with `FrameReady`/`BreakpointHit`; reimplement `run_frame` as the frame-only wrapper; inline expects for hit/resume/frame-unchanged
- [x] 1.2 Verify no regression: `roc test package/main.roc`, blargg suite, acid2 and sound checks all pass unchanged

## 2. PPU debug renders

- [x] 2.1 Background-map render (256×256, active tilemap + addressing mode + BGP) in `Ppu`, with an inline expect anchoring the viewport region to the framebuffer
- [x] 2.2 Tile-data render (all VRAM tiles on a fixed grid) and OAM render (40 slots, palettes and flips) in `Ppu`, with inline expects
- [x] 2.3 `example/debug.roc`: headless runner writing the three renders as PPM files for a ROM + frame count

## 3. Golden harness compare-or-create

- [x] 3.1 Acid2 check: missing golden → write digest + viewable frame image with distinct "golden created" exit path; mismatch → write actual frame image and fail
- [x] 3.2 Sound check: same semantics for the WAV digest, keeping the WAV on bless and on mismatch
- [x] 3.3 Ensure CI/nix check wrappers treat "golden created" as failure so blessing is dev-shell only (exit 3 is nonzero for any wrapper; no CI exists yet)

## 4. Accuracy ladder

- [x] 4.1 Extend `fetch-roms` with Blargg `instr_timing`, `mem_timing` and the mooneye halt/timer acceptance set
- [x] 4.2 Add mooneye pass/fail detection (serial Fibonacci bytes) to the headless runner
- [x] 4.3 Ladder runner with a committed pass-list: listed ROMs gate, others report informatively; run it, record the initial honest pass-list (7 gating: instr_timing, 3 halt, tim00_div_trigger, tim01, tim11_div_trigger; 14 informative fails)
