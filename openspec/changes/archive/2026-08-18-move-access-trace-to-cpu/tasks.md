# Tasks — Move the Access Trace from Bus to Cpu

## 1. Baseline

- [x] 1.1 Confirm `roc check package/main.roc` and `roc test package/main.roc`
      are green before touching anything
- [x] 1.2 Note the Blargg baseline for the perf comparison: 10m22 wall clock
      (measured post-extraction, extract-cpu-module task 4.3)

## 2. Move the trace (one commit — the tree does not compile in between)

- [x] 2.1 Add `trace : [NoTrace, Trace(List({ addr : U16, val : U8, dir : [Read, Write] }))]`
      to the `Cpu` nominal record and the `St` alias; carry it in
      `to_st`/`of_st`; `Cpu.init` starts `NoTrace` (design D1)
- [x] 2.2 Rewrite `mem_read` and `mem_write` to match on `st.trace`
      symmetrically per the proposal's sketch; update `mem_read`'s header
      comment (the trace no longer "threads the Bus") (design D2)
- [x] 2.3 Delete `Bus.trace`, `Bus.trace_access`, and `Bus.read_traced`;
      drop `trace: Trace([])` from `Bus.flat` and `trace: NoTrace` from
      `Bus.init`; update the `Bus.flat` doc comment (design D3)
- [x] 2.4 `GameBoy.from_raw` builds its `Cpu` with `trace: Trace([])`;
      `GameBoy.access_trace` matches `gb.cpu.trace` (design D4)
- [x] 2.5 Grep the repo for `trace_access`, `read_traced`, and `bus.trace` —
      zero hits outside comments/archive before proceeding

## 3. Verify

- [x] 3.1 `roc check package/main.roc` and `roc test package/main.roc` pass
- [x] 3.2 `roc check/single-step/main.roc -- check/single-step/data/*.json`
      passes on the full suite (access placement is the point of this gate)
- [x] 3.3 `roc check/run.roc -- check/blargg/passlist` passes; compare wall
      clock against the 10m22 baseline and note any regression
- [x] 3.4 `roc check/run.roc -- check/mooneye/passlist`,
      `roc check/acid2/main.roc`, `roc check/sound/main.roc`, and
      `roc check/battery/main.roc` pass
- [x] 3.5 `roc build app/ray.roc --output=ray` and
      `roc build app/web/main.roc --output=app/web/play.wasm` build clean
