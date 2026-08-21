# Design — Latest Nightly + Iter Spike

## Context

See proposal.md — Why. Constraints that shape the approach:

- The flake comment is the pairing contract: the pin moves together with
  roc-web, roc-ray's `.roc-version`, and sibling repos. A latest-nightly
  branch deliberately breaks that pairing, so app builds are out of the
  gate set from the start.
- The package itself is platform-free: `roc check` / `roc test
  package/main.roc` work on any nightly that can compile the code. The
  check harnesses are apps with platform dependencies that may or may not
  resolve on the latest nightly.
- Past nightlies have broken this codebase in compiler-bug ways
  (three bugs fixed by 2026-08-10); regressions on latest are a real
  possibility and are themselves a spike finding, not a failure.
- `Iter` steps allocate a closure per element; the emulator's hot loops
  run millions of steps per frame. Blargg wall-clock (10m16 baseline on
  the current pin — but a new nightly resets the baseline) is the only
  honest arbiter.
- The three Bus push accumulators rely on unique ownership for in-place
  mutation (their comments document this); any unifying abstraction must
  preserve that or it is a regression.

## Goals / Non-Goals

**Goals:**

- Learn whether the latest nightly compiles the codebase, and what breaks.
- Land Iter in cold code where it reads better, verified by the 310
  package expects.
- A written pure-push survey with a clear in-house recommendation.

**Non-Goals:**

- No Stream in `package/`, no hot-loop conversions without timing, no
  platform bumps, no merge (proposal — Non-Goals).

## Decisions

**D1 — Branch-only, `main`'s pin frozen.**
All work on `nightly-iter`. Merging is out of scope; if the spike
succeeds, a future change bumps the pin on `main` *together with* paired
platform releases, per the flake comment's own rule.

**D2 — Gates degrade gracefully, and the report records which ran.**
Tier 1 (always): `roc check` + `roc test package/main.roc` — the 310
expects cover every module conversion. Tier 2 (if the harness platforms
build on latest): single-step, Blargg, sound, acid2, battery. Tier 3
(explicitly excluded): app builds. A conversion whose only coverage is
Tier 1 is acceptable for cold code; hot code requires Tier 2's Blargg
timing or stays untouched.

**D3 — Convert cold-to-warm, one commit per conversion, revert-friendly.**
Order: `count_bytes` (coldest, exhaustiveness expects make it
self-verifying) → `Apu.apply_events` (event-time) → expect helpers →
inventory findings. Each conversion is its own commit so a nightly or
perf problem bisects to one change. If the latest nightly rejects `Iter`
usage the docs promise (method-call syntax on ranges, `collect`
inference), record the repro in the report and skip the site — do not
contort the code to work around compiler bugs.

**D4 — New-nightly baseline before any timing claim.**
If Tier 2's Blargg runs, first measure it on the branch *before* Iter
conversions (the compiler bump alone changes codegen), then compare
conversions against that branch baseline — never against `main`'s 10m16.

**D5 — The pure-push deliverable is the report; a module is conditional.**
Survey the latest `Builtin.roc` for queue/deque/ring shapes. The
recommendation weighs one question: would a single in-house structure
(e.g. a preallocated `Ring` with append-in-place semantics) serve
`samples`+`sample_count`, `serial_out`, and `apu_events` better than
their three hand-rolled forms — without losing unique-ownership in-place
mutation, and without abstracting three things that only superficially
rhyme (a bounded ring, an unbounded log, and a drain-on-read queue are
*not* obviously one thing)? If the honest answer is no, the report says
no and nothing is built. If yes, prototype `package/Ring.roc` (or
similar) behind the sound digest if Tier 2 is available, package expects
otherwise.

**D6 — Compiler bugs found on latest get minimal repros, no emulator
provenance.**
Consistent with how previous repro repos were handled: reduce to
standalone snippets before filing or recording anything upstream-facing.

## Risks / Trade-offs

- [Latest nightly fails to compile the package at all] → that is itself
  the spike's primary finding; record what breaks and stop — the branch
  parks until a later nightly.
- [Iter closure overhead in warm paths] → D2/D4 timing discipline; cold
  paths are immune by definition (event-time and test-time code).
- [Harness platforms don't build → thin gates] → Tier 1's 310 expects
  still cover semantics of every conversion site; hot paths simply stay
  untouched (D2).
- [Abstraction for its own sake in the push module] → D5's "three things
  that only superficially rhyme" test; the default answer is no.

## Migration Plan

Spike branch; nothing migrates. Rollback = delete the branch. The change
archives with the report as its lasting artifact regardless of how much
code lands.

## Open Questions

None blocking — the open questions (does latest compile us? is Iter fast
enough warm? is there a builtin queue?) are the tasks themselves.
