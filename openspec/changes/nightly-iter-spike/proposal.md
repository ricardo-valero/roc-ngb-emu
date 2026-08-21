# Spike: Latest Nightly + Iter Adoption + Pure-Push Survey

## Why

The core hand-rolls iteration everywhere because it was written against
nightlies where the new builtins didn't exist or weren't trusted:
`Cpu/Instruction.roc`'s `count_bytes` is a recursive fold over 0–255 with a
"no stdlib range dependency" comment; `Apu.apply_events`, the PPU render and
OAM scans, and several check helpers are `var i` / `while` loops. Roc main
now ships `Iter` — a *pure* lazy pull iterator (`step : () -> …`) with
`fold`, `map`, `keep_if`, `take_first`, integer ranges (`0.to(255)`), and a
size hint — which expresses all of that directly without compromising the
core's purity. (Its effectful sibling `Stream` is out of bounds for
`package/` by construction: one `=>` would infect every caller and break
expect-based testing and digest determinism.)

Two things keep this off `main`: our pin (`nightly-2026-08-15`) predates or
distrusts these builtins, and the flake comment itself states the pairing
discipline — the pin must move together with roc-web, roc-ray's
`.roc-version`, and the sibling repos. Bumping to latest desynchronizes the
platforms, so this work lives on a **branch** (`nightly-iter`) as a spike:
find out what the latest nightly and `Iter` give us, at what cost, with
gates that degrade gracefully when platform-dependent harnesses can't build.

The second question comes from the push/pull analysis: Roc's builtins cover
pure pull (`Iter`) and effectful pull (`Stream`), while *pure push* — emit
by accumulating into threaded state — has no named abstraction. `Bus.roc`
hand-rolls three push accumulators: the preallocated sample ring
(`samples` + `sample_count`, set-based so appends stay in place),
`serial_out` (append), and `apu_events` (append + drain). This change
surveys the latest builtins for anything queue/deque/ring-shaped and
decides — in a written report — whether an in-house pure push structure
would unify those three without regressing the in-place-mutation
guarantees their comments document.

## What Changes

- New branch `nightly-iter`; `flake.nix` roc-overlay pin bumped to the
  latest nightly on the branch only. `main`'s pin does not move.
- Cold-path Iter conversions where the shape fits, each gated by
  `roc test` (310 expects): `count_bytes` → range iterator,
  `Apu.apply_events` → `Iter.fold`, expect-helper folds, and whatever the
  inventory task surfaces. Hot paths (PPU render loops, APU waveform
  timers, CPU step) are converted **only** if the Blargg harness runs on
  the new nightly to time them, and reverted on any regression.
- `report.md` in this change directory: what the latest nightly breaks or
  fixes for us, which Iter conversions stuck, the pure-push survey
  (builtins found, the pull/push × pure/effectful map, and an in-house
  Ring/Queue recommendation with rationale). A prototype module only if
  the report concludes it earns its keep.

## Capabilities

### New Capabilities

None — spike on a branch; behavior is unchanged wherever gates can verify
it.

### Modified Capabilities

None. This change sets `skip_specs: true` in its `.openspec.yaml`.

## Impact

- Branch-only: `flake.nix`, `package/` conversions, the report.
- Gates, in degradation order: `roc check` / `roc test package/main.roc`
  (platform-free — always available); the check harnesses (single-step,
  Blargg, sound, acid2, battery) **if** their platforms build on the
  latest nightly — record which ran; app builds are expected to fail
  until roc-ray/roc-web publish paired releases and are explicitly not a
  gate here.
- `main` is untouched. Merging is a separate future decision that
  requires the platform pins to catch up.

## Non-Goals

- No `Stream` in `package/` (purity — see Why). Harness-side Stream use
  is a possible future nicety, not this change.
- No hot-loop conversion without a Blargg timing comparison available on
  the branch.
- No platform URL/vendored-lib bumps for roc-ray/roc-web unless paired
  releases already exist.
- No merge to `main` as part of this change.
- The pure-push prototype is conditional on the report's recommendation —
  writing the report is the deliverable, not the module.
