# Design — core-debug-api

## Context

See proposal.md for motivation. Source material: the wasmboy tear-down (2026-08-10). The patterns adopted here are the platform-independent third of that study; the host-side patterns (self-describing export contract, JS debug escape hatch, debugger UI) belong to the companion `wasm-platform-spike` and a later web-debugger change.

Current state that shapes the design:

- `GameBoy.run_frame : GameBoy, Buttons -> GameBoy` steps whole instructions until VBlank entry; there is no sub-frame stop condition.
- `Ppu` renders scanlines into a 160×144 framebuffer; tile fetch/palette logic exists but only in the scanline path.
- Verification digests (acid2 framebuffer, WAV) are frozen by hand; the check apps only compare.
- wasmboy reference points: `executeUntilCondition` with reason codes (frame/audio/breakpoint/error), `drawBackgroundMapToWasmMemory`/`drawTileDataToWasmMemory`/`drawOamToWasmMemory` filling debug buffers with the core's own PPU logic, and `test/golden-compare.js` compare-or-create with a PNG rendered alongside the JSON golden.

## Goals / Non-Goals

**Goals:**

- One stepping entry point a debugger can drive (`run_until` + PC breakpoint), with `run_frame` kept as the frame-only wrapper so existing apps and expects are untouched.
- Debug visualizations as pure `GameBoy -> List(U8)` functions in the core, dumpable headlessly today, blittable by any future host.
- Golden flow where blessing is mechanical (delete + re-run) and mismatches are reviewable by eye/ear.

**Non-Goals:**

- Save states / `GameBoy` serialization (own design problem; wasmboy's memory-snapshot trick does not translate to Roc values).
- Audio-buffer-full stop reason wired to a real audio host (no host consumes it yet; the reason-code type should leave room for it).
- Any UI. Memory/register *viewers* are host work; the core already exposes state via the record.

## Decisions

- **Reason codes as a tag union, not integers.** `run_until` returns `(GameBoy, Reason)` where `Reason : [FrameReady, BreakpointHit]` initially. wasmboy uses -1/0/1/2 integers because i32 is all that crosses its FFI; in Roc the host boundary translation happens in the platform later, so the core stays typed. Alternative (mirror wasmboy's integers) rejected: lossy for no benefit at this layer.
- **Breakpoint state lives in the `GameBoy` record** (e.g. an optional PC value), set/cleared via small functions. Alternative (pass breakpoint as a `run_until` argument) rejected: a debugger toggles breakpoints independently of stepping, and the record is already the single state container that survives the host round-trip (boxed in play.roc).
- **Check granularity: instruction-boundary breakpoints.** `run_frame` already steps whole instructions; checking PC after each step is a cheap comparison. Sub-instruction/memory-access breakpoints (wasmboy has read/write breakpoints) are deferred until a debugger exists to use them.
- **Debug renders reuse the PPU's tile/palette functions, not the scanline loop.** BG map render walks the 32×32 tilemap with the active addressing mode and BGP palette; tile-data render walks all of VRAM's tile region on a fixed grid; OAM render draws each of the 40 entries with its palette/flips. This mirrors wasmboy's "core renders, host blits" and guarantees debug views can't drift from real rendering. Alternative (host-side VRAM interpretation) rejected: duplicates PPU rules per frontend.
- **Renders are on-demand pure functions, not per-frame state.** No caching, no extra fields in `Ppu`. A debugger polls them; headless dumps call them once. Cost is irrelevant off the hot path.
- **Headless dump = new `example/debug.roc`** on basic-cli, PPM output like `frame.roc` (P6, four-shade grayscale), one file per view. Extending `frame.roc` was considered; separate app keeps each example single-purpose (existing pattern: frame/wav/cartridge/blargg).
- **Compare-or-create implemented in the check apps** (`example`/check flow), not in nix wrappers: the app looks for the golden file; missing → write digest + viewable artifact, report "golden created" with a distinct exit path so CI can refuse to bless silently; present + mismatch → write actual-output artifact, fail. Nix scripts stay thin fetch/run wrappers.
- **Accuracy ladder uses the existing blargg runner protocol** (serial + `$A000` memory protocol already implemented for dmg_sound). Mooneye ROMs signal pass/fail via the Fibonacci register convention (B,C,D,E,H,L = 3,5,8,13,21,34 on pass, reported over serial / breakpoint opcode `LD B,B`); the runner grows that detection. A pass-list file records which ROMs gate, satisfying the "never shrinks" requirement mechanically.

## Risks / Trade-offs

- [PC check per instruction adds a branch to the hot loop] → It's one `Option` compare; if profiling shows cost, specialize `run_frame` to skip it (kept as a separate wrapper precisely for this).
- [Mooneye ROMs are stricter than our current timing model; ladder may sit mostly non-gating] → That's the point of informative-then-promote; the ladder documents the accuracy frontier instead of hiding it. Known-fail list is visible in the report.
- [Roc nightly quirks (if-chain crash, var/while semantics) may bite new core code] → Follow the established patterns already in `package/` (match over if-chains, existing var/while idioms).
- [Compare-or-create can silently bless in CI if misused] → Distinct "golden created" exit path; CI treats it as failure, so blessing only happens deliberately in a dev shell.

## Open Questions

- Exact grid layout for the tile-data sheet (16×24 tiles vs wasmboy's layout) — cosmetic, decide at implementation.
- Whether the mooneye fetch pulls individual ROMs or the release zip — decide in `fetch-roms` when wiring.
