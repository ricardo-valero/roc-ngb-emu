# Design — wasm-platform-spike

## Context

See proposal.md for motivation and the wasmboy tear-down (2026-08-10) for source patterns. Constraints shaping the spike:

- The Roc side is fixed: `roc build --target=wasm32` exists in the pinned nightly (`--wasm-memory` default 64 MB, `--wasm-stack-size` 8 MB); the platform must declare wasm32 support (basic-cli's refusal error shows the mechanism).
- Reference implementation: roc-wasm4 (Zig host + build.zig, updated for the new compiler per the Rust-to-Zig rewrite posts). Its WASM-4-specific surface (64 KB cap, console API) is discarded; only the host/build wiring pattern is borrowed.
- Target contract (from wasmboy, adapted): JS sees a flat, self-describing module — exported region pointers/constants for ROM-in, framebuffer-out, audio-out; scalar-only calls (`init(rom_len)`, `run_frame(buttons) -> status`). The Zig host owns the Roc runtime and copies Roc `List(U8)` results into fixed export buffers. wasmboy's full contract is legible in ~200 lines of its `test/core/save-state.js`.

## Goals / Non-Goals

**Goals:**

- Answer two questions with evidence: (1) how much friction is authoring a wasm32 platform on the new compiler; (2) what fps the unchanged core sustains in the browser (gate: comfortably ≥ 60 on a desktop browser with `--opt` on; a big shortfall = no-go or core-optimization detour).
- Establish the Zig-host ↔ Roc-app calling pattern (entrypoint shape, allocation, passing `List(U8)` both ways) that a real platform would keep.

**Non-Goals:**

- Platform API design for general consumers (post-spike work, in the new repo, informed by the spike).
- Audio, input beyond hardcoded buttons, IndexedDB persistence, debugger, workers — all later; the spike blits pixels and counts frames.
- Shaders beyond a pass-through blit, a WebGL2 fallback backend, and the GPU-command contract surface — the spike only establishes the WebGPU texture-upload path and the pluggable-renderer seam.
- Releases/bundling. The spike consumes the platform by path, the package by sibling checkout.

## Decisions

- **GB-agnostic platform, emulator as first consumer.** The platform's vocabulary is framebuffer/input/audio/storage, not Game Boy concepts — same relationship raylib has to roc-ray. Alternative (GB-specific platform) rejected: couples the second project to the first for no gain; the debug escape hatch (later) keeps GB-specific needs out of the platform anyway. The stated long-term goal is hosting more complex emulators: the framebuffer contract carries through GBA/SNES-tier (those PPUs emit pixels), and the contract is versioned so a GPU-command surface (for PS1/N64-tier machines with real GPUs) can be added later without breaking v1 consumers.
- **WebGPU rendering from day one.** The JS harness renders via WebGPU: `navigator.gpu` device, per-frame `queue.writeTexture` of the framebuffer, fullscreen triangle with a WGSL shader and nearest-neighbor sampler into a `GPUCanvasContext` — behind a pluggable renderer interface (a WebGL2 fallback backend can slot in later if a target browser demands it; none is expected — WebGPU is shipped in Chrome, Firefox, and Safari as of 2026). Decided by the user 2026-08-10 (stack: Zig + wasm + WebGPU), superseding the earlier WebGL2-baseline decision; rationale: the multi-emulator ambition makes the modern GPU path a certainty, so build on it from the start rather than migrating to it. Canvas `2d` rejected outright. Routing note: all WebGPU calls are made from the JS glue — the browser API needs no bindings — so the Zig host stays `wasm32-freestanding` and the Roc linking story is untouched (host-side WebGPU à la webgpu-wasm-zig would drag in Emscripten; still deferred, and only ever relevant for a future GPU-command contract surface).
- **Zig for the host.** Matches the compiler's own language, roc-wasm4 precedent, `wasm32-freestanding` is Zig's happy path, and the team already reads Zig from nightly debugging. Alternatives (Rust host, raw C) rejected: no precedent to crib on the new compiler.
- **Spike app entrypoint mirrors the future shape** — `program = { init!, frame! }` with buttons in and framebuffer bytes out — but is allowed to be crude (hardcoded ROM embedded via `import "..." as rom : List(U8)`, no config). Rationale: the perf measurement is only meaningful if the per-frame boundary crossing (List round-trip, copy into export buffer) matches what production would do.
- **Benchmark protocol:** run an actual game ROM (MBC1 title, not a test ROM idling in HALT), N=600 frames unthrottled via `requestAnimationFrame`-independent loop, report frames/second and ms/frame percentiles; repeat for `--opt speed` and default; note memory growth across 10k frames (refcount leak canary). Rationale: wasmboy's perf suite showed option-sensitivity matters; ours is opt-level sensitivity.
- **Sibling-checkout consumption** (`../roc-ngb-emu/package/main.roc`). Alternative (copy the package in) rejected: the spike must prove the *unchanged, shared* core works, and drift would invalidate the result.
- **Throwaway-by-default, report-first.** Only `report.md` is a promised artifact; code survives only if the recommendation is "proceed". Prevents the spike from silently becoming an unreviewed platform.

## Risks / Trade-offs

- [New-compiler platform docs are thin; roc-wasm4 archaeology may stall milestone 1] → Time-box it; the Roc Zulip and the rewrite posts name the people/examples to consult. A stall is itself a spike finding (report: "not ready yet, revisit at nightly X").
- [Perf verdict on immutable core may be bad] → That is the spike doing its job; the report should isolate where time goes (CPU loop vs List copies) so a targeted core optimization change can be scoped instead of guessing.
- [Nightly pairing: the platform host must match the Roc nightly's ABI expectations] → Pin the same nightly as this repo's flake from day one (same discipline as the roc-ray pairing rule).
- [Sibling-checkout path makes the spike non-hermetic] → Acceptable for a spike; the report notes the exact commit of both repos used for the benchmark.

## Open Questions

- Repo name (`roc-wasm-frame` is a working title) — decide at repo creation.
- Whether the Zig host or JS drives the frame loop (JS `requestAnimationFrame` calling in, vs host-side scheduling) — milestone 1 will reveal which is natural; either satisfies the benchmark protocol.
- Which Zig version the nightly's wasm32 linking expects — read out of the Roc release notes / roc-wasm4 flake when setting up.
