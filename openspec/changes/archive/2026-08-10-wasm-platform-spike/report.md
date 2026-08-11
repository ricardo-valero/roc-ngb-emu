# Spike report — wasm32 Roc platform (roc-canvas)

**Date:** 2026-08-10. **Verdict: PROCEED.** All three milestones passed in one session; every go/no-go gate cleared.

## What was built

New repo at `~/Documents/dev/roc-canvas` (local, uncommitted-to-remote): a GB-agnostic wasm32 Roc platform with a Zig host, plus spike apps and a WebGPU browser harness.

- `platform/main.roc` — platform header, wasm32-only `targets:` block; app contract `main : { init! : () => model, frame! : model, U32 => model }`; hosted effects `Host.blit!` (RGBA8 + w/h) and `Host.log!`.
- `host/host.zig` (~170 lines) + `host/roc_platform_abi.zig` (copied from roc-wasm4, entrypoint externs adapted) — Roc runtime hooks over `std.heap.wasm_allocator`, JS-facing exports `init` / `run_frame(buttons) -> i32` / `framebuffer_ptr` / `framebuffer_len`, single JS import `env.js_log`.
- `app/hello.roc` (gradient), `app/play.roc` (the **unchanged** roc-ngb-emu core from the sibling checkout, ROM embedded at build time), `app/acid2.roc` (oracle).
- `www/index.html` — WebGPU renderer (160×144 `rgba8unorm` texture, `queue.writeTexture` per frame, fullscreen triangle, nearest sampling) behind the binjgb-style two-method seam, Canvas2D fallback, keyboard → packed-u32 buttons.
- `www/test.js` — Node headless harness (instantiate, run N frames, framebuffer sanity, timing).

## Evidence per gate

**Milestone 1 — hello wasm: PASS.** `roc build app/hello.roc` produced a working module on the first successful compile; `Host.log!` reached JS; gradient pixels verified in Node.

**Milestone 2 — emulator frame: PASS.**
- Correctness: `app/acid2.roc` run 120 frames in wasm produces a framebuffer whose PPM digest is `88dcd6f4…` — **byte-identical to `golden/acid2.sha256`**, the native-verified reference. The wasm build renders pixel-perfectly.
- In-browser: Pokémon Blue (1 MB MBC ROM) visibly runs at `http://localhost:8642/` — verified headlessly via agent-browser (Vercel Labs CLI, headless Chrome 151): **WebGPU backend active**, intro → title → and Start press reaches the game (NEW GAME/OPTION menu), proving the input path end to end.

**Milestone 3 — benchmark: PASS.**

| Configuration | fps (Node, unthrottled) |
|---|---|
| roc `--opt=speed` (default) + Zig Debug host | 92.1 |
| roc `--opt=speed` + Zig ReleaseFast host | 94.3 |
| roc `--opt=size` | 86.1 |
| 10,000-frame sustained run | 88.3 |

- **Memory canary: flat.** 17.2 → 17.6 MB over 10,000 frames with periodic input — no refcount leak.
- Headless-browser rAF loop reported ~50 fps; likely headless compositing/software-GPU pacing, not the core (core is 92+ unthrottled). Confirm on a real browser; not gate-blocking.
- Module sizes: hello 17 KB; emulator ~820 KB with the 1 MB ROM embedded (267 KB with the 32 KB acid2 ROM).

## Friction log (the platform-authoring cost, measured)

1. **The `__multi3` saga — the only real debugging of the spike.** Roc-generated code references 128-bit compiler-rt builtins (`__multi3`) that nothing provides on wasm32-freestanding; unresolved symbols surface as bogus `env` imports. Fix: export it from the Zig host. Two traps inside the fix itself:
   - `a *% b` on `i128` in Zig lowers to a `__multi3` libcall — i.e. the shim recurses into itself.
   - In Debug builds even plain `*` on u64 limbs gets an overflow *check* that lowers to 128-bit multiply — recursion again. Final fix: schoolbook multiply with 32-bit limbs, `*%` everywhere. Symptom either way: `Maximum call stack size exceeded` with one function filling the whole stack.
   - Watch for siblings (`__udivti3`, `__lshrti3`, …) if future Roc code trips new builtins; same pattern applies. Debugging tool that cracked it: `roc build --debug` keeps a wasm name section → V8 stack traces show real symbol names.
2. **Platform caching:** after rebuilding `host.wasm`, `roc build` can reuse a cached platform — use `--no-cache` when iterating on the host.
3. **Nightly pairing is real:** the flake initially pulled nightly 08-10 while roc-ngb-emu pins 08-07; pinned roc-canvas's roc-overlay input to the same rev (`605963e0…`). Keep the pairing rule.
4. **Everything else just worked**: roc-wasm4's `targets:` header syntax, plain C-ABI entrypoints (`provides` names are the linker symbols), the copied `roc_platform_abi.zig`, `std.heap.wasm_allocator` (no umm_malloc/canaries needed), Zig 0.16.0 from nixpkgs.

## Exact versions

roc `nightly-2026-08-07-8d23662` (roc-overlay `605963e0`), Zig 0.16.0, roc-ngb-emu at the `core-debug-api` working tree (204 package tests passing), roc-wasm4 reference @ main 2026-08-09, agent-browser headless Chrome 151.

## Recommendation and follow-ups

Proceed to a real platform. The spike scaffold is worth keeping as the platform's skeleton (not throwaway):

1. **roc-canvas repo**: start its own OpenSpec; design the v1 platform API (audio out via a `Host.queue_audio!` + AudioWorklet, ROM staging region for runtime loading, storage effects for IndexedDB saves); adopt the compiler-rt shim set; publish a bundle (`roc bundle`) once stable.
2. **roc-ngb-emu**: a `web-app` change — real frontend (ROM picker via staging region, battery saves, debugger panes over the `core-debug` API landed today; speaker path for the APU).
3. Benchmark on a real (non-headless) browser and on a lower-power machine; 60 fps has ~35% headroom on this hardware, which is comfortable but not lavish for future consoles.
