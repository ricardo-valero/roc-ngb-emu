# Design — web-platform-v2

## Context

See proposal.md and the 2026-08-11 exploration. Reference points: `app/ray.roc` (the API shape being converged on), roc-web v0.1 (`platform/main.roc`, `host/host.zig`, `www/index.html` — the 140-line page whose audit showed ~90% platform code), roc-ray 0.9.0's `App.Init`/`host.key_down` ergonomics.

## Goals / Non-Goals

**Goals:** an app author writes ray-twin code; every GB-specific datum (screen size, scale, keymap choice, ROM) comes from the app, not the platform; sound works; one release (bundle + `lib/` archive) an app vendors as a pair.

**Non-Goals:** audio-driven frame pacing (v0.2 accepts rAF + ring-buffer slack; revisit if drift is audible); save states / IndexedDB persistence; the GPU-command contract surface; touch controls; roc-ray API changes.

## Decisions

- **`key_down` is pure, not an effect.** JS ships one key bitmask per frame (existing mechanism, widened to u32 with a documented Key-enum bit assignment); the platform passes it into `render!` inside the `Host` value, so `host.key_down(KeyX)` is a bit test. Mirrors roc-ray's non-`!` `key_down`, avoids 8 hosted calls per frame, and keeps the JS→wasm boundary at one scalar. The `Key` enum starts with a practical subset (~arrows, letters, enter/backspace/space/shift); growing it later is additive.
- **`Host` is a value passed to `render!`** holding the input snapshot; its `!` methods (`blit!`, `queue_audio!`, `log!`) wrap the hosted effects. Keeps app syntax identical to ray (`host.key_down(...)`, `host.blit!(...)`).
- **Config crosses as scalars via wasm exports.** `init_for_host` runs the app's config builder first; the Zig host stores width/height/scale/renderer/title and exposes them (`config_width()` etc.); `roc-web.js` reads them after instantiation to size the canvas, set the title, and construct the chosen renderer. Rationale: no record marshalling across the ABI, and JS needs the values before the first frame anyway. Title crosses via a bytes+len export pair.
- **Framebuffer sized from config.** The static 160×144×4 array becomes a host allocation of `width*height*4` at init; `framebuffer_ptr/len` semantics unchanged. This is the change that makes the platform genuinely console-agnostic.
- **ROM staging + init-with-bytes.** JS writes file bytes into a staging buffer (`rom_ptr()/rom_max_len()` exports), then calls `init(len)`; Zig builds a `RocList(U8)` and passes it to `init_for_host`. Re-loading a file calls `init` again (old boxed model decref'd) — reset for free. Default ROM: the page fetches a configurable URL (`start('play.wasm', { rom: 'play.gb' })`); picker and drag-drop are generic bytes-in concerns and live in `roc-web.js`.
- **Audio: `Host.queue_audio! : List(F32) => {}`** appends interleaved 48 kHz stereo into a fixed ring in the Zig host (exported ptr/len + read/write indices). The AudioWorklet cannot read wasm memory from its thread without SharedArrayBuffer (which demands COOP/COEP headers), so v0.2's transport is: the main thread copies each frame's samples out of the ring and `port.postMessage`s a transferable `Float32Array` to the worklet, which queues locally. Underflow plays silence; overflow drops oldest; the status line surfaces buffer health. Drift and SAB are the v0.3 candidates.
- **No JS Workers in v0.2 (considered, deferred).** A frame costs ~11 ms of the 16 ms budget on the benchmark machine's main thread; moving the emulator to a Worker buys jank isolation on slower devices but costs per-frame messaging or SharedArrayBuffer + cross-origin-isolation headers, breaking serve-with-any-static-server. Escalate to a Worker (and a SAB audio ring) in v0.3 only if real devices jank — wasmboy's 5-worker architecture remains the cautionary tale.
- **No dimension exists outside the app's config.** The page's `const W/H`, the `<canvas>` width/height attributes, and the CSS display size are all derived at boot from `config_width()/config_height()/config_scale()`; an app's `index.html` contains no numbers.
- **Renderer enum `[Auto, WebGPU, WebGL, Canvas2D]`** crossed as an integer; Auto tries WebGPU → WebGL → Canvas2D, explicit choices throw to the status line. The WebGL backend is new: same two-method seam, 256-free (NPOT is fine in WebGL2) `texSubImage2D` upload + quad, per binjgb.
- **The JS ships as native ES modules under top-level `lib/`** (a primary platform artifact, peer of `platform/` and `host/`; roc-web's own demo page in `www/` imports `../lib/`, so the demo is served from the repo root) — `roc-web.js` (entry/boot), `key-input.js`, `file-input.js`, `audio.js`, `audio-worklet.js` (AudioWorklet processors *must* be their own file, so single-file shipping was never possible), and `renderer/{webgpu,webgl,canvas2d}.js`, each small behind the two-method seam. Browsers load ES modules natively; no bundler enters the toolchain. **Apps vendor the `lib/` folder** (release asset = an archive of it), versioned in lockstep with the bundle URL — the pairing rule now covers bundle + lib + nightly. Alternative (CDN/URL import) rejected: offline dev and pinning both get worse.
- **Hello app moves to the new contract** in roc-web's own `www/`, serving as the platform's living example and Node-harness subject; the harness keeps working by calling the same exports (`init(len)` with an empty staged ROM for hello).
- **Planning home**: this change lives in roc-ngb-emu's OpenSpec (cross-repo precedent: `wasm-platform-spike`); roc-web adopts its own OpenSpec when a second consumer appears.

## Risks / Trade-offs

- [Richer init ABI (config builder + ROM list into `init_for_host`) is new ground on the nightly] → same technique as v0.1 (adapted glue externs); if a config-record return fights the ABI, fall back to config-as-hosted-setter-calls (`host_set_config(w,h,scale,renderer)`) — decided at implementation without changing the app-facing API.
- [Audio/video drift under rAF pacing] → accepted non-goal; ring depth absorbs jitter; audio-driven pacing is the known v0.3 candidate.
- [AudioWorklet requires secure context + user-gesture autoplay rules] → localhost is a secure context; resume audio on first keydown/click, standard practice.
- [Breaking v0.1] → only consumer is roc-ngb-emu, updated in the same change; version discipline via the release pairing.

## Open Questions

- Whether `blit!` keeps taking width/height args (redundant with config) or drops to just pixels — decide when writing `App.roc`; cosmetic for the app.
- Exact Key enum membership beyond the GB-needed eight — additive later, non-blocking.

## Round two (user review after hearing it): audio v2 — pull-paced, ring-free

v0.2.0's audio worked but was messy in both senses: rAF-paced production
(~50 fps ticks) chronically underfed the 48 kHz consumer (audible), and
the Zig ring + read/write indices + JS modulo-pump was three pieces of
bookkeeping for one buffer. Redesign, prompted by "consider raylib's
AudioStream and wasmboy's worklet architecture":

- **Audio-driven pacing (wasmboy's `executeFrameAndCheckAudio`, raylib's
  backpressure)**: each rAF tick runs emulated frames *until the worklet
  reports enough queued audio* (target ~60 ms, cap 4 frames/tick), then
  presents the latest frame. Emulation locks to the audio clock; video
  tags along. Apps that never queue audio fall back to 1 frame per tick.
- **Granular JS imports replace the ring**: `host_queue_audio` calls an
  imported `env.js_audio_push(ptr, len)` synchronously (wasm memory is
  stable during the call); JS copies once and posts the transferable to
  the worklet, whose internal queue is now the *only* buffer (capped
  ~250 ms, drop-oldest). The Zig ring, its exports, and the pump loop are
  deleted. Backpressure flows back as queued-ms via the worklet port.
- **Web-API note (user question)**: wasm still cannot call browser APIs
  directly — imports are the mechanism (the 2016 answer remains true;
  WebIDL-bindings died, the component model doesn't cover Web APIs). This
  design leans into that: more granular imports = less JS orchestration.
- Roc-facing API unchanged (`Host.queue_audio!`); apps unaffected beyond
  the platform URL bump. Ships as roc-web v0.2.1.
