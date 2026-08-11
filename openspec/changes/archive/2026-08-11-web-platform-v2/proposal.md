# roc-web v0.2: ray-shaped platform API (input, config, ROM loading, audio)

## Why

The v0.1.0 spike contract leaks plumbing into the app: buttons cross as a packed u32 the app must bit-decode, the screen size/scale/keymap/renderer live hardcoded in a 140-line `index.html` that is ~90% generic platform code, ROMs embed at build time, and the APU still has no speaker. roc-ray already demonstrates the right shape — config-driven `init!`, a `render!` that queries `host.key_down(KeyX)` — and converging on it gives both frontends one mental model (explored 2026-08-11; the target `app/web/main.roc` is a near-twin of `app/ray.roc`).

## What Changes

**roc-web (platform, → release v0.2.0):**

- **App contract becomes `program = { init!, render! }`** (ray-shaped). `init!` takes an `App` config — title, screen `{width, height}` (no more hardcoded 160×144: the platform goes truly console-agnostic), display scale, renderer preference — plus a callback receiving the **runtime ROM/file bytes** as `List(U8)`. `render!` receives the model and a `Host` value.
- **Input as pure queries**: JS sends one key-bitmask per frame (mechanism unchanged, widened); the platform wraps it so apps write `host.key_down(KeyX)` — pure, snapshot-based, exactly like roc-ray. A `Key` enum covers a practical keyboard subset; the app owns which key means what.
- **Renderer selection** via config: `[Auto, WebGPU, WebGL, Canvas2D]`. Auto tries in that order; explicit choices fail loudly. A **WebGL backend is written** (third implementation behind the existing two-method seam; binjgb's texSubImage2D pattern).
- **`Host.queue_audio!`**: interleaved 48 kHz stereo F32 into a host ring buffer, drained by an AudioWorklet — the speaker path.
- **`roc-web.js` ships as a release asset**: renderers, wasm glue, keymap→Key mapping, ROM fetch/file-picker/drag-drop, AudioWorklet, boot-from-config-exports. An app's `index.html` becomes a few lines importing it.
- Zig host: key snapshot pass-through, config exports for JS, ROM staging buffer, audio ring, dynamically sized framebuffer.

**roc-ngb-emu (app):**

- `app/web/main.roc` rewritten on the v0.2 contract (the ray.roc-twin sketched in the exploration); build-time ROM embed gone — the page loads `rom/play.gb`-equivalent via fetch/picker/drag-drop; APU samples reach the speaker.
- `app/web/index.html` shrinks to a boot script + vendored `roc-web.js` (versioned with the platform release, same pairing rule as the bundle URL).
- README updates; node harness stays for headless frame verification (no audio/rAF there).

## Capabilities

### New Capabilities

- `web-app`: the browser frontend's behavior — runtime ROM loading (fetch default, picker, drag-drop), keyboard input reaching the joypad, rendered frames via the configured backend, and audio output.

### Modified Capabilities

_None — `play-app` (native window) is untouched; platform-side requirements live with roc-web (its own repo; specs there when it adopts OpenSpec)._

## Impact

- roc-web: `platform/main.roc` (+ new `App.roc`, reworked `Host.roc`), `host/host.zig`, new `www/roc-web.js` (+ worklet), `www/index.html` (hello page on the new contract), release v0.2.0 (bundle + JS asset). Breaking change for v0.1 consumers — acceptable pre-1.0, and the only consumer is us.
- roc-ngb-emu: `app/web/{main.roc,index.html}` (+ vendored `roc-web.js`), README.
- Risks (design.md): audio/video clock drift under rAF pacing; Roc↔host ABI for the richer init (config record crossing); AudioWorklet requires a secure context (localhost is fine).
