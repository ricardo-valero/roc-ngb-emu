# Tasks — web-platform-v2

## 1. roc-web platform (Roc side)

- [x] 1.1 `platform/App.roc`: config builder (`default`, `with_title`, `with_screen`, `with_scale`, `with_renderer`) and the `App.Init` shape; `platform/Host.roc` reworked: `Key` enum, pure `key_down`, `blit!`, `queue_audio!`, `log!`
- [x] 1.2 `platform/main.roc`: `program = { init!, render! }` contract; `init_for_host` runs config + ROM-bytes callback; `render_for_host` passes the key snapshot in the `Host` value

## 2. roc-web host (Zig side)

- [x] 2.1 Config storage + exports (`config_width/height/scale/renderer`, title bytes); dynamically allocated framebuffer
- [x] 2.2 ROM staging buffer (`rom_ptr/rom_max_len`, `init(len)` building the RocList; re-init decrefs the old model); key-snapshot pass-through into `render_for_host`
- [x] 2.3 Audio ring (fixed F32 ring + read/write index exports) filled by `host_queue_audio`

## 3. roc-web JS (top-level `lib/` ES modules)

- [x] 3.1 Split into modules: `roc-web.js` entry (boot from config exports — canvas size/scale/title/renderer, no dimension constants anywhere), `key-input.js` (keymap → Key bitmask), `renderer/{webgpu,canvas2d}.js` extracted + new `renderer/webgl.js`
- [x] 3.2 `file-input.js` (fetch default, file picker, drag-drop → staging buffer → `init(len)`); `audio.js` + `audio-worklet.js` (main thread posts transferable Float32Array chunks from the ring; resume on first gesture); status line (backend · fps · audio health)
- [x] 3.3 Hello app + `www/index.html` on the new contract (served from repo root so `../lib/` resolves); Node harness updated; verify headless

## 4. Release

- [x] 4.1 `zig build -Doptimize=ReleaseFast`; `roc bundle` from `platform/`; GitHub release v0.2.0 with bundle + `lib/` archive; README updated (contract example, pairing rule incl. the lib asset)

## 5. roc-ngb-emu app

- [x] 5.1 `app/web/main.roc` on the v0.2 URL: config init, `render!` with `host.key_down` buttons (ray-twin), `queue_audio!`; delete the ROM embed and `decode_buttons`
- [x] 5.2 `app/web/index.html` → boot script + vendored `lib/` folder; serve `rom/play.gb` as the default fetch ROM; README (browser section: no rebuild per ROM, picker/drag-drop, sound)
- [x] 5.3 Verify: node harness frames still pixel-correct (acid2 digest via staged ROM); in-browser via agent-browser — game boots from fetched ROM, input works, backend reported; sound verified by ear or buffer-health telemetry
