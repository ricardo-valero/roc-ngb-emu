# Tasks — wasm-platform-spike

## 1. Recon (read-only, cheap, de-risks everything)

- [x] 1.1 Read roc-wasm4's build.zig + host source and the current Rocci Bird build; note the host hooks the new compiler requires, the Zig version, and how the platform declares wasm32 (see recon-notes.md)
- [x] 1.2 Read wasmboy's `test/core/save-state.js` and `core/constants.ts` to fix the target JS-side contract; sketch the spike's export list (init, run_frame, region pointers) (see recon-notes.md)
- [x] 1.3 Skim binjgb's web demo (C→wasm, hand-rolled JS glue, WebGL framebuffer blit — closest prior art for the renderer seam) and translate its pattern to WebGPU (`writeTexture` + fullscreen triangle in place of `texSubImage2D` + `drawArrays`); note seyhajin/webgpu-wasm-zig only as a future host-side-WebGPU reference (it requires Emscripten, which conflicts with our wasm32-freestanding + Roc linking; mach-gpu and rayray assessed 2026-08-10: not applicable — deprecated / native-only)

## 2. Milestone 1 — hello wasm (new repo)

- [x] 2.1 Create the repo (name decided at 1.1-time) with nix flake pinning the same Roc nightly as roc-ngb-emu plus the required Zig
- [x] 2.2 Minimal platform: `main.roc` declaring wasm32 only + Zig host with Roc runtime hooks; trivial app entrypoint returning bytes
- [x] 2.3 JS harness page instantiating the module and proving a Roc-computed value reaches the browser console/canvas

## 3. Milestone 2 — emulator frame

- [x] 3.1 Spike app consuming `../roc-ngb-emu/package/main.roc`: init embeds a ROM, `frame!` runs `GameBoy.run_frame` and returns the framebuffer
- [x] 3.2 Host copies framebuffer into a fixed export buffer; JS WebGPU renderer (`navigator.gpu`: `queue.writeTexture` per frame, fullscreen triangle, WGSL, nearest sampler) draws it behind the pluggable renderer seam — a game visibly runs in the browser with hardcoded input

## 4. Milestone 3 — benchmark and verdict

- [x] 4.1 Run the benchmark protocol from design.md (game ROM, 600-frame timing, opt variants, 10k-frame memory-growth canary)
- [x] 4.2 Write `report.md` in this change: numbers, friction notes, exact commits used, and a proceed/stop recommendation with the follow-up changes it implies
