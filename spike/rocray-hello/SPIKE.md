# Spike: roc-ray on the pinned toolchain

**Date:** 2026-08-09
**Outcome: works — no flake change required.**

| Component | Version |
|---|---|
| Flake toolchain | `nightly-2026-08-07-8d23662` (via roc-overlay) |
| roc-ray release | `0.9.0` (default bundle, `3sKTYuHvxSV…tar.zst`) |
| roc-ray's declared nightly | `nightly-2026-August-05-24f0b47` |

Result of `roc build spike/rocray-hello/main.roc` + running the binary on
macOS (Apple Silicon): window opens, raylib 6.0 initializes (GLFW/Cocoa,
Metal-backed GL 4.1, audio device up), render loop runs at 60fps pacing,
ESC exits. The two-day nightly skew between our pin and roc-ray's declared
pairing caused no issues.

Notes for the future rendering milestone:

- Pin the pair together: when bumping the flake nightly, check roc-ray's
  `.roc-version` on the matching release and bump both in one commit.
- roc-ray's `program = { init!, render! }` model fits the emulator core
  directly: `Model` holds the `GameBoy`, `render!` runs a frame's worth of
  `step` calls and blits the framebuffer (mutable-pixel textures exist in
  the API for exactly this).
- Frame pacing via `App.default.with_frame_pacing(Capped(60))`.
