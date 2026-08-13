# Fork roc-ray: PCM Audio Streaming

## Why

The APU change landed a full four-channel mixer that already emits 48 kHz interleaved stereo F32 samples (`GameBoy.take_samples`, golden-WAV-verified by `check/sound`), but the play app is silent: the roc-ray fork's audio surface is file-based `Sound`/`Music` plus procedural `gen_sound!` — there is no way to push emulator-generated PCM to the speakers. This executes the fork's second slice, explicitly deferred by `fork-rocray-file-io`: a raylib `AudioStream`-backed PCM path, with the Roc-side API shaped so roc-web can later satisfy it with Web Audio.

## What Changes

- In the fork (`~/dev/roc-ray`, branch `file-io`): add `Audio.Stream` — create a host-owned PCM stream (48 kHz, stereo, F32), push interleaved sample buffers from Roc each frame, and let the host own buffering so frame-rate pushes decouple from the audio device callback. Same conventions as the existing audio surface: `platform/Audio.roc` receiver-style API over an `AudioHost` `Box(U64)` resource, raylib calls in `src/backend_raylib.zig` (`LoadAudioStream`/`UpdateAudioStream`), hand-written ABI additions in `src/roc_platform_abi.zig` (roc glue cannot link on this machine).
- Host buffering policy: the host keeps a ring buffer between Roc pushes and the device; underrun plays silence (no crash, no pitch warble), overrun drops the oldest samples. Roc can read the buffered depth to let apps regulate.
- Here: `app/ray.roc` drains `GameBoy.take_samples` every frame and pushes to the stream, so game audio plays in the native window.
- Out of scope: roc-web's Web Audio implementation of the same API (its own change in that repo), audio in `app/web/main.roc`, battery saves, roc-nes-emu adoption.

## Capabilities

### New Capabilities

_None — the stream API lives in the fork repo; this repo's spec-level change is confined to the play app gaining audible sound._

### Modified Capabilities

- `play-app`: gains an audio-output requirement — the play app SHALL feed APU samples to a host PCM stream each frame so game audio is audible, with defined behavior when sample production and playback drift (host-side buffering, silence on underrun).

## Impact

- **Fork repo (`~/dev/roc-ray`)**: `platform/Audio.roc`, `platform/AudioHost.roc`, `src/backend_raylib.zig`, `src/host_native.zig`, `src/roc_platform_abi.zig`, plus an example exercising a generated tone through the stream. Build quirks apply: host builds in roc-web's nix shell (`nix develop ~/dev/roc-web -c zig build`), then `roc build --no-cache`.
- **Here**: `app/ray.roc` (drain-and-push in `render!`), README play instructions gain a sound note.
- **Risk**: latency/underrun tuning is the only genuinely new ground — raylib's default stream buffer size may need `SetAudioStreamBufferSizeDefault` before stream creation; macOS arm64 is the only target we own, matching the fork's stated support.
