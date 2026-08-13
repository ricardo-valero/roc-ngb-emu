# Design: Fork roc-ray PCM Audio Streaming

## Context

See proposal.md for motivation. The fork's existing audio surface sets every convention this slice follows: `platform/Audio.roc` exposes receiver-style nominal types over `AudioHost` `Box(U64)` resources; `src/host_native.zig` holds a `HostResourceHeap` per resource kind (token-tagged slots, `roc_dealloc` routing destroys the native value); raylib calls live behind `src/backend_raylib.zig`; and ABI additions are hand-written in `src/roc_platform_abi.zig` to glue conventions (record fields alphabetical) because `roc glue` cannot link on this machine. The host builds in roc-web's nix shell (`nix develop ~/dev/roc-web -c zig build`), and app rebuilds need `roc build --no-cache` afterwards.

On the consumer side, `GameBoy.take_samples` drains the APU's accumulated 48 kHz interleaved stereo F32 samples (~800 pairs per video frame), and `app/ray.roc`'s `render!` already runs once per frame.

## Goals / Non-Goals

**Goals:**
- One new resource kind, `Audio.Stream`, with the minimal surface an emulator needs: create, push samples, read buffered depth.
- Host-owned ring buffer so Roc pushes at frame cadence while the device pulls at callback cadence; bounded memory; silence on underrun.
- An API contract implementable by roc-web later (Web Audio `AudioWorklet` + ring buffer) without Roc-side changes.

**Non-Goals:**
- Multiple sample formats or rates per stream beyond creation-time configuration; F32 interleaved is the only sample encoding.
- Resampling, effects, or mixing multiple Roc-side streams (raylib mixes streams with sounds/music natively; one stream suffices here).
- Web implementation, `app/web` audio, upstream contribution.

## Decisions

1. **F32 stream (raylib `sampleSize = 32`), not I16.** The APU emits F32; raylib's miniaudio backend supports 32-bit float streams natively, and Web Audio is F32-only — so F32 end to end avoids a conversion now and a format fork later. Alternative (I16, matching `gen_sound!`'s buffer) rejected: two lossy conversions for nothing.

2. **Creation-time config record, fixed encoding: `Audio.Stream.create!({ sample_rate : U32, channels : U8 })`.** The emulator passes `{ sample_rate: 48000, channels: 2 }`. Keeping rate/channels as data (rather than hardcoding) costs nothing in the host and keeps the API honest for other apps; fixing F32 keeps the ABI to one push entry point. Errors mirror the existing audio results (`err` byte → `StreamCreateFailed` / `ResourceLimit`).

3. **Host ring buffer between the Roc push and the device, drained by the audio thread itself (`SetAudioStreamCallback`).** `push!` only appends to a Zig-owned, spinlock-guarded ring (no raylib call on that path); raylib's audio-thread callback fills each device request straight from the ring and zero-fills whatever the ring cannot cover, so underrun is exact per-sample silence. This is the same architecture as roc-web's `AudioWorklet` (its `process()` pulls from the worklet queue on the audio thread) and it makes delivery independent of frame pacing: render jitter cannot starve playback while the ring has data. Two earlier cuts fell short and are recorded here as constraints: a per-frame `UpdateAudioStream` pump must only ever submit *whole* sub-buffers (raylib marks a sub-buffer ready however few frames were written, so partial updates play its stale tail as garbage), and even a correct pump ties delivery deadlines (~21 ms) to the render loop, which stutters audibly under frame jitter. Overrun still overwrites the oldest ring contents, honoring the spec's bounded-memory requirement. The lock is a try-only spinlock (`std.atomic.Mutex`) — critical sections are microsecond memcpys and the audio thread must never futex-sleep.

4. **Ring capacity ~250 ms (12 000 frames at 48 kHz), callback requests 1024 frames (~21 ms).** `SetAudioStreamBufferSizeDefault(1024)` before device init sets the callback request granularity. `buffered!` returns queued frame count, and consumers are expected to regulate against it: the emulator paces emulation by the audio clock (run frames until ~60 ms queued, bounded per tick — wasmboy-style, exactly what roc-web's pacer does with the worklet's queued-ms report), which also absorbs the 60 Hz render cap vs 59.73 Hz Game Boy drift as an occasional repeated video frame instead of audio drops.

5. **New `StreamHeap` in `host_native.zig`, capacity 4, next free token tag.** Same `HostResourceHeap` machinery; destroy unloads via `UnloadAudioStream` and frees the ring allocation. Capacity 4 because streams are per-app singletons in practice.

6. **Push payload is `List(F32)` by value (`RocListWith(f32, false)`), mirroring `write_bytes!`'s byte-list transfer.** The host copies into the ring during the call, so Roc retains ownership and ARC semantics stay identical to the file-io precedent.

## Risks / Trade-offs

- [Latency: app target (~60 ms) + device buffers (~43 ms) ≈ 100 ms] → In line with the web app (60 ms worklet target + output latency); tune `target_depth` and `STREAM_DEVICE_BUFFER_FRAMES` by ear if needed.
- [CGB double speed: APU ticks on halved `video_cycles`, so sample rate stays real-time] → Already handled in `GameBoy.tick`; no app-side compensation needed. Verify by ear on a CGB ROM.
- [`render!` pacing is capped 60 fps but not exactly 59.7 Hz (Game Boy frame rate)] → Resolved by audio-clock pacing (decision 4): the tick that finds the queue already at target runs zero emulated frames, repeating the last video frame (~once every 4 s); audio never drops.
- [Hand-written ABI drift] → Same mitigation as file-io: record fields laid out alphabetically, smoke-tested via the fork's example before touching the emulator.

## Migration Plan

Fork-first, same recipe as `fork-rocray-file-io`: land and verify in `~/dev/roc-ray` (branch `file-io`) with a standalone example (`examples/audio_stream.roc`, generated tone pushed through the stream), then point `app/ray.roc` at the already-local platform path — no release cut needed; rollback is reverting the app commit.

## Open Questions

- Exact ring capacity and device buffer size — set the defaults above, tune by ear during the example task.
