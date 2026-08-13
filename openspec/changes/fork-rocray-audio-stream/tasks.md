## 1. Stream resource in the fork

- [x] 1.1 Zig backend (`src/backend_raylib.zig`): `SetAudioStreamBufferSizeDefault(4096)` before device init, `loadStream(sample_rate, channels)` (F32, `LoadAudioStream(rate, 32, channels)`), `unloadStream`, and a per-stream ring buffer (~12 000 frames) with append (drop-oldest on overrun), buffered-depth, and a pump that feeds `UpdateAudioStream` when `IsAudioStreamProcessed`
- [x] 1.2 Zig host (`src/host_native.zig`): `StreamHeap` (`HostResourceHeap`, capacity 4, next token tag, destroy = unload + free ring), `@export`ed effects `hostedAudioStreamCreate` (err byte: 1 failed / 2 limit), `hostedAudioStreamPush` (`RocListWith(f32, false)`, copy into ring), `hostedAudioStreamBuffered`; wire the ring pump into the existing per-frame audio update next to music streams; drain heap in shutdown/reset paths
- [x] 1.3 ABI (`src/roc_platform_abi.zig`): hand-written entries for the three effects, record fields alphabetical, matching the file-io additions in style

## 2. Roc API in the fork

- [x] 2.1 `platform/AudioHost.roc`: `Stream :: Box(U64)`, `StreamResult : { err : U8, stream : Stream }`, transport signatures for create/push/buffered
- [x] 2.2 `platform/Audio.roc`: `Stream` nominal with docs — `create! : { sample_rate : U32, channels : U8 } => Try(Stream, [StreamCreateFailed, ResourceLimit, ..])`, `push! : Stream, List(F32) => {}`, `buffered! : Stream => U64` (queued frames)
- [x] 2.3 `examples/audio_stream.roc`: generate a sine in Roc, push chunks per frame, confirm audible tone and clean exit; tune ring/device buffer sizes by ear here (design open question)
- [x] 2.4 Host rebuild in roc-web's shell + `roc build --no-cache` example run; run `zig build test` (heap tests cover the new resource kind); commit on `file-io` with README audio note

## 3. Emulator sound here

- [x] 3.1 `app/ray.roc`: create the stream in `init!` (`{ sample_rate: 48000, channels: 2 }`, propagate failure with an actionable message), keep it in the model; in `render!` drain `GameBoy.take_samples` and `push!` the samples
- [ ] 3.2 Verify by ear: Pokémon Crystal title fanfare plays in sync with video; window drag stalls to silence and resumes at correct pitch (spec scenarios); a CGB double-speed section still sounds right
- [x] 3.3 Suites stay green (`nix run .#check-blargg`, `.#check-mooneye`, `.#check-acid2`, sound check) and README play section mentions audio

## 4. Wrap up

- [x] 4.1 Update WISHLIST.md: fork audio item done; note roc-web Web Audio as the remaining platform gap for `app/web`
