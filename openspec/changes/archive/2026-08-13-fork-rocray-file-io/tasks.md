## 1. Fork and baseline

- [x] 1.1 Fork `lukewilliamboswell/roc-ray` to `ricardo-valero/roc-ray` (gh), clone to `~/dev/roc-ray`, add `upstream` remote, branch `file-io` from HEAD
- [x] 1.2 Build the unmodified host (`zig build`) and run one existing example headless as a baseline

## 2. File I/O effects in the fork

- [x] 2.1 Zig: `hostedReadBytesRaw` (64 MiB cap, `RocListWith(u8, false)` payload) and `hostedWriteBytesRaw` (atomic temp-file + rename, create-or-truncate, no implicit mkdir), both `@export`ed alongside the existing host effects
- [x] 2.2 Roc: `HostHost.roc` transport entries (`ReadBytesResult` record, write error code) and `Host.roc` public `read_bytes!` / `write_bytes!` with docs, mapping to `Try(List(U8), [NotFound, ReadFailed, ..])` and `Try({}, [WriteFailed, ..])`
- [x] 2.3 `examples/file_io.roc`: write bytes, read them back, assert equality; read a missing path and confirm `NotFound`; run headless (`roc build --no-cache` after host rebuilds)
- [x] 2.4 Commit on `file-io` with fork README note (nightly pairing, `--no-cache` lesson, macOS arm64 scope)

## 3. Runtime ROM loading here

- [x] 3.1 `app/ray.roc`: platform header → local fork path; drop the `import "../rom/play.gbc"` embed; resolve path from the first program argument via the fork's `args!` effect (default `rom/play.gbc`), load via `read_bytes!`, fail `init!` with an error naming the path when missing
- [x] 3.2 Drop the `rom/play.gbc` seeding from `fetch-roms`; update README play instructions (env var, no rebuild per ROM)
- [x] 3.3 Verify: play a real ROM, swap the file and relaunch without rebuilding, confirm missing-path error is actionable; suites stay green (Blargg, check-acid2)

## 4. Wrap up

- [x] 4.1 Retire the parked `rocray-file-io` change (remove the directory; this change's proposal records the supersession)
- [x] 4.2 Update WISHLIST.md: file-I/O half of the fork item done; audio remains, pointing at the fork
