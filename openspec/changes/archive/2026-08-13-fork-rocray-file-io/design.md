# Design: fork-rocray-file-io

## Context

See proposal.md — Why. Relevant mechanics of roc-ray at upstream HEAD (`8ea41e0`, one commit past 0.9.0, `.roc-version` = `nightly-2026-August-05-24f0b47`):

- Host effects are plain exported symbols: a `hosted…`/`exported…` fn in `src/host_native.zig` registered via `@export(&fn, .{ .name = "roc_host_…" })`, surfaced to Roc through `platform/HostHost.roc` (internal transport module) and wrapped with docs and error mapping in `platform/Host.roc` (public, receiver-form methods on `Host`).
- The existing text `read_file!` is the template: Zig `hostedReadFileRaw` reads via `readFileAlloc` capped at `MAX_HOST_TEXT_FILE_BYTES` (16 MiB) and returns a `{ ok, err, contents }` record; `HostHost.ReadFileResult` mirrors it; `Host.read_file!` maps `err` codes to `[NotFound, ReadFailed]`.
- Byte payloads cross the ABI as `RocListWith(u8, false)` (`src/roc_platform_abi.zig` has `fromSlice`; textures already pass pixel lists this way).
- roc-web (same owner) established the release discipline this fork inherits: `zig build -Doptimize=ReleaseFast`, `roc bundle`, attach `<hash>.tar.zst` to a GitHub release, pair with a named nightly. After rebuilding the host, `roc build` needs `--no-cache`.

## Goals / Non-Goals

**Goals:**
- Binary read and write host effects in the fork, shaped so both emulators (and any roc-ray app) can use them.
- Play app loads ROMs at runtime with zero rebuilds; the embed import and its `fetch-roms` seeding step die.
- Fork stays cheaply mergeable with upstream (additive diff, upstream conventions).

**Non-Goals:**
- PCM audio (next change on the fork branch-from-`main` model).
- Battery-save adoption in the play app (needs core cart-RAM exposure; `write_bytes!` merely unblocks it).
- roc-nes-emu adoption (same recipe, executed in that repo).
- Multi-target host builds or upstream PR (macOS arm64 is the only shipped target; upstreaming stays open as a courtesy option since the diff is additive).

## Decisions

- **Branch from upstream HEAD, not the 0.9.0 tag.** HEAD is one harmless merge (window clipboard/types) past 0.9.0 with the same declared nightly, and branching from HEAD keeps `git merge upstream/main` trivial. The emulators' pinned `2026-08-07` nightly already runs the 0.9.0 bundle declared against `2026-August-05`, so the pairing holds.
- **Mirror `read_file!`'s transport shape rather than inventing one.** `HostHost.ReadBytesResult : { ok : Bool, err : U8, contents : List(U8) }` and a `write_bytes!` returning an error code, with `Host.roc` mapping to `Try(List(U8), [NotFound, ReadFailed, ..])` and `Try({}, [WriteFailed, ..])`. Alternative — `Try` directly in HostHost as `read_env!` does — rejected: the record path is the proven one for exactly this call shape, and consistency with the neighboring `read_file!` keeps the diff reviewable.
- **Separate size cap for binary reads:** `MAX_HOST_BINARY_FILE_BYTES = 64 MiB` (largest GB carts are 8 MiB; headroom is free). Reusing the 16 MiB text cap would silently couple two unrelated limits.
- **`write_bytes!` is atomic replace:** write to `<path>.tmp` in the same directory, then rename over the target. Battery saves are the motivating consumer; a crash mid-write must not corrupt the only copy of a save. Create-or-truncate semantics, parent directory must exist (no implicit `mkdir -p` — an error surfaces a typo'd path instead of scattering directories).
- **ROM path via the first program argument, default `rom/play.gbc`.** Upstream roc-ray's host owns argv and rejects unknown arguments, so the first cut used an `NGB_ROM` env var (`read_env!` was the only runtime input channel). The fork then grew a `Host.args!` effect — positional argv entries and everything after `--` reach the app; host flags (`--headless` etc.) parse as before — and the env var was dropped in favor of `./ray game.gb`. A missing file crashes `init!` with a message naming the resolved path and the fix — the platform's init-failure path is otherwise silent (exit 255).
- **Local-path platform reference during development** (`ray: platform "../roc-ray/platform/main.roc"` from the app), switching to the fork's release-bundle URL when the first release is cut. This is how roc-web consumers develop already; it avoids cutting a release just to iterate.

## Risks / Trade-offs

- [Fork nightly drifts from the emulators' pin] → Same discipline as roc-web: a fork release names its nightly; bump the emulator flake and platform URL together. The fork repo keeps `.roc-version` authoritative.
- [Stale roc cache after host rebuilds masks Zig changes] → Known roc-web lesson: `roc build --no-cache` after any `zig build`; record it in the fork README.
- [Upstream diverges and merges stop being trivial] → The diff is confined to two Roc modules and one Zig file's additive blocks; if upstream restructures, the effects are small enough to re-port by hand.
- [`RocListWith(u8, false)` ownership mistakes (leak or double-free at the ABI edge)] → Follow the exact incref/decref pattern of neighboring effects (`defer path_arg.decref`, transfer ownership of the returned list); verify with the round-trip example run headless.

## Migration Plan

1. Fork on GitHub, clone to `~/dev/roc-ray`, branch `file-io`, build the host unchanged, run an existing example (baseline).
2. Land the effects + a `file_io` example (write bytes, read them back, compare, also read a deliberately missing path); verify headless.
3. Point `app/ray.roc` at the local platform path, replace the embed with `read_bytes!`, delete the `fetch-roms` seeding step; play a real ROM.
4. Cut fork release `0.10.0-fork.1` (bundle + nightly note) when local-path development gets annoying or roc-nes-emu wants in; move the app header to the URL.
5. Rollback at any step: the upstream 0.9.0 bundle URL and the embed import are one revert away until step 4 archives.

## Open Questions

- When to cut the first bundled fork release (step 4 timing) — safely deferrable; local path works indefinitely on this machine.
- Whether to offer `read_bytes!`/`write_bytes!` upstream once proven — costless to decide later; the diff stays additive either way.
