# Fork roc-ray: Binary File Read/Write

## Why

roc-ray 0.9.0's host exposes only UTF-8 `read_file!` — no binary reads, no writes — so the play app embeds `rom/play.gbc` at build time (one rebuild per ROM swap) and battery saves are impossible. The parked `rocray-file-io` change bet on an upstream contribution whose timeline we don't own; the wishlist (2026-08-11) already resolved to fork instead, because roc-web proved we own the platform-release muscle (Zig host, `roc bundle`, nightly pairing). This change executes the fork's first slice: binary file I/O. PCM audio is explicitly deferred to a follow-up change on the same fork.

## What Changes

- Fork `lukewilliamboswell/roc-ray` to `ricardo-valero/roc-ray`, cloned at `~/dev/roc-ray`, branched (`file-io`) from upstream HEAD (one commit past the 0.9.0 tag, same declared nightly `nightly-2026-August-05-24f0b47`, which the repos' pinned `2026-08-07` nightly already runs).
- In the fork: add `read_bytes! : Host, Str => Try(List(U8), [NotFound, ReadFailed, ..])` and `write_bytes! : Host, Str, List(U8) => Try({}, [WriteFailed, ..])`, mirroring the existing `read_file!` plumbing (`Host.roc` → `HostHost.roc` → Zig `@export`ed effect in `src/host_native.zig`, byte payloads as `RocListWith(u8, false)`).
- Here: `app/ray.roc` loads the ROM at runtime via `read_bytes!` (path from a runtime source, not compile time) instead of `import "../rom/play.gbc" as rom`; the platform URL moves from the upstream 0.9.0 bundle to the fork (local path during development, fork release bundle once cut).
- Supersedes and retires the parked `rocray-file-io` upstream-contribution change.
- Out of scope: PCM audio streaming (next change on the fork), battery-save adoption in the core/app (needs `write_bytes!`, which this change delivers, plus core cart-RAM exposure — its own change), roc-nes-emu adoption (same recipe, applied in that repo).

## Capabilities

### New Capabilities

_None — the new host effects live in the fork repo; this repo's spec-level change is confined to how the play app sources its ROM._

### Modified Capabilities

- `play-app`: the "Windowed play of an embedded ROM" requirement changes — ROM bytes come from a runtime binary file read (with actionable failure behavior) instead of build-time embedding; `fetch-roms` no longer needs to seed a rebuild.

## Impact

- **Fork repo (`~/dev/roc-ray`)**: `platform/Host.roc`, `platform/HostHost.roc`, `src/host_native.zig`, plus an example exercising a read/write round-trip. Outward-facing: public GitHub fork under `ricardo-valero`.
- **Here**: `app/ray.roc` (runtime load, drop the embed import), platform reference in the app header, README play instructions, `fetch-roms` seeding note.
- **Versioning**: fork releases pair with a named Roc nightly exactly like roc-web; first release pairs with the currently pinned nightly. Bump flake + platform URL together, as always.
- **Risk**: owning host builds for macOS arm64 only (the machine we ship on) — deliberately not upstream's full target matrix. Upstream merges stay cheap while the diff is additive.
