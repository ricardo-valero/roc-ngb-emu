# roc-ray Binary File IO (Upstream + Adoption)

> **Status: deferred.** Drafted 2026-08-09 while the play-app change landed. Not scheduled; pick up when the embedding workflow actually hurts (ROM picker, battery saves) or when upstream shows interest. Design and delta specs are intentionally not authored yet — they depend on the API shape upstream accepts.

## Why

roc-ray 0.9.0's host exposes only UTF-8 `read_file!` — no binary reads, no writes. The play app therefore embeds `rom/play.gb` at build time, which costs a rebuild per ROM swap and blocks two future features outright: a runtime ROM picker, and battery-backed save persistence (MBC cartridge RAM), which needs binary *writes*. Forking or vendoring roc-ray was evaluated and rejected: their releases are prebuilt multi-target platform bundles paired to Roc nightlies, and owning that pipeline is a second project. The right move is a small upstream contribution.

## What Changes

- Upstream (lukewilliamboswell/roc-ray): propose and contribute `read_bytes! : Host, Str => Try(List(U8), ...)` mirroring the existing `read_file!` plumbing (`Host.roc` → `HostHost.roc` → Zig host effect), and open the conversation about a `write_bytes!` counterpart for save persistence.
- Same conversation, second ask: a raw PCM audio-stream API (raylib's `AudioStream`/`UpdateAudioStream` exposed through the platform) — the APU core (2026-08-09) generates 48 kHz stereo samples that currently have no speaker path; roc-ray 0.9.0's audio API is file/tone-based only.
- Here, once released: `example/play.roc` loads the ROM at runtime (path via config or a simple picker) instead of build-time ingestion; `fetch-roms` no longer needs to seed `rom/play.gb`.
- Version discipline unchanged: adopting the new roc-ray release pairs with its declared Roc nightly (bump flake + platform URL together).

## Capabilities

### New Capabilities

_None here until adoption; the upstream work itself changes no repo capability._

### Modified Capabilities

- `play-app` (when adopted): ROM loading requirement changes from build-time embedding to runtime file read. Delta spec to be authored once the upstream API shape is final.

## Impact

- **Upstream**: Zig host + platform Roc modules in a roc-ray checkout; PR from the maintainer's contribution flow. Outward-facing: diff reviewed before submission.
- **Here**: `example/play.roc`, `nix/fetch-roms.nix`, flake pin + platform URL bump.
- **Risk**: upstream timeline is not ours; nothing blocks meanwhile — embedding works today.
