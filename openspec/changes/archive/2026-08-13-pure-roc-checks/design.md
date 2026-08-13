# Design: Pure-Roc Checks

## Context

See proposal.md for motivation. Current state: every runner is already a basic-cli Roc app (`check/acid2/main.roc`, `check/sound/main.roc`, the shared suite runner in `check/lib/Harness.roc` used by blargg and mooneye), goldens hash with the pure-Roc `check/lib/Sha256`, and gating lives in committed passlists. The nix layer is four `writeShellApplication` wrappers (`roc build && run` with nix-store ROM paths) plus `check/gb-test-roms.nix` (retrio/gb-test-roms pinned by commit and hash) and equivalent pins for mooneye and the acid2 ROMs. The sibling pattern to converge on is roc-nes-emu's: `roc check/<name>/fetch.roc` downloads ROMs into `check/<name>/data/` (untracked), `roc check/<name>/main.roc -- ...` runs the check.

## Goals / Non-Goals

**Goals:**
- One invocation style across both emulators: `roc check/<name>/fetch.roc` once, `roc check/<name>/main.roc` thereafter, from the devshell.
- Keep every pinning guarantee nix provided: same commits, hash-verified files.
- Flake shrinks to the devshell; no check logic outside `check/`.

**Non-Goals:**
- Changing what any check verifies (passlists, goldens, budgets, protocols).
- Committing ROM binaries (the sibling keeps them untracked; so do we).
- CI wiring, roc-nes-emu changes.

## Decisions

1. **Per-slice `fetch.roc`, sibling-shaped, plus SHA-256 verification.** Each fetch app lists `{ name, url_path, sha256 }` and downloads from commit-pinned `raw.githubusercontent.com` URLs (retrio/gb-test-roms at the commit `gb-test-roms.nix` pins today; same for mooneye and acid2 sources). After writing each file it re-reads and hashes with `check/lib/Sha256`, deleting and failing on mismatch — this keeps nix's content-addressing, which the sibling's fetch lacks. Already-present files that hash correctly are skipped, so re-running is cheap and offline-safe.
2. **ROM location: `check/<name>/data/`, untracked, each with a local `*` + `!.gitignore` .gitignore so contents stay out of git while the directory survives a fresh clone.** Every slice fetches: blargg and sound from retrio/gb-test-roms raw URLs (plain files, served directly); acid2 from its GitHub release assets, following `Location` redirect hops manually in the fetch app (the http package returns the 302 as-is — verified empirically); and mooneye from gekkio.fi's official archive. Mooneye's upstream ships no individual files — only archives (`.tar.xz`, `.tar.gz`, `.zip`) — so its fetch downloads the pinned `mts-20240926-1737-443f6e1.tar.gz`, verifies the archive SHA-256, and unpacks it with two new pure-Roc `check/lib` modules: `Inflate` (DEFLATE/gzip decoding, a port of zlib's reference decoder `puff`) and `Tar` (POSIX header walk). xz was rejected as an implementation target (LZMA is an order of magnitude more complex than DEFLATE); the `.tar.gz` makes it unnecessary. Every extracted ROM is verified against its own pinned SHA-256, same as the direct fetches. The blargg and sound slices share the same upstream repo but fetch independently — duplication of a few entries beats a cross-slice dependency.
3. **Runners default their ROM arguments to `data/`.** Today the nix wrapper passes nix-store paths; the runners gain a default so `roc check/<name>/main.roc` with no arguments does the right thing, while explicit paths keep working (useful for one-off ROMs). The shared `Harness` suite runner keeps reading the passlist relative to the check directory.
4. **Delete rather than deprecate the nix pieces.** `package.nix` × 4, `gb-test-roms.nix`, and the flake `check-*` packages go in the same commit that lands the fetch apps, so there is never a window with two harnesses to keep in sync. Rollback is `git revert` of one commit.

## Risks / Trade-offs

- [GitHub raw-content availability at fetch time] → Same exposure the sibling accepted; hash verification catches corruption/upstream rewrites. The machine's known DNS flakiness for github.com has a documented `--resolve`/8.8.8.8 workaround.
- [`roc-lang/http` package capability (redirects, binary bodies)] → The sibling's fetch.roc already downloads NES ROM binaries with it; GB ROMs are the same shape. Verify one download end-to-end before porting all lists.
- [Losing nix's store cache (ROMs re-downloaded per checkout)] → data/ persists per working copy and fetch is idempotent; acceptable for < 5 MB of ROMs.

## Migration Plan

Single commit: add fetch apps + runner defaults, delete nix pieces, update README. Verify by running all four checks from a clean `data/`-less state. Revert restores the nix harness wholesale.

## Open Questions

None.
