# CGB arc, increment 2: the color PPU

## Why

Increment 1 gave CGB games working memory and detection; they now run their color code paths into a renderer that ignores everything colorful. This increment makes the picture real: the framebuffer speaks RGB555 (decision B from the exploration — the core speaks hardware, edges colorize), the CGB render path honors tile attributes, palettes, and the changed priority model, and cgb-acid2 becomes the golden oracle exactly as dmg-acid2 is for DMG.

## What Changes

- **Framebuffer becomes RGB555 `List(U16)` for both models.** DMG shades map through a fixed grayscale at render time (31/21/10/0 per channel). Consumers convert 555→RGBA/PPM: both apps, `example/debug.roc`, and the acid2 check. **The dmg-acid2 golden digest changes** (PPM bytes derive from 555 now) — one deliberate re-bless, visually verified against the published reference like the original.
- **CGB render path** as a separate `render_line` variant sharing fetch helpers (the sanctioned fork from increment 1's design): BG/window tile attributes from VRAM bank 1 (palette 0–7, tile bank, flips, per-tile priority), color lookup through the palette RAM landed in increment 1, sprite palettes/banks, and CGB priority resolution — LCDC bit 0 reroles to master BG priority, OAM-index order instead of X order (OPRI honored).
- **cgb-acid2 as a second golden in the acid2 slice**: the check program's `--check` generalizes to `<rom> <golden-prefix>`; the slice gains the pinned `.gbc` ROM and a `cgb-golden.sha256`, blessed once with visual verification against the published reference.
- Debug renders follow the framebuffer (background/OAM through the active model's palettes; tile sheet stays palette-agnostic, shown as grays).

## Capabilities

### Modified Capabilities

- `cgb-core`: ADDED — color rendering requirement (attributes, palettes, priority; cgb-acid2 oracle).
- `ppu-verification`: MODIFIED — the acid2 oracle covers both models with two goldens; digests derive from the RGB555 framebuffer.

## Impact

- `package/Ppu.roc` (framebuffer type, DMG gray mapping, CGB variant, debug renders), `package/GameBoy.roc` (signature), `check/acid2/` (generalized check, second ROM+golden), `example/debug.roc`, both apps' color conversion, README.
- Verification: cgb-acid2 golden (new), dmg-acid2 re-bless (visually verified), all non-image checks byte-identical (blargg/mooneye/sound don't read the framebuffer), full test suite.
