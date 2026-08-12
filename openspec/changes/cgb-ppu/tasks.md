# Tasks — cgb-ppu

## 1. Framebuffer representation

- [x] 1.1 `Ppu`: RGB555 `List(U16)` framebuffer; DMG path writes gray-555; `gray` test helper; all Ppu/GameBoy expects updated; `roc test` green
- [x] 1.2 Consumers converted: `example/debug.roc`, `check/acid2/main.roc` PPM writers (555→888); web app `rgba()`; ray app `shade_color`

## 2. CGB render path

- [x] 2.1 `render_line_cgb`: BG/window attributes (palette, bank, flips, priority) via `read_vram`, palette-RAM color lookup; per-pixel (index, priority) recorded
- [x] 2.2 CGB sprite pass: palettes/banks from OAM attrs, CGB priority resolution incl. LCDC.0 master bit, OAM-index order (OPRI-aware); expects for palette lookup and priority cases

## 3. Oracles

- [x] 3.1 acid2 check generalized to `--check <rom> <golden-prefix>`; package.nix adds the pinned cgb-acid2 ROM and runs both models
- [x] 3.2 Re-bless dmg golden and bless cgb golden, both verified visually against published references; commit digests

## 4. Regression

- [x] 4.1 blargg/mooneye/sound byte-identical; full test suite; both apps build; web app shows color for a CGB cart (browser check)
