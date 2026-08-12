# Design — cgb-ppu

## Context

Increment 2 of the CGB arc; increment 1 landed banked VRAM (with the bank-explicit `read_vram`), palette RAM, and the model enum. The PPU today: `render_line` composes BG/window raw color indices then a sprite pass, writing `palette_shade` results (0–3) into a `List(U8)` framebuffer; `pixel_from_tile_row`/`tile_color` do tile fetches through `mmu.read`.

## Goals / Non-Goals

**Goals:** cgb-acid2 pixel-perfect (golden); dmg-acid2 the same picture in gray RGB555; non-image checks byte-identical.

**Non-Goals:** double speed/HDMA (increment 3); DMG-game colorization; mid-scanline palette/attribute latching subtleties beyond what cgb-acid2 demands.

## Decisions

- **RGB555 `List(U16)` framebuffer, both models** (standing decision B). DMG gray levels 255/170/85/0 become 5-bit 31/21/10/0 replicated per channel. Consumers convert with the standard `(c << 3) | (c >> 2)` channel expansion — apps to RGBA, checks/debug to PPM. The old 8-bit grays (170, 85) become 173 and 82 after round-tripping: visually identical, digest-different — hence the one re-bless.
- **Two `render_line` variants** (the sanctioned fork): `render_line_dmg` is today's code with the gray mapping at the write; `render_line_cgb` fetches the attribute byte via `read_vram(1, map_addr)`, resolves tile data from the attribute's bank with flips applied in the fetch helper, and looks pixels up in palette RAM (`bg_pal[pal*8 + color*2]` little-endian → RGB555). Shared: `pixel_from_tile_row`, map addressing, the scanline driver.
- **CGB priority resolution** in the sprite pass: a sprite pixel lands unless `LCDC.0 && bg_color != 0 && (bg_tile_priority || sprite_priority_bit)`; with LCDC.0 clear, sprites always land on nonzero-or-not BG. Sprite-vs-sprite: OAM index order when OPRI=0 (CGB default — first opaque OAM entry wins), X order when OPRI=1. The DMG variant keeps today's X-order logic untouched.
- **The BG pass records per-pixel `(color_index, tile_priority)`** for the sprite pass (today it records raw index only); DMG variant records priority as always-false.
- **acid2 check generalizes**: `--check <rom> <golden-prefix>` — golden at `<prefix>.sha256`, bless artifact `<prefix>.ppm`, mismatch artifact `<prefix>-actual.ppm`. The slice's package.nix runs DMG then CGB against `check/acid2/golden` and `check/acid2/cgb-golden`. Both blesses verified visually (PPM converted and eyeballed against published references) before committing digests.
- **Debug renders**: background map and OAM render through the active model's palettes into RGB555; the tile sheet (palette-agnostic by design) shows raw indices as the gray ramp. `debug.roc`'s PPM writer takes RGB555.

## Risks / Trade-offs

- [Priority rules are the classically-wrong part] → cgb-acid2 is purpose-built to catch exactly these; bless only on visual match.
- [Framebuffer type change ripples widely] → mechanical and compiler-led (type errors enumerate the sites); Ppu expects updated via a `gray` helper so intent stays readable.
- [Re-blessing dmg-acid2 could hide a real regression] → bless procedure requires the visual check against the published reference, not just accepting the new digest.
