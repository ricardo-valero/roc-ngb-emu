## 1. PPU timing skeleton

- [ ] 1.1 Create `package/Ppu.roc`: nominal `Ppu` record (dot counter, framebuffer, window line counter) with `init` and `tick : Ppu, Mmu, U64 -> { ppu, mmu }` walking the mode state machine (456 dots/line, modes 2→3→0, VBlank lines 144–153), writing LY and STAT mode/coincidence bits via poke (D2, D3)
- [ ] 1.2 Raise interrupts from tick: VBlank (IF bit 0) at line-144 entry; STAT (IF bit 1) for enabled mode-entry and LY=LYC sources (D6); inline expects for LY progression, mode sequence in STAT, VBlank-once-per-frame, LYC coincidence
- [ ] 1.3 Wire `Ppu` into `GameBoy`: add ppu field, tick it in `finish` alongside the timer; remove the `LY = 0x90` stub from `Mmu.init`
- [ ] 1.4 Regression tripwire (D9): full Blargg suite still 11/11 with real LY progression and no rendering yet

## 2. Background rendering

- [ ] 2.1 Tile decode helpers: 2bpp row extraction, both tile-data addressing modes (0x8000 unsigned / 0x8800 signed), BGP palette application, with inline expects
- [ ] 2.2 Scanline background render at mode-3 entry honoring LCDC bits 0/3/4 and SCX/SCY wraparound into the 160×144 framebuffer (D1, D4)
- [ ] 2.3 Inline expects: known tile at scroll origin lands in framebuffer; scroll wrap across the 256-pixel map edge

## 3. Window rendering

- [ ] 3.1 Window layer per scanline: LCDC bits 5/6, WX-7/WY placement, internal line counter incrementing only when the window rendered (D7)
- [ ] 3.2 Inline expects: window overlays background from WY down, window starts at its own line 0

## 4. Sprite rendering

- [ ] 4.1 Mode-2 OAM scan: collect up to 10 sprites for the line in OAM order, 8×8/8×16 per LCDC bit 2 with tile-index bit-0 masking (D7)
- [ ] 4.2 Sprite pixel compositing: X/Y flips, OBP0/OBP1 with color-0 transparency, DMG X-priority with OAM-order tiebreak, BG-over-OBJ attribute
- [ ] 4.3 Inline expects: basic sprite placement, 10-per-line limit, BG-over-OBJ behavior, flips

## 5. OAM DMA

- [ ] 5.1 `Mmu.write` on `0xFF46`: instant 160-byte copy from `value << 8` into OAM (D5), with the sprite-table round-trip expect

## 6. Frame capture and verification

- [ ] 6.1 `GameBoy.run_frame : GameBoy -> GameBoy` (step until VBlank entry, step-budget bounded) and `framebuffer` accessor (D4)
- [ ] 6.2 `example/frame.roc`: load ROM, run N frames, write P6 PPM with 4-gray tone map (D8)
- [ ] 6.3 Extend `nix/fetch-roms.nix` to also fetch dmg-acid2.gb (mattcurrie/dmg-acid2 release, untracked in `rom/`)
- [ ] 6.4 Render dmg-acid2, compare visually against the published reference image, fix rendering until it matches (the debugging loop lives here)
- [ ] 6.5 Freeze the verified dmg-acid2 framebuffer digest into a `check-acid2` nix app (or extend run-blargg) as the regression oracle; suite exits nonzero on mismatch

## 7. Wrap-up

- [ ] 7.1 `roc check`/`roc test` green across package and examples; Blargg 11/11; README status updated (PPU + how to dump frames)
- [ ] 7.2 Verify all delta-spec scenarios map to passing expects or runner behavior; ready for archive
