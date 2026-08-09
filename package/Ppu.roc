# DMG picture processing unit: scanline renderer driven by the cycles each
# CPU step reports, same contract as the timer. Registers (LCDC, STAT, LY,
# palettes, scroll) live in the bus; this record holds what the hardware
# hides: the dot counter, the framebuffer, and the window line counter.
# https://gbdev.io/pandocs/Rendering.html

import /Mmu

Ppu := {
    dots : U64, # position within the current scanline (0..455)
    framebuffer : List(U8), # 160x144 post-palette shades (0..3), row-major
    window_line : U8, # internal window line counter (increments only when the window rendered)
}.{
    init : {} -> Ppu
    init = |_| {
        ppu : Ppu
        ppu = { dots: 0, framebuffer: List.repeat(0, 23040), window_line: 0 }
        ppu
    }

    frame : Ppu -> List(U8)
    frame = |ppu| ppu.framebuffer

    # Advance by elapsed T-cycles, consuming dots up to each mode boundary so
    # chunked deltas never skip a transition.
    tick : Ppu, Mmu, U64 -> { ppu : Ppu, mmu : Mmu }
    tick = |ppu0, mmu0, cycles| {
        if mmu0.read(0xFF40).bitwise_and(0x80) == 0x00 {
            # LCD off: LY pinned to 0, mode 0, nothing advances
            mmu1 = mmu0.poke(0xFF44, 0x00).poke(0xFF41, mmu0.read(0xFF41).bitwise_and(0xFC))
            { ppu: { ..ppu0, dots: 0, window_line: 0 }, mmu: mmu1 }
        } else {
            var ppu = ppu0
            var mmu = mmu0
            var remaining = cycles
            while remaining > 0 {
                ly = mmu.read(0xFF44)
                boundary =
                    if ly >= 144 {
                        456
                    } else if ppu.dots < 80 {
                        80
                    } else if ppu.dots < 252 {
                        252
                    } else {
                        456
                    }
                gap = boundary.minus(ppu.dots)
                delta = if remaining < gap { remaining } else { gap }
                ppu = { ..ppu, dots: ppu.dots.plus(delta) }
                remaining = remaining.minus(delta)
                if ppu.dots == boundary {
                    if boundary == 80 {
                        # enter drawing (mode 3): render this scanline now
                        mmu = set_mode(mmu, 3)
                        ppu = render_line(ppu, mmu, ly)
                        {}
                    } else if boundary == 252 {
                        # enter HBlank (mode 0)
                        mmu = stat_interrupt(set_mode(mmu, 0), 0x08)
                        {}
                    } else {
                        # line complete
                        ppu = { ..ppu, dots: 0 }
                        next_ly = if ly >= 153 { 0 } else { ly.plus(1) }
                        mmu = check_lyc(mmu.poke(0xFF44, next_ly), next_ly)
                        if next_ly == 144 {
                            # enter VBlank (mode 1)
                            mmu = stat_interrupt(set_mode(mmu, 1), 0x10).request_interrupt(0)
                            {}
                        } else if next_ly == 0 {
                            # frame start
                            ppu = { ..ppu, window_line: 0 }
                            mmu = stat_interrupt(set_mode(mmu, 2), 0x20)
                            {}
                        } else if next_ly < 144 {
                            # enter OAM scan (mode 2)
                            mmu = stat_interrupt(set_mode(mmu, 2), 0x20)
                            {}
                        } else {
                            {} # still VBlank
                        }
                    }
                } else {
                    {}
                }
            }
            { ppu: ppu, mmu: mmu }
        }
    }

    set_mode : Mmu, U8 -> Mmu
    set_mode = |mmu, mode|
        mmu.poke(0xFF41, mmu.read(0xFF41).bitwise_and(0xFC).bitwise_or(mode))

    # Raise IF bit 1 when the given STAT enable bit is set
    stat_interrupt : Mmu, U8 -> Mmu
    stat_interrupt = |mmu, enable_mask|
        if mmu.read(0xFF41).bitwise_and(enable_mask) != 0x00 {
            mmu.request_interrupt(1)
        } else {
            mmu
        }

    check_lyc : Mmu, U8 -> Mmu
    check_lyc = |mmu, ly| {
        stat = mmu.read(0xFF41)
        if ly == mmu.read(0xFF45) {
            with_flag = mmu.poke(0xFF41, stat.bitwise_or(0x04))
            stat_interrupt(with_flag, 0x40)
        } else {
            mmu.poke(0xFF41, stat.bitwise_and(0xFB))
        }
    }

    # Compose one scanline: background, window, then sprites
    render_line : Ppu, Mmu, U8 -> Ppu
    render_line = |ppu, mmu, ly| {
        lcdc = mmu.read(0xFF40)
        bgp = mmu.read(0xFF47)
        scx = mmu.read(0xFF43).to_u16()
        scy = mmu.read(0xFF42).to_u16()
        wx = mmu.read(0xFF4B).to_u16()
        wy = mmu.read(0xFF4A)
        bg_enabled = lcdc.bitwise_and(0x01) != 0x00
        # DMG: LCDC bit 0 off blanks the window too
        win_enabled = bg_enabled and lcdc.bitwise_and(0x20) != 0x00 and ly >= wy
        ly16 = ly.to_u16()

        # background + window pass: raw color indices, then palette shades
        var raw = List.repeat(0.U8, 160)
        var fb = ppu.framebuffer
        var window_rendered = Bool.False
        var x = 0.U16
        while x < 160 {
            in_window = win_enabled and x.plus(7) >= wx
            color =
                if in_window {
                    tile_color(mmu, lcdc, 0x40, x.plus(7).minus(wx), ppu.window_line.to_u16())
                } else if bg_enabled {
                    tile_color(mmu, lcdc, 0x08, x.plus(scx).bitwise_and(0xFF), ly16.plus(scy).bitwise_and(0xFF))
                } else {
                    0
                }
            if in_window {
                window_rendered = Bool.True
            } else {
                {}
            }
            raw = raw.set(x.to_u64(), color) ?? raw
            fb = fb.set(ly16.to_u64().shl_wrap(7).plus(ly16.to_u64().shl_wrap(5)).plus(x.to_u64()), palette_shade(bgp, color)) ?? fb
            x = x.plus(1)
        }

        # sprite pass
        fb2 =
            if lcdc.bitwise_and(0x02) != 0x00 {
                render_sprites(mmu, lcdc, ly16, raw, fb)
            } else {
                fb
            }

        next_window_line = if window_rendered { ppu.window_line.plus_wrap(1) } else { ppu.window_line }
        { ..ppu, framebuffer: fb2, window_line: next_window_line }
    }

    # Fetch the 2-bit color of a background/window pixel. map_mask selects the
    # LCDC tile-map bit (0x08 bg, 0x40 window); px/py are map-space coordinates.
    tile_color : Mmu, U8, U8, U16, U16 -> U8
    tile_color = |mmu, lcdc, map_mask, px, py| {
        map_base = if lcdc.bitwise_and(map_mask) != 0x00 { 0x9C00 } else { 0x9800 }
        tile_index = mmu.read(map_base.plus(py.shr_zf_wrap(3).shl_wrap(5)).plus(px.shr_zf_wrap(3)))
        row = py.bitwise_and(0x07)
        tile_addr =
            if lcdc.bitwise_and(0x10) != 0x00 {
                U16.plus(0x8000, tile_index.to_u16().shl_wrap(4)) # unsigned 0x8000 mode
            } else if tile_index >= 0x80 {
                U16.plus(0x8800, tile_index.to_u16().minus(0x80).shl_wrap(4)) # signed 0x8800 mode
            } else {
                U16.plus(0x9000, tile_index.to_u16().shl_wrap(4))
            }
        pixel_from_tile_row(mmu, tile_addr.plus(row.shl_wrap(1)), px.bitwise_and(0x07))
    }

    # Decode one pixel from a 2bpp tile row at addr; col is 0 (leftmost) to 7
    pixel_from_tile_row : Mmu, U16, U16 -> U8
    pixel_from_tile_row = |mmu, addr, col| {
        bit = 7.U8.minus(col.to_u8_wrap())
        lo = mmu.read(addr).shr_zf_wrap(bit).bitwise_and(0x01)
        hi = mmu.read(addr.plus(1)).shr_zf_wrap(bit).bitwise_and(0x01)
        hi.shl_wrap(1).bitwise_or(lo)
    }

    palette_shade : U8, U8 -> U8
    palette_shade = |pal, color| pal.shr_zf_wrap(color.shl_wrap(1)).bitwise_and(0x03)

    render_sprites : Mmu, U8, U16, List(U8), List(U8) -> List(U8)
    render_sprites = |mmu, lcdc, ly16, raw, fb0| {
        height = if lcdc.bitwise_and(0x04) != 0x00 { 16.U16 } else { 8.U16 }
        # OAM scan: first 10 sprites covering this line, in OAM order
        var selected = List.repeat(0xFE00.U16, 0)
        var i = 0.U16
        while i < 40 {
            base = U16.plus(0xFE00, i.shl_wrap(2))
            sy = mmu.read(base).to_u16()
            if selected.len() < 10 and ly16.plus(16) >= sy and ly16.plus(16) < sy.plus(height) {
                selected = selected.append(base)
            } else {
                {}
            }
            i = i.plus(1)
        }
        var fb = fb0
        var x = 0.U16
        while x < 160 {
            # DMG priority: smallest X wins, earlier OAM entry breaks ties —
            # iterate in OAM order and replace only on strictly smaller X
            var best_x = 0xFFFF.U16
            var best_color = 0.U8
            var best_attrs = 0.U8
            var j = 0
            while j < selected.len() {
                base = selected.get(j) ?? 0xFE00
                sx = mmu.read(base.plus(1)).to_u16()
                if x.plus(8) >= sx and x.plus(8) < sx.plus(8) and sx < best_x {
                    color = sprite_color(mmu, base, height, x, ly16)
                    if color != 0x00 {
                        best_x = sx
                        best_color = color
                        best_attrs = mmu.read(base.plus(3))
                    } else {
                        {}
                    }
                } else {
                    {}
                }
                j = j.plus(1)
            }
            if best_color != 0x00 {
                behind_bg = best_attrs.bitwise_and(0x80) != 0x00
                bg_color = raw.get(x.to_u64()) ?? 0
                if behind_bg and bg_color != 0x00 {
                    {} # BG-over-OBJ: nonzero background covers the sprite
                } else {
                    obp = if best_attrs.bitwise_and(0x10) != 0x00 { mmu.read(0xFF49) } else { mmu.read(0xFF48) }
                    idx = ly16.to_u64().shl_wrap(7).plus(ly16.to_u64().shl_wrap(5)).plus(x.to_u64())
                    fb = fb.set(idx, palette_shade(obp, best_color)) ?? fb
                }
            } else {
                {}
            }
            x = x.plus(1)
        }
        fb
    }

    # 2-bit color of a sprite pixel (0 = transparent); x/ly in screen space
    sprite_color : Mmu, U16, U16, U16, U16 -> U8
    sprite_color = |mmu, base, height, x, ly16| {
        sy = mmu.read(base).to_u16()
        sx = mmu.read(base.plus(1)).to_u16()
        tile = mmu.read(base.plus(2))
        attrs = mmu.read(base.plus(3))
        row0 = ly16.plus(16).minus(sy)
        row = if attrs.bitwise_and(0x40) != 0x00 { height.minus(1).minus(row0) } else { row0 }
        col0 = x.plus(8).minus(sx)
        col = if attrs.bitwise_and(0x20) != 0x00 { U16.minus(7, col0) } else { col0 }
        # 16 rows x 2 bytes spans two tiles, so masking bit 0 covers 8x16 mode
        tile_index = if height == 16 { tile.bitwise_and(0xFE) } else { tile }
        addr = U16.plus(0x8000, tile_index.to_u16().shl_wrap(4)).plus(row.shl_wrap(1))
        pixel_from_tile_row(mmu, addr, col)
    }
}

test_rom : List(U8)
test_rom = List.repeat(0x00, 0x8000)

fresh : {} -> { ppu : Ppu, mmu : Mmu }
fresh = |_| { ppu: Ppu.init({}), mmu: Mmu.init(test_rom).poke(0xFF0F, 0x00) }

# Mode transitions within a visible line
expect {
    f = fresh({})
    r = f.ppu.tick(f.mmu, 81)
    r.mmu.read(0xFF41).bitwise_and(0x03) == 3
}
expect {
    f = fresh({})
    r = f.ppu.tick(f.mmu, 253)
    r.mmu.read(0xFF41).bitwise_and(0x03) == 0
}

# Line advance and VBlank entry
expect {
    f = fresh({})
    r = f.ppu.tick(f.mmu, 456)
    r.mmu.read(0xFF44) == 1 and r.mmu.read(0xFF41).bitwise_and(0x03) == 2
}
expect {
    f = fresh({})
    r = f.ppu.tick(f.mmu, 456 * 144)
    r.mmu.read(0xFF44) == 144
    and r.mmu.read(0xFF41).bitwise_and(0x03) == 1
    and r.mmu.read(0xFF0F).bitwise_and(0x01) == 0x01
}

# Full frame wraps LY back to the starting line
expect {
    f = fresh({})
    r = f.ppu.tick(f.mmu, 70224)
    r.mmu.read(0xFF44) == 0
}

# VBlank fires once per frame: clear IF after entry, finish the frame, no re-fire
expect {
    f = fresh({})
    a = f.ppu.tick(f.mmu, 456 * 144)
    b = a.ppu.tick(a.mmu.poke(0xFF0F, 0x00), 456 * 10)
    b.mmu.read(0xFF0F).bitwise_and(0x01) == 0x00
}

# LYC coincidence sets STAT bit 2 and raises STAT interrupt when enabled
expect {
    f = fresh({})
    armed = f.mmu.poke(0xFF45, 40).poke(0xFF41, f.mmu.read(0xFF41).bitwise_or(0x40))
    r = f.ppu.tick(armed, 456 * 40)
    r.mmu.read(0xFF44) == 40
    and r.mmu.read(0xFF41).bitwise_and(0x04) == 0x04
    and r.mmu.read(0xFF0F).bitwise_and(0x02) == 0x02
}

# LCD off pins LY to 0
expect {
    f = fresh({})
    off = f.mmu.poke(0xFF40, 0x00)
    r = f.ppu.tick(off, 10000)
    r.mmu.read(0xFF44) == 0
}

# --- rendering ---

# Fill one 8x8 tile with solid color 3 (every row 0xFF 0xFF)
paint_tile : Mmu, U16 -> Mmu
paint_tile = |mmu0, addr| {
    var mmu = mmu0
    var i = 0.U16
    while i < 16 {
        mmu = mmu.poke(addr.plus(i), 0xFF)
        i = i.plus(1)
    }
    mmu
}

pixel_at : { ppu : Ppu, mmu : Mmu }, U64, U64 -> U8
pixel_at = |r, px, py| r.ppu.framebuffer.get(py * 160 + px) ?? 0xFF

# Tile decode: unsigned 0x8000 mode and signed 0x8800 mode
expect {
    m = paint_tile(fresh({}).mmu, 0x8010).poke(0x9800, 0x01)
    Ppu.tile_color(m, 0x91, 0x08, 0, 0) == 3 and Ppu.tile_color(m, 0x91, 0x08, 8, 0) == 0
}
expect {
    m = paint_tile(fresh({}).mmu, 0x9000) # tile 0 in signed mode lives at 0x9000
    Ppu.tile_color(m, 0x81, 0x08, 0, 0) == 3
}

# Palette application
expect Ppu.palette_shade(0xE4, 3) == 3 and Ppu.palette_shade(0xE4, 0) == 0
expect Ppu.palette_shade(0x1B, 0) == 3 and Ppu.palette_shade(0x1B, 3) == 0

# Background: known tile at the scroll origin lands in the framebuffer
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010).poke(0x9800, 0x01).write(0xFF47, 0xE4)
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 3 and pixel_at(r, 7, 0) == 3 and pixel_at(r, 8, 0) == 0
}

# Background: SCX wraps across the 256-pixel map edge
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010).poke(0x981F, 0x01).write(0xFF47, 0xE4).write(0xFF43, 252)
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 3 and pixel_at(r, 3, 0) == 3 and pixel_at(r, 4, 0) == 0
}

# LCDC bit 0 off blanks the background
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010).poke(0x9800, 0x01).write(0xFF47, 0xE4).write(0xFF40, 0x90)
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 0
}

# Window: overlays background from WY down, drawing the window's own line 0
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010)
        .poke(0x9C00, 0x01) # window map (LCDC bit 6)
        .write(0xFF47, 0xE4)
        .write(0xFF4A, 64) # WY
        .write(0xFF4B, 7) # WX
        .write(0xFF40, 0xF1) # LCD on, win on, win map 0x9C00, bg on
    r = f.ppu.tick(m, 456 * 64 + 81)
    # only window-map column 0 holds the painted tile
    pixel_at(r, 0, 63) == 0 and pixel_at(r, 0, 64) == 3 and pixel_at(r, 7, 64) == 3 and pixel_at(r, 8, 64) == 0
}

# Sprite: basic placement with OBP0, over a blank background
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010)
        .poke(0xFE00, 16).poke(0xFE01, 8).poke(0xFE02, 0x01).poke(0xFE03, 0x00)
        .write(0xFF47, 0x00) # bg all white
        .write(0xFF48, 0xE4) # OBP0
        .write(0xFF40, 0x93) # bg + obj enabled
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 3 and pixel_at(r, 7, 0) == 3 and pixel_at(r, 8, 0) == 0
}

# Sprite scanline limit: the 11th sprite in OAM order does not render
expect {
    f = fresh({})
    var m = paint_tile(f.mmu, 0x8010).write(0xFF47, 0x00).write(0xFF48, 0xE4).write(0xFF40, 0x93)
    var i = 0.U16
    while i < 11 {
        base = U16.plus(0xFE00, i.shl_wrap(2))
        m = m.poke(base, 16).poke(base.plus(1), U16.plus(8, i.shl_wrap(3)).to_u8_wrap()).poke(base.plus(2), 0x01).poke(base.plus(3), 0x00)
        i = i.plus(1)
    }
    r = f.ppu.tick(m, 81)
    pixel_at(r, 72, 0) == 3 and pixel_at(r, 80, 0) == 0
}

# BG-over-OBJ: sprite hides behind nonzero background, shows over color 0
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8010)
        .poke(0x9800, 0x01) # bg color 3 in the first tile column only
        .poke(0xFE00, 16).poke(0xFE01, 8).poke(0xFE02, 0x01).poke(0xFE03, 0x80)
        .poke(0xFE04, 16).poke(0xFE05, 16).poke(0xFE06, 0x01).poke(0xFE07, 0x80)
        .write(0xFF47, 0xE4)
        .write(0xFF48, 0x40) # OBP0: color 3 -> shade 1, distinguishable from bg
        .write(0xFF40, 0x93)
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 3 and pixel_at(r, 8, 0) == 1
}

# X flip: tile row 0xF0 renders reversed
expect {
    f = fresh({})
    m = f.mmu
        .poke(0x8020, 0xF0).poke(0x8021, 0x00) # tile 2, row 0: left half color 1
        .poke(0xFE00, 16).poke(0xFE01, 8).poke(0xFE02, 0x02).poke(0xFE03, 0x20)
        .write(0xFF47, 0x00)
        .write(0xFF48, 0xE4)
        .write(0xFF40, 0x93)
    r = f.ppu.tick(m, 81)
    pixel_at(r, 0, 0) == 0 and pixel_at(r, 7, 0) == 1
}

# 8x16 sprites: lower half comes from the next tile with index bit 0 masked
expect {
    f = fresh({})
    m = paint_tile(f.mmu, 0x8030) # tile 3: lower half of the 8x16 pair 2/3
        .poke(0xFE00, 16).poke(0xFE01, 8).poke(0xFE02, 0x03).poke(0xFE03, 0x00) # index 3 masks to 2
        .write(0xFF47, 0x00)
        .write(0xFF48, 0xE4)
        .write(0xFF40, 0x97) # 8x16 mode
    r = f.ppu.tick(m, 456 * 8 + 81)
    pixel_at(r, 0, 0) == 0 and pixel_at(r, 0, 8) == 3
}
