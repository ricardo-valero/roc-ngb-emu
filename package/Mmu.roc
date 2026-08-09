# Flat 64 KiB memory bus. Only `read`/`write` touch `mem` so the list stays
# uniquely owned and Roc can mutate it in place (never alias it elsewhere).
# https://gbdev.io/pandocs/Memory_Map.html

Mmu := {
    mem : List(U8),
    rom : List(U8), # full cartridge image, bank-mapped on read
    cart_ram : List(U8), # 32 KiB external RAM, session lifetime
    serial_out : List(U8),
    div_counter : U64,
    tima_counter : U64,
    buttons : { up : Bool, down : Bool, left : Bool, right : Bool, a : Bool, b : Bool, start : Bool, select : Bool },
    mbc : [None, Mbc1, Mbc3],
    rom_bank : U8, # raw register value; 0->1 translation happens at read
    bank2 : U8, # MBC1 secondary register / MBC3 RAM bank (or RTC select)
    mode : Bool, # MBC1 banking mode
    ram_enable : Bool,
}.{
    no_buttons : {} -> { up : Bool, down : Bool, left : Bool, right : Bool, a : Bool, b : Bool, start : Bool, select : Bool }
    no_buttons = |_| {
        up: Bool.False,
        down: Bool.False,
        left: Bool.False,
        right: Bool.False,
        a: Bool.False,
        b: Bool.False,
        start: Bool.False,
        select: Bool.False,
    }

    init : List(U8) -> Mmu
    init = |rom| {
        type_byte = rom.get(0x0147) ?? 0x00
        mbc =
            if type_byte >= 0x01 and type_byte <= 0x03 {
                Mbc1
            } else if type_byte >= 0x0F and type_byte <= 0x13 {
                Mbc3
            } else {
                None
            }
        base : Mmu
        base = {
            mem: List.repeat(0, 0x10000),
            rom: rom,
            cart_ram: List.repeat(0, 0x8000),
            serial_out: [],
            div_counter: 0,
            tima_counter: 0,
            buttons: no_buttons({}),
            mbc: mbc,
            rom_bank: 1,
            bank2: 0,
            mode: Bool.False,
            ram_enable: Bool.False,
        }
        # DMG post-boot IO state. LY starts at 0; the PPU advances it for real.
        [
            (0xFF00, 0xCF), # P1/JOYP: no buttons pressed
            (0xFF02, 0x7E), # SC
            (0xFF04, 0xAB), # DIV
            (0xFF07, 0xF8), # TAC
            (0xFF0F, 0xE1), # IF
            (0xFF40, 0x91), # LCDC
            (0xFF41, 0x86), # STAT: line 0, mode 2, LY=LYC
            (0xFF47, 0xFC), # BGP
        ]
            .fold(base, |mmu, (addr, value)| mmu.poke(addr, value))
    }

    read : Mmu, U16 -> U8
    read = |mmu, addr|
        if addr < 0x4000 {
            mmu.rom.get(zero_region_base(mmu).shl_wrap(14).plus(addr.to_u64())) ?? 0xFF
        } else if addr < 0x8000 {
            mmu.rom.get(switch_region_bank(mmu).shl_wrap(14).plus(addr.to_u64().minus(0x4000))) ?? 0xFF
        } else if addr >= 0xA000 and addr < 0xC000 {
            read_cart_ram(mmu, addr)
        } else if addr == 0xFF00 {
            read_p1(mmu)
        } else {
            mmu.mem.get(addr.to_u64()) ?? 0xFF
        }

    bank_count : Mmu -> U64
    bank_count = |mmu| {
        n = mmu.rom.len().shr_zf_wrap(14)
        if n == 0 { 1 } else { n }
    }

    # Effective bank for 0x4000-0x7FFF; the raw register's zero maps to 1
    # before combining, and the result wraps to the ROM's actual bank count
    # the way real cartridge address wiring does.
    switch_region_bank : Mmu -> U64
    switch_region_bank = |mmu|
        match mmu.mbc {
            None => 1
            Mbc1 => {
                low5 = mmu.rom_bank.bitwise_and(0x1F)
                low = if low5 == 0x00 { 1 } else { low5.to_u64() }
                mmu.bank2.bitwise_and(0x03).to_u64().shl_wrap(5).plus(low) % bank_count(mmu)
            }

            Mbc3 => {
                b = mmu.rom_bank.bitwise_and(0x7F)
                (if b == 0x00 { 1 } else { b.to_u64() }) % bank_count(mmu)
            }
        }

    # 0x0000-0x3FFF is bank 0 except MBC1 mode 1, where bank2 maps it
    zero_region_base : Mmu -> U64
    zero_region_base = |mmu|
        match mmu.mbc {
            Mbc1 =>
                if mmu.mode {
                    mmu.bank2.bitwise_and(0x03).to_u64().shl_wrap(5) % bank_count(mmu)
                } else {
                    0
                }

            _ => 0
        }

    # MBC1 banks RAM by bank2 in mode 1 only; MBC3 by bank2 values 0-3
    # (RTC selects 0x08+ read as 0 and drop writes); None is ungated bank 0.
    cart_ram_slot : Mmu -> [Bank(U64), Invalid]
    cart_ram_slot = |mmu|
        match mmu.mbc {
            None => Bank(0)
            Mbc1 =>
                if mmu.ram_enable {
                    if mmu.mode { Bank(mmu.bank2.bitwise_and(0x03).to_u64()) } else { Bank(0) }
                } else {
                    Invalid
                }

            Mbc3 =>
                if mmu.ram_enable and mmu.bank2 <= 0x03 {
                    Bank(mmu.bank2.to_u64())
                } else {
                    Invalid
                }
        }

    read_cart_ram : Mmu, U16 -> U8
    read_cart_ram = |mmu, addr|
        match cart_ram_slot(mmu) {
            Bank(b) => mmu.cart_ram.get(b.shl_wrap(13).plus(addr.to_u64().minus(0xA000))) ?? 0xFF
            Invalid => 0xFF
        }

    set_buttons = |mmu, buttons| { ..mmu, buttons: buttons }

    # P1/JOYP: stored select bits plus the selected group's buttons,
    # active-low (0 = pressed). Both groups selected AND together.
    read_p1 : Mmu -> U8
    read_p1 = |mmu| {
        sel = (mmu.mem.get(0xFF00) ?? 0xFF).bitwise_and(0x30)
        b = mmu.buttons
        dpad = if sel.bitwise_and(0x10) == 0x00 { button_nibble(b.right, b.left, b.up, b.down) } else { 0x0F }
        actions = if sel.bitwise_and(0x20) == 0x00 { button_nibble(b.a, b.b, b.select, b.start) } else { 0x0F }
        U8.bitwise_or(0xC0, sel).bitwise_or(dpad.bitwise_and(actions))
    }

    # bits 0-3, low when pressed
    button_nibble : Bool, Bool, Bool, Bool -> U8
    button_nibble = |b0, b1, b2, b3|
        button_bit(b0, 0x01)
            .bitwise_or(button_bit(b1, 0x02))
            .bitwise_or(button_bit(b2, 0x04))
            .bitwise_or(button_bit(b3, 0x08))

    button_bit : Bool, U8 -> U8
    button_bit = |pressed, mask| if pressed { 0x00 } else { mask }

    serial : Mmu -> List(U8)
    serial = |mmu| mmu.serial_out

    # Raw store bypassing region semantics (init defaults, timer internals)
    poke : Mmu, U16, U8 -> Mmu
    poke = |mmu, addr, value|
        { ..mmu, mem: mmu.mem.set(addr.to_u64(), value) ?? mmu.mem }

    write : Mmu, U16, U8 -> Mmu
    write = |mmu, addr, value|
        if addr < 0x8000 {
            write_mbc(mmu, addr, value)
        } else if addr >= 0xA000 and addr < 0xC000 {
            match cart_ram_slot(mmu) {
                Bank(b) => { ..mmu, cart_ram: mmu.cart_ram.set(b.shl_wrap(13).plus(addr.to_u64().minus(0xA000)), value) ?? mmu.cart_ram }
                Invalid => mmu
            }
        } else if addr == 0xFF02 {
            # Serial control: bit 7 starts a transfer; capture SB as the
            # Blargg reporting channel and mark the transfer complete
            if value.bitwise_and(0x80) != 0x00 {
                sb = mmu.read(0xFF01)
                { ..mmu, serial_out: mmu.serial_out.append(sb) }.poke(addr, value.bitwise_and(0x7F))
            } else {
                mmu.poke(addr, value)
            }
        } else if addr == 0xFF04 {
            { ..mmu, div_counter: 0 }.poke(addr, 0x00) # DIV: any write resets
        } else if addr == 0xFF00 {
            mmu.poke(addr, value.bitwise_and(0x30).bitwise_or(0xC0)) # only the select bits stick
        } else if addr == 0xFF46 {
            # OAM DMA: instant 160-byte copy from value<<8 (games spin in HRAM
            # during the real transfer, so zero-time is invisible to them)
            src = value.to_u16().shl_wrap(8)
            var m = mmu.poke(addr, value)
            var i = 0.U16
            while i < 0xA0 {
                m = m.poke(U16.plus(0xFE00, i), m.read(src.plus(i)))
                i = i.plus(1)
            }
            m
        } else if addr == 0xFF41 {
            # STAT: bits 0-2 are hardware status, games only write the enables
            mmu.poke(addr, value.bitwise_and(0xF8).bitwise_or(mmu.read(addr).bitwise_and(0x07)))
        } else if addr == 0xFF44 {
            mmu # LY is read-only
        } else {
            mmu.poke(addr, value)
        }

    # MBC register writes land in the ROM address range
    write_mbc : Mmu, U16, U8 -> Mmu
    write_mbc = |mmu, addr, value|
        match mmu.mbc {
            None => mmu
            Mbc1 =>
                if addr < 0x2000 {
                    { ..mmu, ram_enable: value.bitwise_and(0x0F) == 0x0A }
                } else if addr < 0x4000 {
                    { ..mmu, rom_bank: value.bitwise_and(0x1F) }
                } else if addr < 0x6000 {
                    { ..mmu, bank2: value.bitwise_and(0x03) }
                } else {
                    { ..mmu, mode: value.bitwise_and(0x01) == 0x01 }
                }

            Mbc3 =>
                if addr < 0x2000 {
                    { ..mmu, ram_enable: value.bitwise_and(0x0F) == 0x0A }
                } else if addr < 0x4000 {
                    { ..mmu, rom_bank: value.bitwise_and(0x7F) }
                } else if addr < 0x6000 {
                    { ..mmu, bank2: value }
                } else {
                    mmu # RTC latch: ignored
                }
        }

    request_interrupt : Mmu, U8 -> Mmu
    request_interrupt = |mmu, bit|
        mmu.poke(0xFF0F, mmu.read(0xFF0F).bitwise_or(U8.shl_wrap(1, bit)))

    # Advance DIV/TIMA by elapsed T-cycles; TIMA overflow reloads TMA and
    # raises the timer interrupt (IF bit 2)
    tick : Mmu, U64 -> Mmu
    tick = |mmu, cycles| {
        div_total = mmu.div_counter.plus(cycles)
        div_incs = div_total // 256
        with_div =
            if div_incs > 0 {
                div = mmu.read(0xFF04)
                { ..mmu, div_counter: div_total % 256 }.poke(0xFF04, div.plus_wrap(div_incs.to_u8_wrap()))
            } else {
                { ..mmu, div_counter: div_total }
            }
        tac = with_div.read(0xFF07)
        if tac.bitwise_and(0x04) == 0x00 {
            with_div
        } else {
            period =
                match tac.bitwise_and(0x03) {
                    0 => 1024
                    1 => 16
                    2 => 64
                    _ => 256
                }
            tima_total = with_div.tima_counter.plus(cycles)
            tima_incs = tima_total // period
            advanced = { ..with_div, tima_counter: tima_total % period }
            if tima_incs == 0 {
                advanced
            } else {
                sum = advanced.read(0xFF05).to_u64().plus(tima_incs)
                if sum > 0xFF {
                    # Overflow: reload from TMA (wrap the excess through the reload value)
                    tma = advanced.read(0xFF06).to_u64()
                    reloaded = tma.plus(sum.minus(0x100) % U64.minus(0x100, tma))
                    advanced.poke(0xFF05, reloaded.to_u8_wrap()).request_interrupt(2)
                } else {
                    advanced.poke(0xFF05, sum.to_u8_wrap())
                }
            }
        }
    }
}

test_rom : List(U8)
test_rom = List.repeat(0x99, 0x200)

# ROM loads at 0x0000 and is read-only
expect Mmu.init(test_rom).read(0x01FF) == 0x99
expect Mmu.init(test_rom).read(0x0200) == 0xFF # past the image: open bus
expect Mmu.init(test_rom).write(0x01FF, 0x55).read(0x01FF) == 0x99

# Work RAM and HRAM round-trip
expect Mmu.init(test_rom).write(0xC123, 0x5A).read(0xC123) == 0x5A
expect Mmu.init(test_rom).write(0xFF85, 0x77).read(0xFF85) == 0x77

# IE / IF are reachable through the bus
expect Mmu.init(test_rom).write(0xFFFF, 0x1F).read(0xFFFF) == 0x1F
expect Mmu.init(test_rom).request_interrupt(2).read(0xFF0F).bitwise_and(0x04) == 0x04

# Serial capture: SB then SC bit 7 appends to the log ("P" = 0x50)
expect Mmu.init(test_rom).write(0xFF01, 0x50).write(0xFF02, 0x81).serial_out == [0x50]
expect Mmu.init(test_rom).write(0xFF01, 0x50).write(0xFF02, 0x01).serial_out == []

# DIV: resets on write, increments every 256 cycles
expect Mmu.init(test_rom).write(0xFF04, 0x12).read(0xFF04) == 0x00
expect Mmu.init(test_rom).write(0xFF04, 0x00).tick(512).read(0xFF04) == 0x02
expect Mmu.init(test_rom).write(0xFF04, 0x00).tick(255).read(0xFF04) == 0x00

# TIMA: counts at the TAC-selected rate; overflow reloads TMA and sets IF bit 2
expect {
    m = Mmu.init(test_rom).poke(0xFF0F, 0x00).write(0xFF07, 0x05).tick(16) # TAC: enabled, period 16
    m.read(0xFF05) == 0x01 and m.read(0xFF0F).bitwise_and(0x04) == 0x00
}
expect {
    m = Mmu.init(test_rom).poke(0xFF0F, 0x00).write(0xFF07, 0x05).write(0xFF05, 0xFF).write(0xFF06, 0xF0).tick(16)
    m.read(0xFF05) == 0xF0 and m.read(0xFF0F).bitwise_and(0x04) == 0x04
}
# Timer disabled: TIMA holds still
expect Mmu.init(test_rom).write(0xFF07, 0x00).write(0xFF05, 0x10).tick(4096).read(0xFF05) == 0x10

# OAM DMA: sprite table prepared in WRAM lands in OAM
expect {
    var m = Mmu.init(test_rom)
    var i = 0.U16
    while i < 0xA0 {
        m = m.write(U16.plus(0xC000, i), i.to_u8_wrap().bitwise_or(0x40))
        i = i.plus(1)
    }
    m = m.write(0xFF46, 0xC0)
    m.read(0xFE00) == 0x40 and m.read(0xFE9F) == 0xDF
}

# Joypad: select a group, read its buttons active-low
expect {
    m = Mmu.init(test_rom).set_buttons({ ..Mmu.no_buttons({}), a: Bool.True }).write(0xFF00, 0x10)
    m.read(0xFF00).bitwise_and(0x0F) == 0x0E
}
expect {
    m = Mmu.init(test_rom).set_buttons({ ..Mmu.no_buttons({}), down: Bool.True }).write(0xFF00, 0x20)
    m.read(0xFF00).bitwise_and(0x0F) == 0x07
}
# Both groups selected AND together; none selected reads 0xF
expect {
    m = Mmu.init(test_rom).set_buttons({ ..Mmu.no_buttons({}), a: Bool.True, down: Bool.True }).write(0xFF00, 0x00)
    m.read(0xFF00).bitwise_and(0x0F) == 0x06
}
expect {
    m = Mmu.init(test_rom).set_buttons({ ..Mmu.no_buttons({}), a: Bool.True, down: Bool.True }).write(0xFF00, 0x30)
    m.read(0xFF00).bitwise_and(0x0F) == 0x0F
}

# --- MBC banking ---

set_byte : List(U8), U64, U8 -> List(U8)
set_byte = |l, i, v| l.set(i, v) ?? l

# 64-bank (1 MiB) image: first byte of each bank is the bank number
big_rom : U8 -> List(U8)
big_rom = |type_byte| {
    var r = List.repeat(0x00, 0x4000 * 64)
    var b = 0.U64
    while b < 64 {
        r = set_byte(r, b.shl_wrap(14), b.to_u8_wrap())
        b = b.plus(1)
    }
    set_byte(r, 0x0147, type_byte)
}

# MBC1: default window is bank 1; 0x2000 write switches; raw 0 maps to 1
expect Mmu.init(big_rom(0x01)).read(0x4000) == 0x01
expect Mmu.init(big_rom(0x01)).write(0x2000, 0x02).read(0x4000) == 0x02
expect Mmu.init(big_rom(0x01)).write(0x2000, 0x00).read(0x4000) == 0x01
expect Mmu.init(big_rom(0x01)).write(0x2000, 0x02).read(0x0000) == 0x00 # bank 0 fixed

# MBC1: bank2 extends the window bank (0x21 = bank2 1, low 1)
expect Mmu.init(big_rom(0x01)).write(0x2000, 0x01).write(0x4000, 0x01).read(0x4000) == 0x21
# MBC1 mode 1: bank2 also maps the zero region
expect Mmu.init(big_rom(0x01)).write(0x4000, 0x01).write(0x6000, 0x01).read(0x0000) == 0x20
expect Mmu.init(big_rom(0x01)).write(0x4000, 0x01).read(0x0000) == 0x00 # mode 0: pinned

# MBC3: 7-bit bank select, 0 maps to 1
expect Mmu.init(big_rom(0x11)).write(0x2000, 0x05).read(0x4000) == 0x05
expect Mmu.init(big_rom(0x11)).write(0x2000, 0x00).read(0x4000) == 0x01
expect Mmu.init(big_rom(0x11)).write(0x2000, 0x3F).read(0x4000) == 0x3F

# Cartridge RAM: gated by enable; disabled reads 0xFF and drops writes
expect Mmu.init(big_rom(0x03)).write(0xA000, 0x55).read(0xA000) == 0xFF
expect Mmu.init(big_rom(0x03)).write(0x0000, 0x0A).write(0xA000, 0x55).read(0xA000) == 0x55
expect Mmu.init(big_rom(0x03)).write(0x0000, 0x0A).write(0xA000, 0x55).write(0x0000, 0x00).read(0xA000) == 0xFF

# MBC3 RAM banking: each bank keeps its own contents
expect {
    m = Mmu.init(big_rom(0x13))
        .write(0x0000, 0x0A)
        .write(0x4000, 0x00)
        .write(0xA000, 0x01)
        .write(0x4000, 0x01)
        .write(0xA000, 0x02)
    m.write(0x4000, 0x00).read(0xA000) == 0x01 and m.write(0x4000, 0x01).read(0xA000) == 0x02
}
# MBC3 RTC select: reads 0xFF (no clock), writes dropped
expect Mmu.init(big_rom(0x13)).write(0x0000, 0x0A).write(0x4000, 0x08).read(0xA000) == 0xFF

# ROM-only: register writes are inert, RAM region is plain storage
expect Mmu.init(test_rom).write(0x2000, 0x02).read(0x01FF) == 0x99
expect Mmu.init(test_rom).write(0xA000, 0x77).read(0xA000) == 0x77
