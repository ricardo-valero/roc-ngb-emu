# The cartridge PCB: ROM, the MBC banking controller, external RAM, the
# MBC3 real-time clock, and battery signaling. The bus (Bus) decodes
# addresses and delegates the 0x0000-0x7FFF and 0xA000-0xBFFF regions
# here; wall-clock `now` (UNIX seconds) arrives as an argument at the
# RTC's observation points, keeping headless callers deterministic.
# Only this module's functions touch `rom` and `ram`, so both lists stay
# uniquely owned and Roc can mutate `ram` in place.

# MBC3 real-time clock. Raw register bytes as hardware packs them: dh
# carries day bit 8 (bit 0), halt (bit 6), and the sticky day-overflow
# carry (bit 7). `latched` is the copy games read after the 0x00->0x01
# latch; `last_now` is the wall-clock second the running registers were
# last advanced to (0 = never synced: adopt the next `now` without
# adding elapsed time, so fresh carts and headless runs start at zero).
RtcRegs : { s : U8, m : U8, h : U8, dl : U8, dh : U8 }
RtcState : { regs : RtcRegs, latched : RtcRegs, last_now : U64, armed : Bool }

rtc_init : {} -> RtcState
rtc_init = |_| {
    regs: { s: 0, m: 0, h: 0, dl: 0, dh: 0 },
    latched: { s: 0, m: 0, h: 0, dl: 0, dh: 0 },
    last_now: 0,
    armed: Bool.False,
}

# Advance the running clock to `now` — called lazily at the observation
# points only (latch, register write, battery extraction). Halted clocks
# (dh bit 6) adopt `now` without accumulating, as does a clock that was
# never synced or sees time move backwards. Out-of-range register values
# are normalized through total-seconds arithmetic (a documented
# simplification; hardware would count them oddly).
rtc_advance : RtcState, U64 -> RtcState
rtc_advance = |r, now| {
    halted = r.regs.dh.bitwise_and(0x40) != 0x00
    if now == 0 {
        r # no clock supplied (headless caller): hold everything as-is
    } else if r.last_now == 0 or halted or now <= r.last_now {
        { ..r, last_now: now }
    } else {
        days = r.regs.dh.bitwise_and(0x01).to_u64().shl_wrap(8).plus(r.regs.dl.to_u64())
        held = (days * 86400).plus(r.regs.h.to_u64() * 3600).plus(r.regs.m.to_u64() * 60).plus(r.regs.s.to_u64())
        total = held.plus(now.minus(r.last_now))
        d = total // 86400
        carry = if d > 511 { 0x80 } else { r.regs.dh.bitwise_and(0x80) }
        day_hi = if d % 512 >= 256 { 0x01.U8 } else { 0x00.U8 }
        dh = carry.bitwise_or(r.regs.dh.bitwise_and(0x40)).bitwise_or(day_hi)
        regs = {
            s: (total % 60).to_u8_wrap(),
            m: ((total // 60) % 60).to_u8_wrap(),
            h: ((total // 3600) % 24).to_u8_wrap(),
            dl: (d % 256).to_u8_wrap(),
            dh: dh,
        }
        { ..r, regs: regs, last_now: now }
    }
}

Cartridge := {
    rom : List(U8), # full cartridge image, bank-mapped on read
    ram : List(U8), # 32 KiB external RAM, session lifetime
    mbc : [None, Mbc1, Mbc3, Mbc5],
    rom_bank : U8, # raw register value; 0->1 translation happens at read (not MBC5)
    rom_bank_hi : U8, # MBC5 ninth ROM bank bit (0x3000-0x3FFF register)
    bank2 : U8, # MBC1 secondary register / MBC3+MBC5 RAM bank (or RTC select)
    mode : Bool, # MBC1 banking mode
    ram_enable : Bool,
    rtc : [None, Rtc(RtcState)], # present on MBC3 Timer carts only
    ram_written : Bool, # battery state touched since RAM was last enabled
    save_events : U64, # bumped on RAM-disable-after-write: the "game just saved" signal
}.{
    init : List(U8) -> Cartridge
    init = |rom| {
        type_byte = rom.get(0x0147) ?? 0x00
        mbc =
            if type_byte >= 0x01 and type_byte <= 0x03 {
                Mbc1
            } else if type_byte >= 0x0F and type_byte <= 0x13 {
                Mbc3
            } else if type_byte >= 0x19 and type_byte <= 0x1E {
                Mbc5
            } else {
                None
            }
        {
            rom: rom,
            ram: List.repeat(0, 0x20000), # 128 KiB: MBC5's max (16 banks); others mask lower
            mbc: mbc,
            rom_bank: 1,
            rom_bank_hi: 0,
            bank2: 0,
            mode: Bool.False,
            ram_enable: Bool.False,
            rtc: if type_byte == 0x0F or type_byte == 0x10 { Rtc(rtc_init({})) } else { None },
            ram_written: Bool.False,
            save_events: 0,
        }
    }

    # ROM region (0x0000-0x7FFF) as the game sees it through the MBC
    read_rom : Cartridge, U16 -> U8
    read_rom = |cart, addr|
        if addr < 0x4000 {
            cart.rom.get(zero_region_base(cart).shl_wrap(14).plus(addr.to_u64())) ?? 0xFF
        } else {
            cart.rom.get(switch_region_bank(cart).shl_wrap(14).plus(addr.to_u64().minus(0x4000))) ?? 0xFF
        }

    bank_count : Cartridge -> U64
    bank_count = |cart| {
        n = cart.rom.len().shr_zf_wrap(14)
        if n == 0 { 1 } else { n }
    }

    # Effective bank for 0x4000-0x7FFF; the raw register's zero maps to 1
    # before combining, and the result wraps to the ROM's actual bank count
    # the way real cartridge address wiring does.
    switch_region_bank : Cartridge -> U64
    switch_region_bank = |cart|
        match cart.mbc {
            None => 1
            Mbc1 => {
                low5 = cart.rom_bank.bitwise_and(0x1F)
                low = if low5 == 0x00 { 1 } else { low5.to_u64() }
                cart.bank2.bitwise_and(0x03).to_u64().shl_wrap(5).plus(low) % bank_count(cart)
            }

            Mbc3 => {
                b = cart.rom_bank.bitwise_and(0x7F)
                (if b == 0x00 { 1 } else { b.to_u64() }) % bank_count(cart)
            }

            # 9-bit bank, and bank 0 IS selectable (no zero->one translation)
            Mbc5 =>
                cart.rom_bank_hi.bitwise_and(0x01).to_u64().shl_wrap(8).plus(cart.rom_bank.to_u64()) % bank_count(cart)
        }

    # 0x0000-0x3FFF is bank 0 except MBC1 mode 1, where bank2 maps it
    zero_region_base : Cartridge -> U64
    zero_region_base = |cart|
        match cart.mbc {
            Mbc1 =>
                if cart.mode {
                    cart.bank2.bitwise_and(0x03).to_u64().shl_wrap(5) % bank_count(cart)
                } else {
                    0
                }

            _ => 0
        }

    # MBC1 banks RAM by bank2 in mode 1 only; MBC3 by bank2 values 0-3
    # (RTC selects 0x08+ read as 0 and drop writes); None is ungated bank 0.
    ram_slot : Cartridge -> [Bank(U64), Invalid]
    ram_slot = |cart|
        match cart.mbc {
            None => Bank(0)
            Mbc1 =>
                if cart.ram_enable {
                    if cart.mode { Bank(cart.bank2.bitwise_and(0x03).to_u64()) } else { Bank(0) }
                } else {
                    Invalid
                }

            Mbc3 =>
                if cart.ram_enable and cart.bank2 <= 0x03 {
                    Bank(cart.bank2.to_u64())
                } else {
                    Invalid
                }

            Mbc5 =>
                if cart.ram_enable {
                    Bank(cart.bank2.bitwise_and(0x0F).to_u64())
                } else {
                    Invalid
                }
        }

    # External-RAM region (0xA000-0xBFFF): banked RAM, or the latched RTC
    # register when the 0x4000 select holds a clock index
    read_ram : Cartridge, U16 -> U8
    read_ram = |cart, addr|
        match rtc_reg_selected(cart) {
            Some(reg) => reg
            None =>
                match ram_slot(cart) {
                    Bank(b) => cart.ram.get(b.shl_wrap(13).plus(addr.to_u64().minus(0xA000))) ?? 0xFF
                    Invalid => 0xFF
                }
        }

    write_ram : Cartridge, U16, U8, U64 -> Cartridge
    write_ram = |cart, addr, value, now|
        match write_rtc_reg(cart, value, now) {
            Done(c) => c
            NotRtc =>
                match ram_slot(cart) {
                    Bank(b) => { ..cart, ram: cart.ram.set(b.shl_wrap(13).plus(addr.to_u64().minus(0xA000)), value) ?? cart.ram, ram_written: Bool.True }
                    Invalid => cart
                }
        }

    # The latched RTC register mapped over 0xA000-0xBFFF, if the 0x4000
    # select holds a clock index (0x08-0x0C) — gated like cart RAM.
    rtc_reg_selected : Cartridge -> [None, Some(U8)]
    rtc_reg_selected = |cart|
        match cart.rtc {
            Rtc(r) =>
                if cart.ram_enable and cart.bank2 >= 0x08 and cart.bank2 <= 0x0C {
                    match cart.bank2 {
                        0x08 => Some(r.latched.s)
                        0x09 => Some(r.latched.m)
                        0x0A => Some(r.latched.h)
                        0x0B => Some(r.latched.dl)
                        _ => Some(r.latched.dh)
                    }
                } else {
                    None
                }

            None => None
        }

    # Writes land on the running clock, masked to the bits hardware
    # implements (s/m: 6, h: 5, dh: day bit 8 + halt + carry).
    write_rtc_reg : Cartridge, U8, U64 -> [Done(Cartridge), NotRtc]
    write_rtc_reg = |cart, value, now|
        match cart.rtc {
            Rtc(r0) =>
                if cart.ram_enable and cart.bank2 >= 0x08 and cart.bank2 <= 0x0C {
                    r = rtc_advance(r0, now)
                    regs =
                        match cart.bank2 {
                            0x08 => { ..r.regs, s: value.bitwise_and(0x3F) }
                            0x09 => { ..r.regs, m: value.bitwise_and(0x3F) }
                            0x0A => { ..r.regs, h: value.bitwise_and(0x1F) }
                            0x0B => { ..r.regs, dl: value }
                            _ => { ..r.regs, dh: value.bitwise_and(0xC1) }
                        }
                    Done({ ..cart, rtc: Rtc({ ..r, regs: regs }), ram_written: Bool.True })
                } else {
                    NotRtc
                }

            None => NotRtc
        }

    # RAM enable, plus the save heuristic: games disable RAM right after
    # writing a save to protect the SRAM — that falling edge (with writes
    # since the last enable) bumps `save_events` so frontends know to
    # flush battery bytes now instead of waiting for exit.
    set_ram_enable : Cartridge, U8 -> Cartridge
    set_ram_enable = |cart, value| {
        enabled = value.bitwise_and(0x0F) == 0x0A
        if cart.ram_enable and enabled == Bool.False and cart.ram_written {
            { ..cart, ram_enable: enabled, ram_written: Bool.False, save_events: cart.save_events.plus(1) }
        } else {
            { ..cart, ram_enable: enabled }
        }
    }

    # MBC register writes land in the ROM address range
    write_control : Cartridge, U16, U8, U64 -> Cartridge
    write_control = |cart, addr, value, now|
        match cart.mbc {
            None => cart
            Mbc1 =>
                if addr < 0x2000 {
                    set_ram_enable(cart, value)
                } else if addr < 0x4000 {
                    { ..cart, rom_bank: value.bitwise_and(0x1F) }
                } else if addr < 0x6000 {
                    { ..cart, bank2: value.bitwise_and(0x03) }
                } else {
                    { ..cart, mode: value.bitwise_and(0x01) == 0x01 }
                }

            Mbc3 =>
                if addr < 0x2000 {
                    set_ram_enable(cart, value)
                } else if addr < 0x4000 {
                    { ..cart, rom_bank: value.bitwise_and(0x7F) }
                } else if addr < 0x6000 {
                    { ..cart, bank2: value }
                } else {
                    # RTC latch: 0x00 then 0x01 captures the running clock
                    # into the readable registers atomically
                    match cart.rtc {
                        Rtc(r) =>
                            if value == 0x00 {
                                { ..cart, rtc: Rtc({ ..r, armed: Bool.True }) }
                            } else if value == 0x01 and r.armed {
                                r2 = rtc_advance(r, now)
                                { ..cart, rtc: Rtc({ ..r2, latched: r2.regs, armed: Bool.False }) }
                            } else {
                                { ..cart, rtc: Rtc({ ..r, armed: Bool.False }) }
                            }

                        None => cart
                    }
                }

            Mbc5 =>
                if addr < 0x2000 {
                    set_ram_enable(cart, value)
                } else if addr < 0x3000 {
                    { ..cart, rom_bank: value } # full 8 bits, 0 allowed
                } else if addr < 0x4000 {
                    { ..cart, rom_bank_hi: value.bitwise_and(0x01) }
                } else if addr < 0x6000 {
                    { ..cart, bank2: value.bitwise_and(0x0F) } # rumble carts' motor bit masked off
                } else {
                    cart
                }
        }

    set_ram : Cartridge, List(U8) -> Cartridge
    set_ram = |cart, ram| { ..cart, ram: ram }

    # The `.sav` RTC footer, 48-byte form: little-endian dwords holding
    # the running clock (advanced to `now` — extraction is an observation
    # point), the latched copies, then a 64-bit UNIX timestamp. Loaders
    # use the timestamp to advance the clock by wall time spent off.
    # Empty for carts without an RTC.
    battery_footer : Cartridge, U64 -> List(U8)
    battery_footer = |cart, now|
        match cart.rtc {
            Rtc(r0) => {
                r = rtc_advance(r0, now)
                var out = [
                    r.regs.s, 0, 0, 0,
                    r.regs.m, 0, 0, 0,
                    r.regs.h, 0, 0, 0,
                    r.regs.dl, 0, 0, 0,
                    r.regs.dh, 0, 0, 0,
                    r.latched.s, 0, 0, 0,
                    r.latched.m, 0, 0, 0,
                    r.latched.h, 0, 0, 0,
                    r.latched.dl, 0, 0, 0,
                    r.latched.dh, 0, 0, 0,
                ]
                # After the advance this is `now` whenever a clock was
                # supplied; headless extraction preserves the loaded stamp
                var t = r.last_now
                var i = 0
                while i < 8 {
                    out = out.append(t.to_u8_wrap())
                    t = t.shr_zf_wrap(8)
                    i = i.plus(1)
                }
                out
            }

            None => []
        }

    # Restore RTC state from a `.sav` footer — either the 48-byte form
    # (64-bit timestamp) or legacy 44-byte (32-bit). The clock is NOT
    # advanced here; the stored timestamp becomes `last_now`, so the next
    # observation point applies the elapsed wall time (a halted clock
    # stays put there too). No-op for carts without an RTC.
    load_battery_footer : Cartridge, List(U8) -> Cartridge
    load_battery_footer = |cart, f|
        match cart.rtc {
            Rtc(_) => {
                g = |i| f.get(i) ?? 0
                regs = { s: g(0), m: g(4), h: g(8), dl: g(12), dh: g(16) }
                latched = { s: g(20), m: g(24), h: g(28), dl: g(32), dh: g(36) }
                var t = 0.U64
                var i = if f.len() >= 48 { 8.U64 } else { 4.U64 }
                while i > 0 {
                    i = i.minus(1)
                    t = t.shl_wrap(8).plus(g(40.U64.plus(i)).to_u64())
                }
                { ..cart, rtc: Rtc({ regs: regs, latched: latched, last_now: t, armed: Bool.False }) }
            }

            None => cart
        }
}

# --- test helpers ---

test_set_byte : List(U8), U64, U8 -> List(U8)
test_set_byte = |l, i, v| l.set(i, v) ?? l

# 64-bank (1 MiB) image: first byte of each bank is the bank number
test_rom : U8 -> List(U8)
test_rom = |type_byte| {
    var r = List.repeat(0x00, 0x4000 * 64)
    var b = 0.U64
    while b < 64 {
        r = test_set_byte(r, b.shl_wrap(14), b.to_u8_wrap())
        b = b.plus(1)
    }
    test_set_byte(r, 0x0147, type_byte)
}

# MBC detection from the header type byte
expect Cartridge.init(test_rom(0x00)).mbc == None
expect Cartridge.init(test_rom(0x01)).mbc == Mbc1
expect Cartridge.init(test_rom(0x13)).mbc == Mbc3
expect Cartridge.init(test_rom(0x1A)).mbc == Mbc5
expect Cartridge.init(test_rom(0x10)).rtc != None
expect Cartridge.init(test_rom(0x11)).rtc == None

# RTC catch-up on load: the footer timestamp becomes last_now, so the
# next latch advances by the wall time spent off (90 s -> 1 min 30 s)
expect {
    save = Cartridge.init(test_rom(0x10)).battery_footer(1000)
    c =
        Cartridge.init(test_rom(0x10))
            .load_battery_footer(save)
            .write_control(0x6000, 0x00, 1090)
            .write_control(0x6000, 0x01, 1090)
            .write_control(0x0000, 0x0A, 1090)
            .write_control(0x4000, 0x08, 1090)
    c.read_ram(0xA000) == 30 and c.write_control(0x4000, 0x09, 1090).read_ram(0xA000) == 1
}

# A halted clock does not catch up across a save/load
expect {
    save =
        Cartridge.init(test_rom(0x10))
            .write_control(0x0000, 0x0A, 1000)
            .write_control(0x4000, 0x0C, 1000)
            .write_ram(0xA000, 0x40, 1000)
            .battery_footer(1000)
    c =
        Cartridge.init(test_rom(0x10))
            .load_battery_footer(save)
            .write_control(0x6000, 0x00, 999999)
            .write_control(0x6000, 0x01, 999999)
            .write_control(0x0000, 0x0A, 999999)
            .write_control(0x4000, 0x08, 999999)
    c.read_ram(0xA000) == 0x00 and c.write_control(0x4000, 0x0C, 999999).read_ram(0xA000) == 0x40
}

# Legacy 44-byte footers load; re-saving writes the 48-byte form with
# the timestamp intact in the low dword
expect {
    f48 = Cartridge.init(test_rom(0x10)).battery_footer(70000)
    f44 = f48.sublist({ start: 0, len: 44 })
    resaved = Cartridge.init(test_rom(0x10)).load_battery_footer(f44).battery_footer(0)
    ts_lo = (resaved.get(40) ?? 0).to_u64().plus((resaved.get(41) ?? 0).to_u64().shl_wrap(8)).plus((resaved.get(42) ?? 0).to_u64().shl_wrap(16))
    f48.len() == 48 and f44.len() == 44 and resaved.len() == 48 and ts_lo == 70000
}

# Batteryless carts produce no footer
expect Cartridge.init(test_rom(0x01)).battery_footer(1000) == []
