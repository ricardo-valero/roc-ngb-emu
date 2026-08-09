# Flat 64 KiB memory bus. Only `read`/`write` touch `mem` so the list stays
# uniquely owned and Roc can mutate it in place (never alias it elsewhere).
# https://gbdev.io/pandocs/Memory_Map.html

Mmu := {
    mem : List(U8),
    serial_out : List(U8),
    div_counter : U64,
    tima_counter : U64,
}.{
    # No MBC banking: individual Blargg cpu_instrs ROMs fit in 32 KiB
    init : List(U8) -> Mmu
    init = |rom| {
        rom_len = if rom.len() > 0x8000 { 0x8000 } else { rom.len() }
        mem = rom
            .sublist({ start: 0, len: rom_len })
            .concat(List.repeat(0, 0x10000 - rom_len))
        base : Mmu
        base = { mem: mem, serial_out: [], div_counter: 0, tima_counter: 0 }
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
    read = |mmu, addr| mmu.mem.get(addr.to_u64()) ?? 0xFF

    serial : Mmu -> List(U8)
    serial = |mmu| mmu.serial_out

    # Raw store bypassing region semantics (init defaults, timer internals)
    poke : Mmu, U16, U8 -> Mmu
    poke = |mmu, addr, value|
        { ..mmu, mem: mmu.mem.set(addr.to_u64(), value) ?? mmu.mem }

    write : Mmu, U16, U8 -> Mmu
    write = |mmu, addr, value|
        if addr < 0x8000 {
            mmu # cartridge ROM: writes ignored (MBC banking out of scope)
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
expect Mmu.init(test_rom).read(0x0200) == 0x00
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
