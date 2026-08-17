# SoC composition: the DMG-CPU chip — the one place the CPU core, bus,
# PPU, and APU are wired together and scheduled — plus the
# harness/frontend API (raw single-step machines, battery, breakpoints,
# frame stepping).

import /Cartridge
import /Cartridge/Header
import /Joypad
import /Cpu
import /Cpu/Register
import /Cpu/Register/Status
import /Apu
import /Bus
import /Ppu

# Raw machine state for the single-step harness: every register, IME, and
# a flat 64 KiB memory image with no address mapping. See check/single-step.
SingleStep : { pc : U16, sp : U16, a : U8, b : U8, c : U8, d : U8, e : U8, f : U8, h : U8, l : U8, ime : Bool, mem : List(U8) }

GameBoy := {
    cpu : Cpu,
    bus : Bus,
    ppu : Ppu,
    apu : Apu,
    breakpoint : [None, At(U16)],
}.{
    init : List(U8) -> GameBoy
    init = |rom| {
        gb : GameBoy
        gb = {
            cpu: Cpu.init({}),
            bus: Bus.init(rom),
            ppu: Ppu.init({}),
            apu: Apu.init({}),
            breakpoint: None,
        }
        # CGB carts boot with A = 0x11 — how games detect the console
        if gb.bus.is_cgb() {
            { ..gb, cpu: { ..gb.cpu, reg: gb.cpu.reg.write8(Accumulator, 0x11) } }
        } else {
            gb
        }
    }

    serial : GameBoy -> List(U8)
    serial = |gb| gb.bus.serial()

    # Bus read for harnesses (e.g. Blargg's memory-reporting test protocol)
    peek : GameBoy, U16 -> U8
    peek = |gb, addr| gb.bus.read(addr)

    # Bus write for harnesses — peek's counterpart: the mapped write, MBC
    # control included (no access trace; that lives on the CPU path)
    poke : GameBoy, U16, U8 -> GameBoy
    poke = |gb, addr, value| { ..gb, bus: gb.bus.write(addr, value) }

    framebuffer : GameBoy -> List(U16)
    framebuffer = |gb| gb.ppu.frame()

    # Debug renders (see Ppu): 256x256 background map, 128x192 tile sheet,
    # 64x80 OAM grid — RGB555 pixels, blittable by any host
    debug_background : GameBoy -> List(U16)
    debug_background = |gb| Ppu.debug_background(gb.bus)

    debug_tiles : GameBoy -> List(U16)
    debug_tiles = |gb| Ppu.debug_tiles(gb.bus)

    debug_oam : GameBoy -> List(U16)
    debug_oam = |gb| Ppu.debug_oam(gb.bus)

    # Neutral per-frame input: no buttons, epoch zero. `now` is wall-clock
    # UNIX seconds, entering the pure core as data — a constant here keeps
    # every headless caller deterministic by construction.
    no_input = |_| { buttons: Joypad.none({}), now: 0.U64 }

    # Battery-backed cartridge state as `.sav` bytes: cart RAM sized to
    # the header's declaration, not the internal allocation — the format
    # every emulator and flashcart reads. RTC carts append the 48-byte
    # clock footer. Empty for batteryless carts.
    battery : GameBoy -> List(U8)
    battery = |gb|
        if Header.has_battery(gb.bus.cart.rom) {
            gb.bus.cart.ram
                .sublist({ start: 0, len: Header.ram_bytes(gb.bus.cart.rom) })
                .concat(gb.bus.cart.battery_footer(gb.bus.now))
        } else {
            []
        }

    # Restore bytes saved by `battery` (or any emulator's `.sav`): bare
    # RAM, RAM + 44-byte RTC footer, and RAM + 48-byte footer all load.
    # How supplied .sav bytes fit this cartridge. `with_battery` stays
    # forgiving (pad/truncate, below), but silence was the flaw: a wrong or
    # corrupt sav would boot a garbage save with no trace. Frontends SHOULD
    # surface anything except Exact/Empty/NoBattery. Accepted-as-Exact:
    # the declared RAM size bare, or with a 44-48 byte RTC footer (the
    # legacy 44-byte form loads too).
    battery_fit : GameBoy, List(U8) -> [NoBattery, Empty, Exact, Short(U64), Long(U64)]
    battery_fit = |gb, bytes|
        if Header.has_battery(gb.bus.cart.rom) == Bool.False {
            NoBattery
        } else if bytes.len() == 0 {
            Empty
        } else {
            declared = Header.ram_bytes(gb.bus.cart.rom)
            len = bytes.len()
            if len == declared or (len >= declared.plus(44) and len <= declared.plus(48)) {
                Exact
            } else if len < declared {
                Short(declared.minus(len))
            } else {
                Long(len.minus(declared))
            }
        }

    # Short payloads are zero-padded, long ones truncated — loading never
    # fails on size grounds. No-op for batteryless carts.
    with_battery : GameBoy, List(U8) -> GameBoy
    with_battery = |gb, bytes|
        if Header.has_battery(gb.bus.cart.rom) {
            declared = Header.ram_bytes(gb.bus.cart.rom)
            kept = bytes.sublist({ start: 0, len: declared })
            ram = kept.concat(List.repeat(0, gb.bus.cart.ram.len().minus(kept.len())))
            footer = bytes.sublist({ start: declared, len: 48 })
            cart1 = gb.bus.cart.set_ram(ram)
            cart2 = if footer.len() >= 44 { cart1.load_battery_footer(footer) } else { cart1 }
            { ..gb, bus: gb.bus.set_cart(cart2) }
        } else {
            gb
        }

    # Monotonic count of "the game just saved" moments (cart RAM disabled
    # after being written). Frontends diff it across frames and flush
    # `battery` bytes to storage when it moves.
    save_events : GameBoy -> U64
    save_events = |gb| gb.bus.cart.save_events

    # Drain the APU's generated 48 kHz interleaved stereo samples
    take_samples : GameBoy -> { gb : GameBoy, samples : List(F32) }
    take_samples = |gb| {
        t = gb.bus.take_samples()
        { gb: { ..gb, bus: t.bus }, samples: t.samples }
    }

    set_breakpoint : GameBoy, U16 -> GameBoy
    set_breakpoint = |gb, addr| { ..gb, breakpoint: At(addr) }

    clear_breakpoint : GameBoy -> GameBoy
    clear_breakpoint = |gb| { ..gb, breakpoint: None }

    at_breakpoint : GameBoy, U16 -> Bool
    at_breakpoint = |gb, pc|
        match gb.breakpoint {
            At(addr) => addr == pc
            None => Bool.False
        }

    # Run until the next VBlank entry (LY reaching 144) or the breakpoint,
    # bounded so a wedged ROM cannot hang the caller. A frame is ~17.6k
    # steps even when halted. The breakpoint check is skipped before the
    # first step so a machine stopped at the breakpoint resumes past it.
    run_until : GameBoy, _ -> (GameBoy, [FrameReady, BreakpointHit])
    run_until = |gb0, input| {
        var gb = { ..gb0, bus: gb0.bus.set_input(input) }
        var budget = 40000.U64
        var stopped = Bool.False
        var hit = Bool.False
        var first = Bool.True
        while budget > 0 and stopped == Bool.False {
            if first == Bool.False and at_breakpoint(gb, gb.cpu.reg.read16(ProgramCounter)) {
                stopped = Bool.True
                hit = Bool.True
            } else {
                was_ly = gb.bus.read(0xFF44)
                gb = match gb.step() { (g, _) => g }
                if was_ly != 144 and gb.bus.read(0xFF44) == 144 {
                    stopped = Bool.True
                } else {
                    {}
                }
            }
            first = Bool.False
            budget = budget.minus(1)
        }
        if hit { (gb, BreakpointHit) } else { (gb, FrameReady) }
    }

    run_frame : GameBoy, _ -> GameBoy
    run_frame = |gb0, input| match gb0.run_until(input) { (gb, _) => gb }

    # --- single-step harness (SingleStepTests vectors) ---

    # Build a machine from raw state: registers verbatim, flat memory,
    # access trace on. Nothing else of the machine is live — the PPU and
    # APU exist but never tick through this surface.
    from_raw : SingleStep -> GameBoy
    from_raw = |s| {
        gb : GameBoy
        gb = {
            cpu: {
                reg: Register.init({})
                    .write16(ProgramCounter, s.pc)
                    .write16(StackPointer, s.sp)
                    .write8(Accumulator, s.a)
                    .write8(B, s.b)
                    .write8(C, s.c)
                    .write8(D, s.d)
                    .write8(E, s.e)
                    .write8(Status, s.f)
                    .write8(H, s.h)
                    .write8(L, s.l),
                ime: s.ime,
                halted: Bool.False,
                ei_pending: Bool.False,
            },
            bus: Bus.flat(s.mem),
            ppu: Ppu.init({}),
            apu: Apu.init({}),
            breakpoint: None,
        }
        gb
    }

    # Read raw state back out (inverse of from_raw for flat machines)
    raw : GameBoy -> SingleStep
    raw = |gb| {
        pc: gb.cpu.reg.read16(ProgramCounter),
        sp: gb.cpu.reg.read16(StackPointer),
        a: gb.cpu.reg.read8(Accumulator),
        b: gb.cpu.reg.read8(B),
        c: gb.cpu.reg.read8(C),
        d: gb.cpu.reg.read8(D),
        e: gb.cpu.reg.read8(E),
        f: gb.cpu.reg.read8(Status),
        h: gb.cpu.reg.read8(H),
        l: gb.cpu.reg.read8(L),
        ime: gb.cpu.ime,
        mem: gb.bus.mem,
    }

    # Execute exactly one instruction: no interrupt poll or dispatch, no
    # timer/PPU/APU ticks. What the vectors mean by "step".
    step_instruction : GameBoy -> (GameBoy, U64)
    step_instruction = |gb| {
        r = gb.cpu.step_instruction(gb.bus)
        ({ ..gb, cpu: r.cpu, bus: r.bus }, r.cycles)
    }

    # The ordered memory accesses of everything stepped so far (empty
    # unless the machine was built by from_raw)
    access_trace : GameBoy -> List({ addr : U16, val : U8, dir : [Read, Write] })
    access_trace = |gb|
        match gb.bus.trace {
            Trace(list) => list
            NoTrace => []
        }

    step : GameBoy -> (GameBoy, U64)
    step = |gb| {
        r = gb.cpu.step(gb.bus)
        finish({ ..gb, cpu: r.cpu, bus: r.bus }, r.cycles)
    }

    # Every path leaves through here so the timer, PPU, and APU see all
    # cycles. Timers are CPU-clocked (full count even in double speed);
    # the PPU and APU run in real time, so double speed feeds them half.
    finish : GameBoy, U64 -> (GameBoy, U64)
    finish = |gb, cycles| {
        video_cycles = if gb.bus.double_speed { cycles // 2 } else { cycles }
        r = gb.ppu.tick(gb.bus.tick(cycles), video_cycles)
        a = gb.apu.tick(r.bus, video_cycles)
        ({ ..gb, bus: a.bus, ppu: r.ppu, apu: a.apu }, cycles)
    }
}

# --- step semantics ---

rom_with : List(U8) -> List(U8)
rom_with = |code| List.repeat(0x00, 0x100).concat(code) # entry point is 0x0100

after_step : GameBoy -> GameBoy
after_step = |gb| match gb.step() { (g, _) => g }

cycles_of : GameBoy -> U64
cycles_of = |gb| match gb.step() { (_, c) => c }

# CGB console detection: A = 0x11 with the header flag, 0x01 without
expect {
    plain = List.repeat(0x00.U8, 0x8000)
    cgb = plain.set(0x0143, 0x80) ?? plain
    GameBoy.init(cgb).cpu.reg.read8(Accumulator) == 0x11
    and GameBoy.init(plain).cpu.reg.read8(Accumulator) == 0x01
}

# LD A, 0x2A: A loaded, PC advanced by 2, 8 cycles
expect {
    g = after_step(GameBoy.init(rom_with([0x3E, 0x2A])))
    g.cpu.reg.read8(Accumulator) == 0x2A and g.cpu.reg.read16(ProgramCounter) == 0x0102
}
expect cycles_of(GameBoy.init(rom_with([0x3E, 0x2A]))) == 8

# Conditional cycle split: post-boot F has Z set, so NZ misses and Z hits
expect cycles_of(GameBoy.init(rom_with([0x20, 0x05]))) == 8
expect {
    match GameBoy.init(rom_with([0x28, 0x05])).step() {
        (g, c) => c == 12 and g.cpu.reg.read16(ProgramCounter) == 0x0107
    }
}

# ALU writeback: XOR A clears A and sets only Z
expect {
    g = after_step(GameBoy.init(rom_with([0xAF])))
    g.cpu.reg.read8(Accumulator) == 0x00 and g.cpu.reg.read8(Status) == 0b1000_0000
}

# CB path: SET 7, A then BIT 7, A
expect {
    g = after_step(after_step(GameBoy.init(rom_with([0xCB, 0xFF, 0xCB, 0x7F]))))
    g.cpu.reg.read8(Accumulator).bitwise_and(0x80) == 0x80 and Status.check(Zero, g.cpu.reg.read8(Status)) == Bool.False
}

# Interrupt dispatch: PC pushed, vector 0x40 taken, IME and IF bit cleared, 20 cycles
expect {
    gb0 = GameBoy.init(rom_with([0x00]))
    gb1 = { ..gb0, cpu: { ..gb0.cpu, ime: Bool.True }, bus: gb0.bus.write(0xFFFF, 0x01).poke(0xFF0F, 0x01) }
    match gb1.step() {
        (g, c) =>
            c == 20
            and g.cpu.reg.read16(ProgramCounter) == 0x0040
            and g.cpu.ime == Bool.False
            and g.bus.read(0xFF0F).bitwise_and(0x01) == 0x00
            and g.bus.read(0xFFFD) == 0x01
            and g.bus.read(0xFFFC) == 0x00
    }
}

# EI delay: the instruction after EI runs before dispatch
expect {
    gb0 = GameBoy.init(rom_with([0xFB, 0x00])) # EI; NOP
    gb1 = { ..gb0, bus: gb0.bus.write(0xFFFF, 0x01).poke(0xFF0F, 0x01) }
    g2 = after_step(after_step(gb1)) # EI, then NOP still executes
    g3 = after_step(g2) # now the interrupt dispatches
    g2.cpu.reg.read16(ProgramCounter) == 0x0102 and g3.cpu.reg.read16(ProgramCounter) == 0x0040
}

# HALT wakes without IME: resumes after HALT, no vector taken
expect {
    gb0 = GameBoy.init(rom_with([0x76, 0x3E, 0x07])) # HALT; LD A, 0x07
    # post-boot IF is 0xE1, so clear it or the HALT wakes immediately
    gb1 = { ..gb0, bus: gb0.bus.write(0xFFFF, 0x01).poke(0xFF0F, 0x00) }
    g1 = after_step(gb1) # halts
    g2 = after_step(g1) # nothing pending: stays halted
    g3 = after_step({ ..g2, bus: g2.bus.poke(0xFF0F, 0x01) }) # wakes, runs the LD
    g1.cpu.halted == Bool.True
    and g2.cpu.halted == Bool.True
    and g3.cpu.reg.read8(Accumulator) == 0x07
    and g3.cpu.reg.read16(ProgramCounter) == 0x0103
    and g3.cpu.ime == Bool.False
}

# Stack round-trip: PUSH BC / POP DE
expect {
    g = after_step(after_step(GameBoy.init(rom_with([0xC5, 0xD1]))))
    g.cpu.reg.read16(DE) == g.cpu.reg.read16(BC) and g.cpu.reg.read16(StackPointer) == 0xFFFE
}

# CALL pushes the return address; RET comes back
expect {
    # CALL 0x0110; NOP... at 0x0110: RET
    code = [0xCD, 0x10, 0x01].concat(List.repeat(0x00, 13)).concat([0xC9])
    g1 = after_step(GameBoy.init(rom_with(code)))
    g2 = after_step(g1)
    g1.cpu.reg.read16(ProgramCounter) == 0x0110 and g2.cpu.reg.read16(ProgramCounter) == 0x0103
}

# Frame stepping: returns at VBlank entry with a full framebuffer of shades
expect {
    gb = GameBoy.init(rom_with([0x18, 0xFE])).run_frame(GameBoy.no_input({})) # JR -2: tight loop
    fb = gb.framebuffer()
    gb.bus.read(0xFF44) == 144
    and fb.len() == 23040
    and fb.fold(Bool.True, |ok, px| ok and (px == 0x7FFF or px == 0x56B5 or px == 0x294A or px == 0x0000))
}

# run_until without a breakpoint stops for the frame, like run_frame
expect {
    match GameBoy.init(rom_with([0x18, 0xFE])).run_until(GameBoy.no_input({})) {
        (gb, FrameReady) => gb.bus.read(0xFF44) == 144
        (_, BreakpointHit) => Bool.False
    }
}

# Breakpoint hit: NOP at 0x0100, JR -2 self-loop at 0x0101. Stop lands on
# the breakpoint address with its instruction not yet executed.
expect {
    gb0 = GameBoy.init(rom_with([0x00, 0x18, 0xFE])).set_breakpoint(0x0101)
    match gb0.run_until(GameBoy.no_input({})) {
        (gb, BreakpointHit) => gb.cpu.reg.read16(ProgramCounter) == 0x0101
        (_, FrameReady) => Bool.False
    }
}

# Resume after hit: the first step executes the breakpoint instruction
# (self-loop re-hits it); after clearing, the frame completes.
expect {
    gb0 = GameBoy.init(rom_with([0x00, 0x18, 0xFE])).set_breakpoint(0x0101)
    match gb0.run_until(GameBoy.no_input({})) {
        (gb1, BreakpointHit) =>
            match gb1.run_until(GameBoy.no_input({})) {
                (gb2, BreakpointHit) =>
                    match gb2.clear_breakpoint().run_until(GameBoy.no_input({})) {
                        (gb3, FrameReady) => gb3.bus.read(0xFF44) == 144
                        (_, BreakpointHit) => Bool.False
                    }

                (_, FrameReady) => Bool.False
            }

        (_, FrameReady) => Bool.False
    }
}

# Double speed: after an armed STOP, the PPU advances half as fast —
# LD A,1; LDH (4D),A; STOP burn 20+2 dots, then 217 NOPs at 2 dots each
# land exactly on the 456-dot line boundary.
expect {
    base = List.repeat(0x00.U8, 0x8000)
    with_flag = base.set(0x0143, 0x80) ?? base
    prog = [0x3E, 0x01, 0xE0, 0x4D, 0x10, 0x00]
    rom = prog.fold({ i: 0x0100.U64, r: with_flag }, |acc, byte| { i: acc.i + 1, r: acc.r.set(acc.i, byte) ?? acc.r }).r
    var gb = GameBoy.init(rom)
    var n = 0
    while n < 220 {
        gb = after_step(gb)
        n = n + 1
    }
    gb.peek(0xFF4D) == 0xFE and gb.peek(0xFF44) == 1
}

# Battery: round trip through `.sav` bytes on an MBC1+Ram+Battery cart
battery_rom : U8 -> List(U8)
battery_rom = |ram_size_byte| {
    base = List.repeat(0x00.U8, 0x8000)
    typed = base.set(0x0147, 0x03) ?? base
    typed.set(0x0149, ram_size_byte) ?? typed
}

expect {
    rom = battery_rom(0x02) # 8 KiB declared
    gb0 = GameBoy.init(rom)
    gb1 = { ..gb0, bus: gb0.bus.write(0x0000, 0x0A).write(0xA000, 0x5A) }
    sav = gb1.battery()
    restored = GameBoy.init(rom).with_battery(sav)
    enabled = { ..restored, bus: restored.bus.write(0x0000, 0x0A) }
    sav.len() == 8192 and (sav.get(0) ?? 0x00) == 0x5A and enabled.peek(0xA000) == 0x5A
}

# Batteryless cart: nothing to extract, injection is a no-op
expect {
    plain = List.repeat(0x00.U8, 0x8000)
    rom = plain.set(0x0147, 0x01) ?? plain # MBC1, no RAM/battery
    gb = GameBoy.init(rom).with_battery(List.repeat(0x77, 8192))
    # injection was a no-op: the RAM buffer still holds its init value
    gb.battery().len() == 0 and { ..gb, bus: gb.bus.write(0x0000, 0x0A) }.peek(0xA000) == 0x00
}

# battery_fit reports how sav bytes match the cartridge's declared RAM:
# exact and footer-bearing sizes pass, everything else is Short/Long
expect {
    plain = List.repeat(0x00.U8, 0x8000)
    rom = plain.set(0x0147, 0x03) ?? plain # MBC1+RAM+BATTERY
    with_ram = rom.set(0x0149, 0x02) ?? rom # 8 KiB RAM
    gb = GameBoy.init(with_ram)
    batteryless = GameBoy.init(plain.set(0x0147, 0x01) ?? plain)
    gb.battery_fit(List.repeat(0, 8192)) == Exact
    and gb.battery_fit(List.repeat(0, 8192 + 48)) == Exact
    and gb.battery_fit(List.repeat(0, 8192 + 44)) == Exact
    and gb.battery_fit([]) == Empty
    and gb.battery_fit(List.repeat(0, 100)) == Short(8092)
    and gb.battery_fit(List.repeat(0, 9000)) == Long(808)
    and batteryless.battery_fit(List.repeat(0, 8192)) == NoBattery
}

# Size tolerance: short payloads zero-pad, long ones truncate
expect {
    rom = battery_rom(0x02)
    short = GameBoy.init(rom).with_battery([0x11, 0x22])
    long = GameBoy.init(rom).with_battery(List.repeat(0x77.U8, 0x30000))
    s = short.battery()
    l = long.battery()
    s.len() == 8192
    and (s.get(0) ?? 0x00) == 0x11
    and (s.get(2) ?? 0xFF) == 0x00
    and l.len() == 8192
    and (l.get(8191) ?? 0x00) == 0x77
}

# RTC cart round trip: RAM + 48-byte footer, byte-identical under a fixed now
expect {
    base = List.repeat(0x00.U8, 0x8000)
    typed = base.set(0x0147, 0x10) ?? base
    rom = typed.set(0x0149, 0x03) ?? typed # MBC3+Timer+Ram+Battery, 32 KiB
    gb0 = GameBoy.init(rom)
    gb1 = { ..gb0, bus: gb0.bus.set_input(GameBoy.no_input({})).write(0x0000, 0x0A).write(0xA000, 0x42) }
    sav = gb1.battery()
    again = GameBoy.init(rom).with_battery(sav).battery()
    sav.len() == 0x8000.U64.plus(48) and sav == again
}

# --- single-step harness ---

flat_state : List(U8) -> SingleStep
flat_state = |mem|
    { pc: 0x0100, sp: 0xFFFE, a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, h: 0, l: 0, ime: Bool.False, mem: mem }

# Injected state round-trips without stepping
expect {
    image = List.repeat(0xAA.U8, 0x10000)
    s = { pc: 0x1234.U16, sp: 0xBEEF.U16, a: 0x01.U8, b: 0x02.U8, c: 0x03.U8, d: 0x04.U8, e: 0x05.U8, f: 0xF0.U8, h: 0x07.U8, l: 0x08.U8, ime: Bool.True, mem: image }
    r = GameBoy.from_raw(s).raw()
    r.pc == 0x1234
    and r.sp == 0xBEEF
    and r.a == 0x01
    and r.b == 0x02
    and r.c == 0x03
    and r.d == 0x04
    and r.e == 0x05
    and r.f == 0xF0
    and r.h == 0x07
    and r.l == 0x08
    and r.ime
    and (r.mem.get(0xFFFF) ?? 0x00) == 0xAA
}

# Flat memory has no mapping: LD (BC), A lands a byte in "ROM"
expect {
    mem0 = List.repeat(0x00.U8, 0x10000)
    prog = mem0.set(0x0100, 0x02) ?? mem0
    s = { ..flat_state(prog), a: 0x5A, c: 0x05 }
    g = match GameBoy.from_raw(s).step_instruction() { (gb, _) => gb }
    (g.raw().mem.get(0x0005) ?? 0x00) == 0x5A
}

# One instruction exactly: pending interrupts don't dispatch, nothing ticks
expect {
    mem0 = List.repeat(0x00.U8, 0x10000)
    m1 = mem0.set(0xFFFF, 0x1F) ?? mem0 # IE all set (NOP at 0x0100 already)
    m2 = m1.set(0xFF0F, 0x1F) ?? m1 # IF all set
    s = { ..flat_state(m2), ime: Bool.True }
    match GameBoy.from_raw(s).step_instruction() {
        (g, cycles) => g.raw().pc == 0x0101 and cycles == 4 and g.cpu.ime
    }
}

# Trace records ordered accesses: PUSH BC = fetch, high write, low write
expect {
    mem0 = List.repeat(0x00.U8, 0x10000)
    prog = mem0.set(0x0100, 0xC5) ?? mem0
    s = { ..flat_state(prog), b: 0x12, c: 0x34 }
    g = match GameBoy.from_raw(s).step_instruction() { (gb, _) => gb }
    g.access_trace()
    == [
        { addr: 0x0100, val: 0xC5, dir: Read },
        { addr: 0xFFFD, val: 0x12, dir: Write },
        { addr: 0xFFFC, val: 0x34, dir: Write },
    ]
}

# POP traces low-then-high, the order hardware pops
expect {
    mem0 = List.repeat(0x00.U8, 0x10000)
    m1 = mem0.set(0x0100, 0xD1) ?? mem0 # POP DE
    m2 = m1.set(0xFFFC, 0x34) ?? m1
    m3 = m2.set(0xFFFD, 0x12) ?? m2
    s = { ..flat_state(m3), sp: 0xFFFC }
    g = match GameBoy.from_raw(s).step_instruction() { (gb, _) => gb }
    g.raw().d == 0x12
    and g.raw().e == 0x34
    and g.access_trace()
    == [
        { addr: 0x0100, val: 0xD1, dir: Read },
        { addr: 0xFFFC, val: 0x34, dir: Read },
        { addr: 0xFFFD, val: 0x12, dir: Read },
    ]
}

# Ordinary machines have no trace
expect GameBoy.init(rom_with([0x00])).access_trace() == []
