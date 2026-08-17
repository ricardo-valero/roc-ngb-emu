# SM83 execution core: fetch/decode/execute over the memory bus.
# `step` is pure state -> state; cycles are reported by execute because only
# it knows conditional outcomes (taken vs. not-taken).

import /Cartridge
import /Cartridge/Header
import /Cpu/Alu
import /Cpu/Instruction
import /Cpu/Register
import /Cpu/Register/Status
import /Apu
import /Bus
import /Ppu

# Raw machine state for the single-step harness: every register, IME, and
# a flat 64 KiB memory image with no address mapping. See check/single-step.
SingleStep : { pc : U16, sp : U16, a : U8, b : U8, c : U8, d : U8, e : U8, f : U8, h : U8, l : U8, ime : Bool, mem : List(U8) }

GameBoy := {
    reg : Register,
    bus : Bus,
    ppu : Ppu,
    apu : Apu,
    ime : Bool,
    halted : Bool,
    ei_pending : Bool,
    breakpoint : [None, At(U16)],
}.{
    init : List(U8) -> GameBoy
    init = |rom| {
        gb : GameBoy
        gb = {
            reg: Register.init({}),
            bus: Bus.init(rom),
            ppu: Ppu.init({}),
            apu: Apu.init({}),
            ime: Bool.False,
            halted: Bool.False,
            ei_pending: Bool.False,
            breakpoint: None,
        }
        # CGB carts boot with A = 0x11 — how games detect the console
        if gb.bus.is_cgb() {
            { ..gb, reg: gb.reg.write8(Accumulator, 0x11) }
        } else {
            gb
        }
    }

    serial : GameBoy -> List(U8)
    serial = |gb| gb.bus.serial()

    # Bus read for harnesses (e.g. Blargg's memory-reporting test protocol)
    peek : GameBoy, U16 -> U8
    peek = |gb, addr| gb.bus.read(addr)

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
    no_input = |_| { buttons: Bus.no_buttons({}), now: 0.U64 }

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
            if first == Bool.False and at_breakpoint(gb, gb.reg.read16(ProgramCounter)) {
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
            bus: Bus.flat(s.mem),
            ppu: Ppu.init({}),
            apu: Apu.init({}),
            ime: s.ime,
            halted: Bool.False,
            ei_pending: Bool.False,
            breakpoint: None,
        }
        gb
    }

    # Read raw state back out (inverse of from_raw for flat machines)
    raw : GameBoy -> SingleStep
    raw = |gb| {
        pc: gb.reg.read16(ProgramCounter),
        sp: gb.reg.read16(StackPointer),
        a: gb.reg.read8(Accumulator),
        b: gb.reg.read8(B),
        c: gb.reg.read8(C),
        d: gb.reg.read8(D),
        e: gb.reg.read8(E),
        f: gb.reg.read8(Status),
        h: gb.reg.read8(H),
        l: gb.reg.read8(L),
        ime: gb.ime,
        mem: gb.bus.mem,
    }

    # Execute exactly one instruction: no interrupt poll or dispatch, no
    # timer/PPU/APU ticks. What the vectors mean by "step".
    step_instruction : GameBoy -> (GameBoy, U64)
    step_instruction = |gb| run_bare(gb)

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
        pending = gb.bus.read(0xFFFF).bitwise_and(gb.bus.read(0xFF0F)).bitwise_and(0x1F)
        if gb.ime and pending != 0x00 {
            dispatch(gb, pending)
        } else if gb.halted {
            if pending != 0x00 {
                # Wake regardless of IME; without IME no dispatch happens
                run({ ..gb, halted: Bool.False })
            } else {
                finish(gb, 4)
            }
        } else {
            run(gb)
        }
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

    dispatch : GameBoy, U8 -> (GameBoy, U64)
    dispatch = |gb, pending| {
        bit =
            if pending.bitwise_and(0x01) != 0x00 {
                0
            } else if pending.bitwise_and(0x02) != 0x00 {
                1
            } else if pending.bitwise_and(0x04) != 0x00 {
                2
            } else if pending.bitwise_and(0x08) != 0x00 {
                3
            } else {
                4
            }
        gb1 = push16(gb, gb.reg.read16(ProgramCounter))
        bus2 = gb1.bus.poke(0xFF0F, gb1.bus.read(0xFF0F).bitwise_and(U8.shl_wrap(1, bit).bitwise_not()))
        reg2 = gb1.reg.write16(ProgramCounter, bit.to_u16().shl_wrap(3).plus(0x0040))
        finish({ ..gb1, reg: reg2, bus: bus2, ime: Bool.False, halted: Bool.False }, 20)
    }

    run : GameBoy -> (GameBoy, U64)
    run = |gb0| match run_bare(gb0) { (gb, cycles) => finish(gb, cycles) }

    # The instruction itself, peripherals excluded — `run` wraps this in
    # `finish`; `step_instruction` exposes it to the single-step harness
    run_bare : GameBoy -> (GameBoy, U64)
    run_bare = |gb0| {
        was_ei_pending = gb0.ei_pending
        pc = gb0.reg.read16(ProgramCounter)
        op = mem_read(gb0, pc)
        r = execute(op.gb, pc.plus_wrap(1), Instruction.lookup(op.value))
        gb1 = { ..r.gb, reg: r.gb.reg.write16(ProgramCounter, r.pc) }
        # EI takes effect after the instruction that follows it (DI cancels)
        gb2 =
            if was_ei_pending and gb1.ei_pending {
                { ..gb1, ime: Bool.True, ei_pending: Bool.False }
            } else {
                gb1
            }
        (gb2, r.cycles)
    }

    imm16 : GameBoy, U16 -> { gb : GameBoy, value : U16 }
    imm16 = |gb, pc| {
        lo = mem_read(gb, pc)
        hi = mem_read(lo.gb, pc.plus_wrap(1))
        { gb: hi.gb, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    sign_extend : U8 -> U16
    sign_extend = |e8|
        if e8 >= 0x80 { e8.to_u16().bitwise_or(0xFF00) } else { e8.to_u16() }

    mem_write : GameBoy, U16, U8 -> GameBoy
    mem_write = |gb, addr, value| { ..gb, bus: gb.bus.write(addr, value).trace_access(addr, value, Write) }

    # CPU-path read, threading the Bus so the access trace stays in
    # program order. With tracing off (every non-harness machine) this is
    # the plain pure read.
    mem_read : GameBoy, U16 -> { gb : GameBoy, value : U8 }
    mem_read = |gb, addr|
        match gb.bus.trace {
            NoTrace => { gb: gb, value: gb.bus.read(addr) }
            Trace(_) => {
                r = gb.bus.read_traced(addr)
                { gb: { ..gb, bus: r.bus }, value: r.value }
            }
        }

    push16 : GameBoy, U16 -> GameBoy
    push16 = |gb, value| {
        sp = gb.reg.read16(StackPointer)
        gb2 = mem_write(mem_write(gb, sp.minus_wrap(1), value.shr_zf_wrap(8).to_u8_wrap()), sp.minus_wrap(2), value.to_u8_wrap())
        { ..gb2, reg: gb2.reg.write16(StackPointer, sp.minus_wrap(2)) }
    }

    pop16 : GameBoy -> { gb : GameBoy, value : U16 }
    pop16 = |gb| {
        sp = gb.reg.read16(StackPointer)
        lo = mem_read(gb, sp) # hardware pops low first; the trace keeps the order honest
        hi = mem_read(lo.gb, sp.plus_wrap(1))
        { gb: { ..hi.gb, reg: hi.gb.reg.write16(StackPointer, sp.plus_wrap(2)) }, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    apply_flags = |reg, delta| reg.write8(Status, delta(reg.read8(Status)))

    carry_flag : GameBoy -> Bool
    carry_flag = |gb| Status.check(Carry, gb.reg.read8(Status))

    # Read an 8-bit source operand; returns extra cycles beyond the base cost
    src8 = |gb, pc, mode|
        match mode {
            Immediate => {
                rd = mem_read(gb, pc)
                { gb: rd.gb, value: rd.value, pc: pc.plus_wrap(1), cycles: 4.U64 }
            }

            Direct8(r) => { gb: gb, value: gb.reg.read8(r), pc: pc, cycles: 0.U64 }
            Direct16(_) => { gb: gb, value: 0xFF, pc: pc, cycles: 0.U64 } # not an 8-bit operand
            Indirect(target) =>
                match target {
                    C => {
                        rd = mem_read(gb, gb.reg.read8(C).to_u16().plus(0xFF00))
                        { gb: rd.gb, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    BC => {
                        rd = mem_read(gb, gb.reg.read16(BC))
                        { gb: rd.gb, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    DE => {
                        rd = mem_read(gb, gb.reg.read16(DE))
                        { gb: rd.gb, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HL => {
                        rd = mem_read(gb, gb.reg.read16(HL))
                        { gb: rd.gb, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HLPostIncrement => {
                        hl = gb.reg.read16(HL)
                        rd = mem_read(gb, hl)
                        { gb: { ..rd.gb, reg: rd.gb.reg.write16(HL, hl.plus_wrap(1)) }, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HLPostDecrement => {
                        hl = gb.reg.read16(HL)
                        rd = mem_read(gb, hl)
                        { gb: { ..rd.gb, reg: rd.gb.reg.write16(HL, hl.minus_wrap(1)) }, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    Word8Operand => {
                        n = mem_read(gb, pc)
                        rd = mem_read(n.gb, n.value.to_u16().plus(0xFF00))
                        { gb: rd.gb, value: rd.value, pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(gb, pc)
                        rd = mem_read(addr.gb, addr.value)
                        { gb: rd.gb, value: rd.value, pc: pc.plus_wrap(2), cycles: 12.U64 }
                    }
                }
        }

    # Write an 8-bit destination operand
    dst8 = |gb, pc, mode, value|
        match mode {
            Direct8(r) => { gb: { ..gb, reg: gb.reg.write8(r, value) }, pc: pc, cycles: 0.U64 }
            Indirect(target) =>
                match target {
                    C => { gb: mem_write(gb, gb.reg.read8(C).to_u16().plus(0xFF00), value), pc: pc, cycles: 4.U64 }
                    BC => { gb: mem_write(gb, gb.reg.read16(BC), value), pc: pc, cycles: 4.U64 }
                    DE => { gb: mem_write(gb, gb.reg.read16(DE), value), pc: pc, cycles: 4.U64 }
                    HL => { gb: mem_write(gb, gb.reg.read16(HL), value), pc: pc, cycles: 4.U64 }
                    HLPostIncrement => {
                        hl = gb.reg.read16(HL)
                        gb2 = mem_write(gb, hl, value)
                        { gb: { ..gb2, reg: gb2.reg.write16(HL, hl.plus_wrap(1)) }, pc: pc, cycles: 4.U64 }
                    }

                    HLPostDecrement => {
                        hl = gb.reg.read16(HL)
                        gb2 = mem_write(gb, hl, value)
                        { gb: { ..gb2, reg: gb2.reg.write16(HL, hl.minus_wrap(1)) }, pc: pc, cycles: 4.U64 }
                    }

                    Word8Operand => {
                        n = mem_read(gb, pc)
                        { gb: mem_write(n.gb, n.value.to_u16().plus(0xFF00), value), pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(gb, pc)
                        { gb: mem_write(addr.gb, addr.value, value), pc: pc.plus_wrap(2), cycles: 12.U64 }
                    }
                }

            _ => { gb: gb, pc: pc, cycles: 0.U64 } # not an 8-bit destination
        }

    read16m = |gb, mode|
        match mode {
            Direct16(rr) => gb.reg.read16(rr)
            _ => 0x0000
        }

    write16m = |gb, mode, value|
        match mode {
            Direct16(rr) => { ..gb, reg: gb.reg.write16(rr, value) }
            _ => gb
        }

    # Accumulator + flags writeback shared by the 8-bit ALU instructions
    acc_result = |gb, pc, cycles, (value, delta)| {
        reg2 = apply_flags(gb.reg.write8(Accumulator, value), delta)
        { gb: { ..gb, reg: reg2 }, pc: pc, cycles: cycles }
    }

    # RLCA/RRCA/RLA/RRA force Z clear, unlike their CB twins
    rotate_a = |gb, pc, (value, delta)| {
        reg2 = gb.reg.write8(Accumulator, value)
        { gb: { ..gb, reg: reg2.write8(Status, delta(reg2.read8(Status)).bitwise_and(0x7F)) }, pc: pc, cycles: 4.U64 }
    }

    flags_only = |gb, pc, delta|
        { gb: { ..gb, reg: apply_flags(gb.reg, delta) }, pc: pc, cycles: 4.U64 }

    alu8 = |gb, pc, mode, op| {
        s = src8(gb, pc, mode)
        acc_result(s.gb, s.pc, s.cycles.plus(4), op(s.gb.reg.read8(Accumulator), s.value))
    }

    # Read-modify-write (INC/DEC and the CB rotate/shift/set/res family)
    rmw = |gb0, pc, dst, src, base, op| {
        s = src8(gb0, pc, src)
        match op(s.value) {
            (value, delta) => {
                gb1 = { ..s.gb, reg: apply_flags(s.gb.reg, delta) }
                w = dst8(gb1, s.pc, dst, value)
                { gb: w.gb, pc: w.pc, cycles: base.plus(s.cycles).plus(w.cycles) }
            }
        }
    }

    execute = |gb, pc, instr|
        match instr {
            Nop => { gb: gb, pc: pc, cycles: 4.U64 }
            Stop => { gb: { ..gb, bus: gb.bus.stop_switch() }, pc: pc.plus_wrap(1), cycles: 4.U64 } # speed switch when armed; else a NOP that skips its padding
            Halt => { gb: { ..gb, halted: Bool.True }, pc: pc, cycles: 4.U64 }
            Illegal => { gb: gb, pc: pc, cycles: 4.U64 } # real hardware locks up
            Unknown => { gb: gb, pc: pc, cycles: 4.U64 }
            Prefix => {
                cb = mem_read(gb, pc)
                execute_cb(cb.gb, pc.plus_wrap(1), cb.value)
            }
            Interrupts(Enable) => { gb: { ..gb, ei_pending: Bool.True }, pc: pc, cycles: 4.U64 }
            Interrupts(Disable) => { gb: { ..gb, ime: Bool.False, ei_pending: Bool.False }, pc: pc, cycles: 4.U64 }
            CarryFlag(Set) => flags_only(gb, pc, Status.modify(Unchanged, Value(Bool.False), Value(Bool.False), Value(Bool.True)))
            CarryFlag(Complement) => flags_only(gb, pc, Status.modify(Unchanged, Value(Bool.False), Value(Bool.False), Complement))
            DecimalAdjustAccumulator => {
                f = gb.reg.read8(Status)
                acc_result(gb, pc, 4, Alu.daa(gb.reg.read8(Accumulator), Status.check(Subtract, f), Status.check(HalfCarry, f), Status.check(Carry, f)))
            }

            ComplementAccumulator => acc_result(gb, pc, 4, Alu.complement(gb.reg.read8(Accumulator)))
            RotateCircularAccumulator(Left) => rotate_a(gb, pc, Alu.rlc(gb.reg.read8(Accumulator)))
            RotateCircularAccumulator(Right) => rotate_a(gb, pc, Alu.rrc(gb.reg.read8(Accumulator)))
            RotateAccumulator(Left) => rotate_a(gb, pc, Alu.rl(gb.reg.read8(Accumulator), carry_flag(gb)))
            RotateAccumulator(Right) => rotate_a(gb, pc, Alu.rr(gb.reg.read8(Accumulator), carry_flag(gb)))
            Add(mode) => alu8(gb, pc, mode, Alu.add)
            Sub(mode) => alu8(gb, pc, mode, Alu.sub)
            And(mode) => alu8(gb, pc, mode, Alu.and_a)
            Or(mode) => alu8(gb, pc, mode, Alu.or_a)
            Xor(mode) => alu8(gb, pc, mode, Alu.xor_a)
            Adc(mode) => alu8(gb, pc, mode, |a, b| Alu.adc(a, b, carry_flag(gb)))
            Sbc(mode) => alu8(gb, pc, mode, |a, b| Alu.sbc(a, b, carry_flag(gb)))
            Compare(mode) => {
                s = src8(gb, pc, mode)
                match Alu.compare(s.gb.reg.read8(Accumulator), s.value) {
                    (_, delta) => { gb: { ..s.gb, reg: apply_flags(s.gb.reg, delta) }, pc: s.pc, cycles: s.cycles.plus(4) }
                }
            }

            Inc(dst, src) => rmw(gb, pc, dst, src, 4.U64, Alu.inc)
            Dec(dst, src) => rmw(gb, pc, dst, src, 4.U64, Alu.dec)
            Add16(mode) =>
                match Alu.add16(gb.reg.read16(HL), read16m(gb, mode)) {
                    (value, delta) => { gb: { ..gb, reg: apply_flags(gb.reg.write16(HL, value), delta) }, pc: pc, cycles: 8.U64 }
                }

            Inc16(dst, _) => { gb: write16m(gb, dst, read16m(gb, dst).plus_wrap(1)), pc: pc, cycles: 8.U64 }
            Dec16(dst, _) => { gb: write16m(gb, dst, read16m(gb, dst).minus_wrap(1)), pc: pc, cycles: 8.U64 }
            AddStackPointerImmediate => {
                e8 = mem_read(gb, pc)
                match Alu.add_sp(e8.gb.reg.read16(StackPointer), e8.value) {
                    (value, delta) => { gb: { ..e8.gb, reg: apply_flags(e8.gb.reg.write16(StackPointer, value), delta) }, pc: pc.plus_wrap(1), cycles: 16.U64 }
                }
            }

            LoadHLStackPointerImmediate => {
                e8 = mem_read(gb, pc)
                match Alu.add_sp(e8.gb.reg.read16(StackPointer), e8.value) {
                    (value, delta) => { gb: { ..e8.gb, reg: apply_flags(e8.gb.reg.write16(HL, value), delta) }, pc: pc.plus_wrap(1), cycles: 12.U64 }
                }
            }

            Load(dst, src) => {
                s = src8(gb, pc, src)
                w = dst8(s.gb, s.pc, dst, s.value)
                { gb: w.gb, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(4) }
            }

            Load16(Direct16(StackPointer), Direct16(HL)) =>
                { gb: { ..gb, reg: gb.reg.write16(StackPointer, gb.reg.read16(HL)) }, pc: pc, cycles: 8.U64 }
            Load16(Indirect(Word16Operand), Direct16(StackPointer)) => {
                addr = imm16(gb, pc)
                sp = addr.gb.reg.read16(StackPointer)
                gb2 = mem_write(mem_write(addr.gb, addr.value, sp.to_u8_wrap()), addr.value.plus_wrap(1), sp.shr_zf_wrap(8).to_u8_wrap())
                { gb: gb2, pc: pc.plus_wrap(2), cycles: 20.U64 }
            }

            Load16(dst, Immediate) => {
                nn = imm16(gb, pc)
                { gb: write16m(nn.gb, dst, nn.value), pc: pc.plus_wrap(2), cycles: 12.U64 }
            }
            Load16(_, _) => { gb: gb, pc: pc, cycles: 4.U64 } # no other encodings exist
            Push(mode) => { gb: push16(gb, read16m(gb, mode)), pc: pc, cycles: 16.U64 }
            Pop(mode) => {
                p = pop16(gb)
                { gb: write16m(p.gb, mode, p.value), pc: pc, cycles: 12.U64 }
            }

            Jump(_, HL) => { gb: gb, pc: gb.reg.read16(HL), cycles: 4.U64 }
            Jump(condition, Immediate) => {
                target = imm16(gb, pc)
                if Instruction.condition_met(condition, target.gb.reg.read8(Status)) {
                    { gb: target.gb, pc: target.value, cycles: 16.U64 }
                } else {
                    { gb: target.gb, pc: pc.plus_wrap(2), cycles: 12.U64 }
                }
            }

            Branch(condition) => {
                e8 = mem_read(gb, pc)
                offset = sign_extend(e8.value)
                after = pc.plus_wrap(1)
                if Instruction.condition_met(condition, e8.gb.reg.read8(Status)) {
                    { gb: e8.gb, pc: after.plus_wrap(offset), cycles: 12.U64 }
                } else {
                    { gb: e8.gb, pc: after, cycles: 8.U64 }
                }
            }

            Call(condition) => {
                target = imm16(gb, pc)
                after = pc.plus_wrap(2)
                if Instruction.condition_met(condition, target.gb.reg.read8(Status)) {
                    { gb: push16(target.gb, after), pc: target.value, cycles: 24.U64 }
                } else {
                    { gb: target.gb, pc: after, cycles: 12.U64 }
                }
            }

            Return(Always) => {
                p = pop16(gb)
                { gb: p.gb, pc: p.value, cycles: 16.U64 }
            }

            Return(condition) =>
                if Instruction.condition_met(condition, gb.reg.read8(Status)) {
                    p = pop16(gb)
                    { gb: p.gb, pc: p.value, cycles: 20.U64 }
                } else {
                    { gb: gb, pc: pc, cycles: 8.U64 }
                }

            ReturnAndEnableInterrupts => {
                p = pop16(gb)
                { gb: { ..p.gb, ime: Bool.True }, pc: p.value, cycles: 16.U64 }
            }

            Restart(addr) => { gb: push16(gb, pc), pc: addr.to_u16(), cycles: 16.U64 }
        }

    execute_cb = |gb, pc, byte|
        match Instruction.lookup_prefixed(byte) {
            Bit(index, mode) => {
                s = src8(gb, pc, mode)
                { gb: { ..s.gb, reg: apply_flags(s.gb.reg, Alu.bit_test(index, s.value)) }, pc: s.pc, cycles: s.cycles.plus(8) }
            }

            Set(index, mode) => {
                s = src8(gb, pc, mode)
                w = dst8(s.gb, s.pc, mode, Alu.set_bit(index, s.value))
                { gb: w.gb, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(8) }
            }

            Reset(index, mode) => {
                s = src8(gb, pc, mode)
                w = dst8(s.gb, s.pc, mode, Alu.clear_bit(index, s.value))
                { gb: w.gb, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(8) }
            }

            Rotate(Left, mode) => rmw(gb, pc, mode, mode, 8.U64, |v| Alu.rl(v, carry_flag(gb)))
            Rotate(Right, mode) => rmw(gb, pc, mode, mode, 8.U64, |v| Alu.rr(v, carry_flag(gb)))
            RotateCircular(Left, mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.rlc)
            RotateCircular(Right, mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.rrc)
            ShiftArithmetic(Left, mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.sla)
            ShiftArithmetic(Right, mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.sra)
            ShiftLogical(Right, mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.srl)
            Swap(mode) => rmw(gb, pc, mode, mode, 8.U64, Alu.swap)
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
    GameBoy.init(cgb).reg.read8(Accumulator) == 0x11
    and GameBoy.init(plain).reg.read8(Accumulator) == 0x01
}

# LD A, 0x2A: A loaded, PC advanced by 2, 8 cycles
expect {
    g = after_step(GameBoy.init(rom_with([0x3E, 0x2A])))
    g.reg.read8(Accumulator) == 0x2A and g.reg.read16(ProgramCounter) == 0x0102
}
expect cycles_of(GameBoy.init(rom_with([0x3E, 0x2A]))) == 8

# Conditional cycle split: post-boot F has Z set, so NZ misses and Z hits
expect cycles_of(GameBoy.init(rom_with([0x20, 0x05]))) == 8
expect {
    match GameBoy.init(rom_with([0x28, 0x05])).step() {
        (g, c) => c == 12 and g.reg.read16(ProgramCounter) == 0x0107
    }
}

# ALU writeback: XOR A clears A and sets only Z
expect {
    g = after_step(GameBoy.init(rom_with([0xAF])))
    g.reg.read8(Accumulator) == 0x00 and g.reg.read8(Status) == 0b1000_0000
}

# CB path: SET 7, A then BIT 7, A
expect {
    g = after_step(after_step(GameBoy.init(rom_with([0xCB, 0xFF, 0xCB, 0x7F]))))
    g.reg.read8(Accumulator).bitwise_and(0x80) == 0x80 and Status.check(Zero, g.reg.read8(Status)) == Bool.False
}

# Interrupt dispatch: PC pushed, vector 0x40 taken, IME and IF bit cleared, 20 cycles
expect {
    gb0 = GameBoy.init(rom_with([0x00]))
    gb1 = { ..gb0, ime: Bool.True, bus: gb0.bus.write(0xFFFF, 0x01).poke(0xFF0F, 0x01) }
    match gb1.step() {
        (g, c) =>
            c == 20
            and g.reg.read16(ProgramCounter) == 0x0040
            and g.ime == Bool.False
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
    g2.reg.read16(ProgramCounter) == 0x0102 and g3.reg.read16(ProgramCounter) == 0x0040
}

# HALT wakes without IME: resumes after HALT, no vector taken
expect {
    gb0 = GameBoy.init(rom_with([0x76, 0x3E, 0x07])) # HALT; LD A, 0x07
    # post-boot IF is 0xE1, so clear it or the HALT wakes immediately
    gb1 = { ..gb0, bus: gb0.bus.write(0xFFFF, 0x01).poke(0xFF0F, 0x00) }
    g1 = after_step(gb1) # halts
    g2 = after_step(g1) # nothing pending: stays halted
    g3 = after_step({ ..g2, bus: g2.bus.poke(0xFF0F, 0x01) }) # wakes, runs the LD
    g1.halted == Bool.True
    and g2.halted == Bool.True
    and g3.reg.read8(Accumulator) == 0x07
    and g3.reg.read16(ProgramCounter) == 0x0103
    and g3.ime == Bool.False
}

# Stack round-trip: PUSH BC / POP DE
expect {
    g = after_step(after_step(GameBoy.init(rom_with([0xC5, 0xD1]))))
    g.reg.read16(DE) == g.reg.read16(BC) and g.reg.read16(StackPointer) == 0xFFFE
}

# CALL pushes the return address; RET comes back
expect {
    # CALL 0x0110; NOP... at 0x0110: RET
    code = [0xCD, 0x10, 0x01].concat(List.repeat(0x00, 13)).concat([0xC9])
    g1 = after_step(GameBoy.init(rom_with(code)))
    g2 = after_step(g1)
    g1.reg.read16(ProgramCounter) == 0x0110 and g2.reg.read16(ProgramCounter) == 0x0103
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
        (gb, BreakpointHit) => gb.reg.read16(ProgramCounter) == 0x0101
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
        (g, cycles) => g.raw().pc == 0x0101 and cycles == 4 and g.ime
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
