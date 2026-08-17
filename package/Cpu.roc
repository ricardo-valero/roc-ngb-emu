# SM83 execution core: fetch/decode/execute over the memory bus. `step`
# makes the whole CPU-side decision — the interrupt poll and dispatch,
# HALT wake, and the EI-delay latch — and reports the machine cycles
# consumed; the peripheral fan-out on those cycles stays in GameBoy.
# These registers are the only ones on the console that are not
# memory-mapped, so the CPU owns them directly.

import /Bus
import /Cpu/Alu
import /Cpu/Instruction
import /Cpu/Register
import /Cpu/Register/Status

# The state threaded through one instruction: the CPU's fields flat next
# to the bus, so the execution code reads and updates them exactly as it
# did on the GameBoy record. `step` packs and unpacks at the boundary.
St : { reg : Register, ime : Bool, halted : Bool, ei_pending : Bool, bus : Bus }

Cpu := { reg : Register, ime : Bool, halted : Bool, ei_pending : Bool }.{
    # DMG post-boot state (no boot ROM)
    init : {} -> Cpu
    init = |_| { reg: Register.init({}), ime: Bool.False, halted: Bool.False, ei_pending: Bool.False }

    to_st : Cpu, Bus -> St
    to_st = |cpu, bus| { reg: cpu.reg, ime: cpu.ime, halted: cpu.halted, ei_pending: cpu.ei_pending, bus: bus }

    of_st : St, U64 -> { cpu : Cpu, bus : Bus, cycles : U64 }
    of_st = |st, cycles|
        { cpu: { reg: st.reg, ime: st.ime, halted: st.halted, ei_pending: st.ei_pending }, bus: st.bus, cycles: cycles }

    step : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }
    step = |cpu, bus| {
        pending = bus.read(0xFFFF).bitwise_and(bus.read(0xFF0F)).bitwise_and(0x1F)
        if cpu.ime and pending != 0x00 {
            match dispatch(to_st(cpu, bus), pending) { (st, cycles) => of_st(st, cycles) }
        } else if cpu.halted {
            if pending != 0x00 {
                # Wake regardless of IME; without IME no dispatch happens
                match run_bare(to_st({ ..cpu, halted: Bool.False }, bus)) { (st, cycles) => of_st(st, cycles) }
            } else {
                { cpu: cpu, bus: bus, cycles: 4 }
            }
        } else {
            match run_bare(to_st(cpu, bus)) { (st, cycles) => of_st(st, cycles) }
        }
    }

    # Exactly one instruction: no interrupt poll or dispatch — what the
    # single-step vectors mean by "step"
    step_instruction : Cpu, Bus -> { cpu : Cpu, bus : Bus, cycles : U64 }
    step_instruction = |cpu, bus|
        match run_bare(to_st(cpu, bus)) { (st, cycles) => of_st(st, cycles) }

    dispatch : St, U8 -> (St, U64)
    dispatch = |st, pending| {
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
        st1 = push16(st, st.reg.read16(ProgramCounter))
        bus2 = st1.bus.poke(0xFF0F, st1.bus.read(0xFF0F).bitwise_and(U8.shl_wrap(1, bit).bitwise_not()))
        reg2 = st1.reg.write16(ProgramCounter, bit.to_u16().shl_wrap(3).plus(0x0040))
        ({ ..st1, reg: reg2, bus: bus2, ime: Bool.False, halted: Bool.False }, 20)
    }

    # The instruction itself, peripherals excluded — `step` wraps this in
    # the interrupt poll; `step_instruction` exposes it to the single-step harness
    run_bare : St -> (St, U64)
    run_bare = |st0| {
        was_ei_pending = st0.ei_pending
        pc = st0.reg.read16(ProgramCounter)
        op = mem_read(st0, pc)
        r = execute(op.st, pc.plus_wrap(1), Instruction.lookup(op.value))
        st1 = { ..r.st, reg: r.st.reg.write16(ProgramCounter, r.pc) }
        # EI takes effect after the instruction that follows it (DI cancels)
        st2 =
            if was_ei_pending and st1.ei_pending {
                { ..st1, ime: Bool.True, ei_pending: Bool.False }
            } else {
                st1
            }
        (st2, r.cycles)
    }

    imm16 : St, U16 -> { st : St, value : U16 }
    imm16 = |st, pc| {
        lo = mem_read(st, pc)
        hi = mem_read(lo.st, pc.plus_wrap(1))
        { st: hi.st, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    sign_extend : U8 -> U16
    sign_extend = |e8|
        if e8 >= 0x80 { e8.to_u16().bitwise_or(0xFF00) } else { e8.to_u16() }

    mem_write : St, U16, U8 -> St
    mem_write = |st, addr, value| { ..st, bus: st.bus.write(addr, value).trace_access(addr, value, Write) }

    # CPU-path read, threading the Bus so the access trace stays in
    # program order. With tracing off (every non-harness machine) this is
    # the plain pure read.
    mem_read : St, U16 -> { st : St, value : U8 }
    mem_read = |st, addr|
        match st.bus.trace {
            NoTrace => { st: st, value: st.bus.read(addr) }
            Trace(_) => {
                r = st.bus.read_traced(addr)
                { st: { ..st, bus: r.bus }, value: r.value }
            }
        }

    push16 : St, U16 -> St
    push16 = |st, value| {
        sp = st.reg.read16(StackPointer)
        st2 = mem_write(mem_write(st, sp.minus_wrap(1), value.shr_zf_wrap(8).to_u8_wrap()), sp.minus_wrap(2), value.to_u8_wrap())
        { ..st2, reg: st2.reg.write16(StackPointer, sp.minus_wrap(2)) }
    }

    pop16 : St -> { st : St, value : U16 }
    pop16 = |st| {
        sp = st.reg.read16(StackPointer)
        lo = mem_read(st, sp) # hardware pops low first; the trace keeps the order honest
        hi = mem_read(lo.st, sp.plus_wrap(1))
        { st: { ..hi.st, reg: hi.st.reg.write16(StackPointer, sp.plus_wrap(2)) }, value: hi.value.to_u16().shl_wrap(8).bitwise_or(lo.value.to_u16()) }
    }

    apply_flags = |reg, delta| reg.write8(Status, delta(reg.read8(Status)))

    carry_flag : St -> Bool
    carry_flag = |st| Status.check(Carry, st.reg.read8(Status))

    # Read an 8-bit source operand; returns extra cycles beyond the base cost
    src8 = |st, pc, mode|
        match mode {
            Immediate => {
                rd = mem_read(st, pc)
                { st: rd.st, value: rd.value, pc: pc.plus_wrap(1), cycles: 4.U64 }
            }

            Direct8(r) => { st: st, value: st.reg.read8(r), pc: pc, cycles: 0.U64 }
            Direct16(_) => { st: st, value: 0xFF, pc: pc, cycles: 0.U64 } # not an 8-bit operand
            Indirect(target) =>
                match target {
                    C => {
                        rd = mem_read(st, st.reg.read8(C).to_u16().plus(0xFF00))
                        { st: rd.st, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    BC => {
                        rd = mem_read(st, st.reg.read16(BC))
                        { st: rd.st, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    DE => {
                        rd = mem_read(st, st.reg.read16(DE))
                        { st: rd.st, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HL => {
                        rd = mem_read(st, st.reg.read16(HL))
                        { st: rd.st, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HLPostIncrement => {
                        hl = st.reg.read16(HL)
                        rd = mem_read(st, hl)
                        { st: { ..rd.st, reg: rd.st.reg.write16(HL, hl.plus_wrap(1)) }, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    HLPostDecrement => {
                        hl = st.reg.read16(HL)
                        rd = mem_read(st, hl)
                        { st: { ..rd.st, reg: rd.st.reg.write16(HL, hl.minus_wrap(1)) }, value: rd.value, pc: pc, cycles: 4.U64 }
                    }

                    Word8Operand => {
                        n = mem_read(st, pc)
                        rd = mem_read(n.st, n.value.to_u16().plus(0xFF00))
                        { st: rd.st, value: rd.value, pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(st, pc)
                        rd = mem_read(addr.st, addr.value)
                        { st: rd.st, value: rd.value, pc: pc.plus_wrap(2), cycles: 12.U64 }
                    }
                }
        }

    # Write an 8-bit destination operand
    dst8 = |st, pc, mode, value|
        match mode {
            Direct8(r) => { st: { ..st, reg: st.reg.write8(r, value) }, pc: pc, cycles: 0.U64 }
            Indirect(target) =>
                match target {
                    C => { st: mem_write(st, st.reg.read8(C).to_u16().plus(0xFF00), value), pc: pc, cycles: 4.U64 }
                    BC => { st: mem_write(st, st.reg.read16(BC), value), pc: pc, cycles: 4.U64 }
                    DE => { st: mem_write(st, st.reg.read16(DE), value), pc: pc, cycles: 4.U64 }
                    HL => { st: mem_write(st, st.reg.read16(HL), value), pc: pc, cycles: 4.U64 }
                    HLPostIncrement => {
                        hl = st.reg.read16(HL)
                        st2 = mem_write(st, hl, value)
                        { st: { ..st2, reg: st2.reg.write16(HL, hl.plus_wrap(1)) }, pc: pc, cycles: 4.U64 }
                    }

                    HLPostDecrement => {
                        hl = st.reg.read16(HL)
                        st2 = mem_write(st, hl, value)
                        { st: { ..st2, reg: st2.reg.write16(HL, hl.minus_wrap(1)) }, pc: pc, cycles: 4.U64 }
                    }

                    Word8Operand => {
                        n = mem_read(st, pc)
                        { st: mem_write(n.st, n.value.to_u16().plus(0xFF00), value), pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(st, pc)
                        { st: mem_write(addr.st, addr.value, value), pc: pc.plus_wrap(2), cycles: 12.U64 }
                    }
                }

            _ => { st: st, pc: pc, cycles: 0.U64 } # not an 8-bit destination
        }

    read16m = |st, mode|
        match mode {
            Direct16(rr) => st.reg.read16(rr)
            _ => 0x0000
        }

    write16m = |st, mode, value|
        match mode {
            Direct16(rr) => { ..st, reg: st.reg.write16(rr, value) }
            _ => st
        }

    # Accumulator + flags writeback shared by the 8-bit ALU instructions
    acc_result = |st, pc, cycles, (value, delta)| {
        reg2 = apply_flags(st.reg.write8(Accumulator, value), delta)
        { st: { ..st, reg: reg2 }, pc: pc, cycles: cycles }
    }

    # RLCA/RRCA/RLA/RRA force Z clear, unlike their CB twins
    rotate_a = |st, pc, (value, delta)| {
        reg2 = st.reg.write8(Accumulator, value)
        { st: { ..st, reg: reg2.write8(Status, delta(reg2.read8(Status)).bitwise_and(0x7F)) }, pc: pc, cycles: 4.U64 }
    }

    flags_only = |st, pc, delta|
        { st: { ..st, reg: apply_flags(st.reg, delta) }, pc: pc, cycles: 4.U64 }

    alu8 = |st, pc, mode, op| {
        s = src8(st, pc, mode)
        acc_result(s.st, s.pc, s.cycles.plus(4), op(s.st.reg.read8(Accumulator), s.value))
    }

    # Read-modify-write (INC/DEC and the CB rotate/shift/set/res family)
    rmw = |st0, pc, dst, src, base, op| {
        s = src8(st0, pc, src)
        match op(s.value) {
            (value, delta) => {
                st1 = { ..s.st, reg: apply_flags(s.st.reg, delta) }
                w = dst8(st1, s.pc, dst, value)
                { st: w.st, pc: w.pc, cycles: base.plus(s.cycles).plus(w.cycles) }
            }
        }
    }

    execute = |st, pc, instr|
        match instr {
            Nop => { st: st, pc: pc, cycles: 4.U64 }
            Stop => { st: { ..st, bus: st.bus.stop_switch() }, pc: pc.plus_wrap(1), cycles: 4.U64 } # speed switch when armed; else a NOP that skips its padding
            Halt => { st: { ..st, halted: Bool.True }, pc: pc, cycles: 4.U64 }
            Illegal => { st: st, pc: pc, cycles: 4.U64 } # real hardware locks up
            Unknown => { st: st, pc: pc, cycles: 4.U64 }
            Prefix => {
                cb = mem_read(st, pc)
                execute_cb(cb.st, pc.plus_wrap(1), cb.value)
            }
            Interrupts(Enable) => { st: { ..st, ei_pending: Bool.True }, pc: pc, cycles: 4.U64 }
            Interrupts(Disable) => { st: { ..st, ime: Bool.False, ei_pending: Bool.False }, pc: pc, cycles: 4.U64 }
            CarryFlag(Set) => flags_only(st, pc, Status.modify(Unchanged, Value(Bool.False), Value(Bool.False), Value(Bool.True)))
            CarryFlag(Complement) => flags_only(st, pc, Status.modify(Unchanged, Value(Bool.False), Value(Bool.False), Complement))
            DecimalAdjustAccumulator => {
                f = st.reg.read8(Status)
                acc_result(st, pc, 4, Alu.daa(st.reg.read8(Accumulator), Status.check(Subtract, f), Status.check(HalfCarry, f), Status.check(Carry, f)))
            }

            ComplementAccumulator => acc_result(st, pc, 4, Alu.complement(st.reg.read8(Accumulator)))
            RotateCircularAccumulator(Left) => rotate_a(st, pc, Alu.rlc(st.reg.read8(Accumulator)))
            RotateCircularAccumulator(Right) => rotate_a(st, pc, Alu.rrc(st.reg.read8(Accumulator)))
            RotateAccumulator(Left) => rotate_a(st, pc, Alu.rl(st.reg.read8(Accumulator), carry_flag(st)))
            RotateAccumulator(Right) => rotate_a(st, pc, Alu.rr(st.reg.read8(Accumulator), carry_flag(st)))
            Add(mode) => alu8(st, pc, mode, Alu.add)
            Sub(mode) => alu8(st, pc, mode, Alu.sub)
            And(mode) => alu8(st, pc, mode, Alu.and_a)
            Or(mode) => alu8(st, pc, mode, Alu.or_a)
            Xor(mode) => alu8(st, pc, mode, Alu.xor_a)
            Adc(mode) => alu8(st, pc, mode, |a, b| Alu.adc(a, b, carry_flag(st)))
            Sbc(mode) => alu8(st, pc, mode, |a, b| Alu.sbc(a, b, carry_flag(st)))
            Compare(mode) => {
                s = src8(st, pc, mode)
                match Alu.compare(s.st.reg.read8(Accumulator), s.value) {
                    (_, delta) => { st: { ..s.st, reg: apply_flags(s.st.reg, delta) }, pc: s.pc, cycles: s.cycles.plus(4) }
                }
            }

            Inc(dst, src) => rmw(st, pc, dst, src, 4.U64, Alu.inc)
            Dec(dst, src) => rmw(st, pc, dst, src, 4.U64, Alu.dec)
            Add16(mode) =>
                match Alu.add16(st.reg.read16(HL), read16m(st, mode)) {
                    (value, delta) => { st: { ..st, reg: apply_flags(st.reg.write16(HL, value), delta) }, pc: pc, cycles: 8.U64 }
                }

            Inc16(dst, _) => { st: write16m(st, dst, read16m(st, dst).plus_wrap(1)), pc: pc, cycles: 8.U64 }
            Dec16(dst, _) => { st: write16m(st, dst, read16m(st, dst).minus_wrap(1)), pc: pc, cycles: 8.U64 }
            AddStackPointerImmediate => {
                e8 = mem_read(st, pc)
                match Alu.add_sp(e8.st.reg.read16(StackPointer), e8.value) {
                    (value, delta) => { st: { ..e8.st, reg: apply_flags(e8.st.reg.write16(StackPointer, value), delta) }, pc: pc.plus_wrap(1), cycles: 16.U64 }
                }
            }

            LoadHLStackPointerImmediate => {
                e8 = mem_read(st, pc)
                match Alu.add_sp(e8.st.reg.read16(StackPointer), e8.value) {
                    (value, delta) => { st: { ..e8.st, reg: apply_flags(e8.st.reg.write16(HL, value), delta) }, pc: pc.plus_wrap(1), cycles: 12.U64 }
                }
            }

            Load(dst, src) => {
                s = src8(st, pc, src)
                w = dst8(s.st, s.pc, dst, s.value)
                { st: w.st, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(4) }
            }

            Load16(Direct16(StackPointer), Direct16(HL)) =>
                { st: { ..st, reg: st.reg.write16(StackPointer, st.reg.read16(HL)) }, pc: pc, cycles: 8.U64 }
            Load16(Indirect(Word16Operand), Direct16(StackPointer)) => {
                addr = imm16(st, pc)
                sp = addr.st.reg.read16(StackPointer)
                st2 = mem_write(mem_write(addr.st, addr.value, sp.to_u8_wrap()), addr.value.plus_wrap(1), sp.shr_zf_wrap(8).to_u8_wrap())
                { st: st2, pc: pc.plus_wrap(2), cycles: 20.U64 }
            }

            Load16(dst, Immediate) => {
                nn = imm16(st, pc)
                { st: write16m(nn.st, dst, nn.value), pc: pc.plus_wrap(2), cycles: 12.U64 }
            }
            Load16(_, _) => { st: st, pc: pc, cycles: 4.U64 } # no other encodings exist
            Push(mode) => { st: push16(st, read16m(st, mode)), pc: pc, cycles: 16.U64 }
            Pop(mode) => {
                p = pop16(st)
                { st: write16m(p.st, mode, p.value), pc: pc, cycles: 12.U64 }
            }

            Jump(_, HL) => { st: st, pc: st.reg.read16(HL), cycles: 4.U64 }
            Jump(condition, Immediate) => {
                target = imm16(st, pc)
                if Instruction.condition_met(condition, target.st.reg.read8(Status)) {
                    { st: target.st, pc: target.value, cycles: 16.U64 }
                } else {
                    { st: target.st, pc: pc.plus_wrap(2), cycles: 12.U64 }
                }
            }

            Branch(condition) => {
                e8 = mem_read(st, pc)
                offset = sign_extend(e8.value)
                after = pc.plus_wrap(1)
                if Instruction.condition_met(condition, e8.st.reg.read8(Status)) {
                    { st: e8.st, pc: after.plus_wrap(offset), cycles: 12.U64 }
                } else {
                    { st: e8.st, pc: after, cycles: 8.U64 }
                }
            }

            Call(condition) => {
                target = imm16(st, pc)
                after = pc.plus_wrap(2)
                if Instruction.condition_met(condition, target.st.reg.read8(Status)) {
                    { st: push16(target.st, after), pc: target.value, cycles: 24.U64 }
                } else {
                    { st: target.st, pc: after, cycles: 12.U64 }
                }
            }

            Return(Always) => {
                p = pop16(st)
                { st: p.st, pc: p.value, cycles: 16.U64 }
            }

            Return(condition) =>
                if Instruction.condition_met(condition, st.reg.read8(Status)) {
                    p = pop16(st)
                    { st: p.st, pc: p.value, cycles: 20.U64 }
                } else {
                    { st: st, pc: pc, cycles: 8.U64 }
                }

            ReturnAndEnableInterrupts => {
                p = pop16(st)
                { st: { ..p.st, ime: Bool.True }, pc: p.value, cycles: 16.U64 }
            }

            Restart(addr) => { st: push16(st, pc), pc: addr.to_u16(), cycles: 16.U64 }
        }

    execute_cb = |st, pc, byte|
        match Instruction.lookup_prefixed(byte) {
            Bit(index, mode) => {
                s = src8(st, pc, mode)
                { st: { ..s.st, reg: apply_flags(s.st.reg, Alu.bit_test(index, s.value)) }, pc: s.pc, cycles: s.cycles.plus(8) }
            }

            Set(index, mode) => {
                s = src8(st, pc, mode)
                w = dst8(s.st, s.pc, mode, Alu.set_bit(index, s.value))
                { st: w.st, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(8) }
            }

            Reset(index, mode) => {
                s = src8(st, pc, mode)
                w = dst8(s.st, s.pc, mode, Alu.clear_bit(index, s.value))
                { st: w.st, pc: w.pc, cycles: s.cycles.plus(w.cycles).plus(8) }
            }

            Rotate(Left, mode) => rmw(st, pc, mode, mode, 8.U64, |v| Alu.rl(v, carry_flag(st)))
            Rotate(Right, mode) => rmw(st, pc, mode, mode, 8.U64, |v| Alu.rr(v, carry_flag(st)))
            RotateCircular(Left, mode) => rmw(st, pc, mode, mode, 8.U64, Alu.rlc)
            RotateCircular(Right, mode) => rmw(st, pc, mode, mode, 8.U64, Alu.rrc)
            ShiftArithmetic(Left, mode) => rmw(st, pc, mode, mode, 8.U64, Alu.sla)
            ShiftArithmetic(Right, mode) => rmw(st, pc, mode, mode, 8.U64, Alu.sra)
            ShiftLogical(Right, mode) => rmw(st, pc, mode, mode, 8.U64, Alu.srl)
            Swap(mode) => rmw(st, pc, mode, mode, 8.U64, Alu.swap)
        }
}

# --- expects: the cheap CPU-level checks over flat memory; the
# full-machine step semantics stay in GameBoy.roc (design D7) ---

flat_mem : List(U8)
flat_mem = List.repeat(0x00, 0x10000)

# EI delay: IME turns on only after the instruction following EI retires
expect {
    prog = flat_mem.set(0x0100, 0xFB) ?? flat_mem # EI; NOP
    r1 = Cpu.init({}).step(Bus.flat(prog))
    r2 = r1.cpu.step(r1.bus)
    r1.cpu.ime == Bool.False and r1.cpu.ei_pending and r2.cpu.ime
}

# Dispatch vector arithmetic: pending bit 3 vectors to 0x58, pushes PC,
# clears its IF bit, and takes 20 cycles
expect {
    m1 = flat_mem.set(0xFFFF, 0x08) ?? flat_mem
    m2 = m1.set(0xFF0F, 0x08) ?? m1
    c0 = { ..Cpu.init({}), ime: Bool.True }
    r = c0.step(Bus.flat(m2))
    r.cycles == 20
    and r.cpu.reg.read16(ProgramCounter) == 0x0058
    and r.cpu.ime == Bool.False
    and r.bus.read(0xFF0F) == 0x00
    and r.bus.read(0xFFFD) == 0x01
    and r.bus.read(0xFFFC) == 0x00
}
