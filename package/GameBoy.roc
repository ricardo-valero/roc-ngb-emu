# SM83 execution core: fetch/decode/execute over the memory bus.
# `step` is pure state -> state; cycles are reported by execute because only
# it knows conditional outcomes (taken vs. not-taken).

import /Cpu/Alu
import /Cpu/Instruction
import /Cpu/Register
import /Cpu/Register/Status
import /Mmu
import /Ppu

GameBoy := {
    reg : Register,
    mmu : Mmu,
    ppu : Ppu,
    ime : Bool,
    halted : Bool,
    ei_pending : Bool,
}.{
    init : List(U8) -> GameBoy
    init = |rom| {
        gb : GameBoy
        gb = {
            reg: Register.init({}),
            mmu: Mmu.init(rom),
            ppu: Ppu.init({}),
            ime: Bool.False,
            halted: Bool.False,
            ei_pending: Bool.False,
        }
        gb
    }

    serial : GameBoy -> List(U8)
    serial = |gb| gb.mmu.serial()

    framebuffer : GameBoy -> List(U8)
    framebuffer = |gb| gb.ppu.frame()

    # Run until the next VBlank entry (LY reaching 144), bounded so a wedged
    # ROM cannot hang the caller. A frame is ~17.6k steps even when halted.
    run_frame : GameBoy -> GameBoy
    run_frame = |gb0| {
        var gb = gb0
        var budget = 40000.U64
        var vblank_seen = Bool.False
        while budget > 0 and vblank_seen == Bool.False {
            was_ly = gb.mmu.read(0xFF44)
            gb = match gb.step() { (g, _) => g }
            if was_ly != 144 and gb.mmu.read(0xFF44) == 144 {
                vblank_seen = Bool.True
            } else {
                {}
            }
            budget = budget.minus(1)
        }
        gb
    }

    step : GameBoy -> (GameBoy, U64)
    step = |gb| {
        pending = gb.mmu.read(0xFFFF).bitwise_and(gb.mmu.read(0xFF0F)).bitwise_and(0x1F)
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

    # Every path leaves through here so the timer and PPU see all elapsed cycles
    finish : GameBoy, U64 -> (GameBoy, U64)
    finish = |gb, cycles| {
        r = gb.ppu.tick(gb.mmu.tick(cycles), cycles)
        ({ ..gb, mmu: r.mmu, ppu: r.ppu }, cycles)
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
        mmu2 = gb1.mmu.poke(0xFF0F, gb1.mmu.read(0xFF0F).bitwise_and(U8.shl_wrap(1, bit).bitwise_not()))
        reg2 = gb1.reg.write16(ProgramCounter, bit.to_u16().shl_wrap(3).plus(0x0040))
        finish({ ..gb1, reg: reg2, mmu: mmu2, ime: Bool.False, halted: Bool.False }, 20)
    }

    run : GameBoy -> (GameBoy, U64)
    run = |gb0| {
        was_ei_pending = gb0.ei_pending
        pc = gb0.reg.read16(ProgramCounter)
        opcode = gb0.mmu.read(pc)
        r = execute(gb0, pc.plus_wrap(1), Instruction.lookup(opcode))
        gb1 = { ..r.gb, reg: r.gb.reg.write16(ProgramCounter, r.pc) }
        # EI takes effect after the instruction that follows it (DI cancels)
        gb2 =
            if was_ei_pending and gb1.ei_pending {
                { ..gb1, ime: Bool.True, ei_pending: Bool.False }
            } else {
                gb1
            }
        finish(gb2, r.cycles)
    }

    imm16 : GameBoy, U16 -> U16
    imm16 = |gb, pc|
        gb.mmu.read(pc).to_u16().bitwise_or(gb.mmu.read(pc.plus_wrap(1)).to_u16().shl_wrap(8))

    sign_extend : U8 -> U16
    sign_extend = |e8|
        if e8 >= 0x80 { e8.to_u16().bitwise_or(0xFF00) } else { e8.to_u16() }

    mem_write : GameBoy, U16, U8 -> GameBoy
    mem_write = |gb, addr, value| { ..gb, mmu: gb.mmu.write(addr, value) }

    push16 : GameBoy, U16 -> GameBoy
    push16 = |gb, value| {
        sp = gb.reg.read16(StackPointer)
        gb2 = mem_write(mem_write(gb, sp.minus_wrap(1), value.shr_zf_wrap(8).to_u8_wrap()), sp.minus_wrap(2), value.to_u8_wrap())
        { ..gb2, reg: gb2.reg.write16(StackPointer, sp.minus_wrap(2)) }
    }

    pop16 : GameBoy -> { gb : GameBoy, value : U16 }
    pop16 = |gb| {
        sp = gb.reg.read16(StackPointer)
        value = gb.mmu.read(sp.plus_wrap(1)).to_u16().shl_wrap(8).bitwise_or(gb.mmu.read(sp).to_u16())
        { gb: { ..gb, reg: gb.reg.write16(StackPointer, sp.plus_wrap(2)) }, value: value }
    }

    apply_flags = |reg, delta| reg.write8(Status, delta(reg.read8(Status)))

    carry_flag : GameBoy -> Bool
    carry_flag = |gb| Status.check(Carry, gb.reg.read8(Status))

    # Read an 8-bit source operand; returns extra cycles beyond the base cost
    src8 = |gb, pc, mode|
        match mode {
            Immediate => { gb: gb, value: gb.mmu.read(pc), pc: pc.plus_wrap(1), cycles: 4.U64 }
            Direct8(r) => { gb: gb, value: gb.reg.read8(r), pc: pc, cycles: 0.U64 }
            Direct16(_) => { gb: gb, value: 0xFF, pc: pc, cycles: 0.U64 } # not an 8-bit operand
            Indirect(target) =>
                match target {
                    C => { gb: gb, value: gb.mmu.read(gb.reg.read8(C).to_u16().plus(0xFF00)), pc: pc, cycles: 4.U64 }
                    BC => { gb: gb, value: gb.mmu.read(gb.reg.read16(BC)), pc: pc, cycles: 4.U64 }
                    DE => { gb: gb, value: gb.mmu.read(gb.reg.read16(DE)), pc: pc, cycles: 4.U64 }
                    HL => { gb: gb, value: gb.mmu.read(gb.reg.read16(HL)), pc: pc, cycles: 4.U64 }
                    HLPostIncrement => {
                        hl = gb.reg.read16(HL)
                        { gb: { ..gb, reg: gb.reg.write16(HL, hl.plus_wrap(1)) }, value: gb.mmu.read(hl), pc: pc, cycles: 4.U64 }
                    }

                    HLPostDecrement => {
                        hl = gb.reg.read16(HL)
                        { gb: { ..gb, reg: gb.reg.write16(HL, hl.minus_wrap(1)) }, value: gb.mmu.read(hl), pc: pc, cycles: 4.U64 }
                    }

                    Word8Operand => {
                        n = gb.mmu.read(pc)
                        { gb: gb, value: gb.mmu.read(n.to_u16().plus(0xFF00)), pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(gb, pc)
                        { gb: gb, value: gb.mmu.read(addr), pc: pc.plus_wrap(2), cycles: 12.U64 }
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
                        n = gb.mmu.read(pc)
                        { gb: mem_write(gb, n.to_u16().plus(0xFF00), value), pc: pc.plus_wrap(1), cycles: 8.U64 }
                    }

                    Word16Operand => {
                        addr = imm16(gb, pc)
                        { gb: mem_write(gb, addr, value), pc: pc.plus_wrap(2), cycles: 12.U64 }
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
            Stop => { gb: gb, pc: pc.plus_wrap(1), cycles: 4.U64 } # NOP that skips its padding byte
            Halt => { gb: { ..gb, halted: Bool.True }, pc: pc, cycles: 4.U64 }
            Illegal => { gb: gb, pc: pc, cycles: 4.U64 } # real hardware locks up
            Unknown => { gb: gb, pc: pc, cycles: 4.U64 }
            Prefix => execute_cb(gb, pc.plus_wrap(1), gb.mmu.read(pc))
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
            AddStackPointerImmediate =>
                match Alu.add_sp(gb.reg.read16(StackPointer), gb.mmu.read(pc)) {
                    (value, delta) => { gb: { ..gb, reg: apply_flags(gb.reg.write16(StackPointer, value), delta) }, pc: pc.plus_wrap(1), cycles: 16.U64 }
                }

            LoadHLStackPointerImmediate =>
                match Alu.add_sp(gb.reg.read16(StackPointer), gb.mmu.read(pc)) {
                    (value, delta) => { gb: { ..gb, reg: apply_flags(gb.reg.write16(HL, value), delta) }, pc: pc.plus_wrap(1), cycles: 12.U64 }
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
                sp = gb.reg.read16(StackPointer)
                gb2 = mem_write(mem_write(gb, addr, sp.to_u8_wrap()), addr.plus_wrap(1), sp.shr_zf_wrap(8).to_u8_wrap())
                { gb: gb2, pc: pc.plus_wrap(2), cycles: 20.U64 }
            }

            Load16(dst, Immediate) => { gb: write16m(gb, dst, imm16(gb, pc)), pc: pc.plus_wrap(2), cycles: 12.U64 }
            Load16(_, _) => { gb: gb, pc: pc, cycles: 4.U64 } # no other encodings exist
            Push(mode) => { gb: push16(gb, read16m(gb, mode)), pc: pc, cycles: 16.U64 }
            Pop(mode) => {
                p = pop16(gb)
                { gb: write16m(p.gb, mode, p.value), pc: pc, cycles: 12.U64 }
            }

            Jump(_, HL) => { gb: gb, pc: gb.reg.read16(HL), cycles: 4.U64 }
            Jump(condition, Immediate) => {
                target = imm16(gb, pc)
                if Instruction.condition_met(condition, gb.reg.read8(Status)) {
                    { gb: gb, pc: target, cycles: 16.U64 }
                } else {
                    { gb: gb, pc: pc.plus_wrap(2), cycles: 12.U64 }
                }
            }

            Branch(condition) => {
                offset = sign_extend(gb.mmu.read(pc))
                after = pc.plus_wrap(1)
                if Instruction.condition_met(condition, gb.reg.read8(Status)) {
                    { gb: gb, pc: after.plus_wrap(offset), cycles: 12.U64 }
                } else {
                    { gb: gb, pc: after, cycles: 8.U64 }
                }
            }

            Call(condition) => {
                target = imm16(gb, pc)
                after = pc.plus_wrap(2)
                if Instruction.condition_met(condition, gb.reg.read8(Status)) {
                    { gb: push16(gb, after), pc: target, cycles: 24.U64 }
                } else {
                    { gb: gb, pc: after, cycles: 12.U64 }
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
    gb1 = { ..gb0, ime: Bool.True, mmu: gb0.mmu.write(0xFFFF, 0x01).poke(0xFF0F, 0x01) }
    match gb1.step() {
        (g, c) =>
            c == 20
            and g.reg.read16(ProgramCounter) == 0x0040
            and g.ime == Bool.False
            and g.mmu.read(0xFF0F).bitwise_and(0x01) == 0x00
            and g.mmu.read(0xFFFD) == 0x01
            and g.mmu.read(0xFFFC) == 0x00
    }
}

# EI delay: the instruction after EI runs before dispatch
expect {
    gb0 = GameBoy.init(rom_with([0xFB, 0x00])) # EI; NOP
    gb1 = { ..gb0, mmu: gb0.mmu.write(0xFFFF, 0x01).poke(0xFF0F, 0x01) }
    g2 = after_step(after_step(gb1)) # EI, then NOP still executes
    g3 = after_step(g2) # now the interrupt dispatches
    g2.reg.read16(ProgramCounter) == 0x0102 and g3.reg.read16(ProgramCounter) == 0x0040
}

# HALT wakes without IME: resumes after HALT, no vector taken
expect {
    gb0 = GameBoy.init(rom_with([0x76, 0x3E, 0x07])) # HALT; LD A, 0x07
    # post-boot IF is 0xE1, so clear it or the HALT wakes immediately
    gb1 = { ..gb0, mmu: gb0.mmu.write(0xFFFF, 0x01).poke(0xFF0F, 0x00) }
    g1 = after_step(gb1) # halts
    g2 = after_step(g1) # nothing pending: stays halted
    g3 = after_step({ ..g2, mmu: g2.mmu.poke(0xFF0F, 0x01) }) # wakes, runs the LD
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
    gb = GameBoy.init(rom_with([0x18, 0xFE])).run_frame() # JR -2: tight loop
    fb = gb.framebuffer()
    gb.mmu.read(0xFF44) == 144
    and fb.len() == 23040
    and fb.fold(Bool.True, |ok, shade| ok and shade <= 3)
}
