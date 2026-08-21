# The SoC's divider/timer unit as pure arithmetic: counters in, register
# updates out. The bus owns the DIV/TIMA/TMA/TAC register bytes in `mem`;
# it reads them, calls `tick`, applies the returned pokes, and raises the
# timer interrupt (IF bit 2) when `irq` is set. Register updates come
# back as [Unchanged, Set(U8)] so the bus pokes exactly when the fused
# original did — no redundant writes on the per-instruction hot path.

Timer := {
	div_counter : U64,
	tima_counter : U64,
}.{
	init : {} -> Timer
	init = |_| { div_counter: 0, tima_counter: 0 }

	# DIV: any write resets the divider's internal counter
	reset_div : Timer -> Timer
	reset_div = |t| { ..t, div_counter: 0 }

	# Advance DIV/TIMA by elapsed T-cycles; TIMA overflow reloads TMA
	# (wrapping the excess through the reload value) and requests the
	# timer interrupt. TIMA's counter holds still while TAC disables it.
	tick : Timer, { div : U8, tac : U8, tima : U8, tma : U8 }, U64 -> { timer : Timer, div : [Unchanged, Set(U8)], tima : [Unchanged, Set(U8)], irq : Bool }
	tick = |t, regs, cycles| {
		div_total = t.div_counter.plus(cycles)
		div_incs = div_total // 256
		(with_div, div_out) =
			if div_incs > 0 {
				({ ..t, div_counter: div_total % 256 }, Set(regs.div.plus_wrap(div_incs.to_u8_wrap())))
			} else {
				({ ..t, div_counter: div_total }, Unchanged)
			}
		if regs.tac.bitwise_and(0x04) == 0x00 {
			{ timer: with_div, div: div_out, tima: Unchanged, irq: Bool.False }
		} else {
			period =
				match regs.tac.bitwise_and(0x03) {
					0 => 1024
					1 => 16
					2 => 64
					_ => 256
				}
			tima_total = with_div.tima_counter.plus(cycles)
			tima_incs = tima_total // period
			advanced = { ..with_div, tima_counter: tima_total % period }
			if tima_incs == 0 {
				{ timer: advanced, div: div_out, tima: Unchanged, irq: Bool.False }
			} else {
				sum = regs.tima.to_u64().plus(tima_incs)
				if sum > 0xFF {
					# Overflow: reload from TMA (wrap the excess through the reload value)
					tma = regs.tma.to_u64()
					reloaded = tma.plus(sum.minus(0x100) % U64.minus(0x100, tma))
					{ timer: advanced, div: div_out, tima: Set(reloaded.to_u8_wrap()), irq: Bool.True }
				} else {
					{ timer: advanced, div: div_out, tima: Set(sum.to_u8_wrap()), irq: Bool.False }
				}
			}
		}
	}
}

# DIV accumulates below 256 cycles without touching the register
expect Timer.init({}).tick({ div: 0x00, tac: 0x00, tima: 0, tma: 0 }, 255).div == Unchanged
expect Timer.init({}).tick({ div: 0x00, tac: 0x00, tima: 0, tma: 0 }, 512).div == Set(0x02)

# TIMA at period 16: one increment, no interrupt
expect {
	r = Timer.init({}).tick({ div: 0x00, tac: 0x05, tima: 0x00, tma: 0x00 }, 16)
	r.tima == Set(0x01) and r.irq == Bool.False
}

# Overflow reloads from TMA and raises the interrupt
expect {
	r = Timer.init({}).tick({ div: 0x00, tac: 0x05, tima: 0xFF, tma: 0xF0 }, 16)
	r.tima == Set(0xF0) and r.irq == Bool.True
}

# Excess increments past the overflow wrap through the reload value
expect {
	# 3 increments from 0xFF with TMA 0xF0: 0x102 -> 0xF0 + (2 % 0x10) = 0xF2
	r = Timer.init({}).tick({ div: 0x00, tac: 0x05, tima: 0xFF, tma: 0xF0 }, 48)
	r.tima == Set(0xF2) and r.irq == Bool.True
}

# Disabled timer: TIMA untouched, its counter holds still
expect {
	r = Timer.init({}).tick({ div: 0x00, tac: 0x00, tima: 0x10, tma: 0x00 }, 4096)
	r.tima == Unchanged and r.timer.tima_counter == 0
}

# Sub-period cycles carry across ticks
expect {
	r1 = Timer.init({}).tick({ div: 0x00, tac: 0x05, tima: 0x00, tma: 0x00 }, 8)
	r2 = r1.timer.tick({ div: 0x00, tac: 0x05, tima: 0x00, tma: 0x00 }, 8)
	r1.tima == Unchanged and r2.tima == Set(0x01)
}
