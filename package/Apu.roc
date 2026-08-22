# DMG audio processing unit: four channels driven by a 512 Hz frame
# sequencer, ticked from CPU-step cycles like the timer and PPU. Registers
# live in the bus (with read-back masks applied there; their *formats*
# decode in Apu/Register), and the hidden per-channel state lives in
# Apu/Channel. This module is the bus-coupled orchestration: events,
# triggers, the frame sequencer, sweep writeback, and the mixer.
# https://gbdev.io/pandocs/Audio_details.html

import /Apu/Channel
import /Apu/Register
import /Bus

Apu := {
	ch1 : Channel,
	ch2 : Channel,
	ch3 : Channel,
	ch4 : Channel,
	pending : U64, # cycles accumulated since the last real advance (batching)
	fs_timer : U64, # counts to 8192 cycles = one 512 Hz frame-sequencer step
	fs_step : U8,
	sample_acc : U64, # fractional accumulator: emit when >= 4194304
}.{
	init : {} -> Apu
	init = |_| {
		apu = {
			ch1: Channel.blank({}),
			ch2: Channel.blank({}),
			ch3: Channel.blank({}),
			ch4: Channel.blank({}),
			pending: 0,
			fs_timer: 0,
			fs_step: 7, # so the first step to fire is 0, as after power-on
			sample_acc: 0,
		}
		apu
	}

	# Called every CPU step. Real work happens only when a register event
	# arrived or enough cycles accumulated to matter (the next 48 kHz sample
	# is ~87 cycles away) — per-instruction the cost is one compare.
	tick : Apu, Bus, U64 -> { apu : Apu, bus : Bus }
	tick = |apu, bus, cycles| {
		if bus.apu_events.len() > 0 {
			# the writes happened during this step: advance the batched time
			# first so the quirks observe the frame sequencer as of the write
			pre = advance({ ..apu, pending: 0 }, bus, apu.pending)
			applied = apply_events(pre.apu, pre.bus)
			advance(applied.apu, applied.bus, cycles)
		} else {
			total = apu.pending.plus(cycles)
			# flush when the batch crosses a frame-sequencer step so NR52
			# status updates within an instruction of a length clock — test
			# ROMs poll NR52 to sync against the sequencer phase
			if total >= 128 or apu.fs_timer.plus(total) >= 8192 {
				advance({ ..apu, pending: 0 }, bus, total)
			} else {
				{ apu: { ..apu, pending: total }, bus: bus }
			}
		}
	}

	apply_events : Apu, Bus -> { apu : Apu, bus : Bus }
	apply_events = |apu0, bus0| {
		drained = bus0.take_apu_events()
		bus = drained.bus # last use of `drained`'s bus: it stays uniquely owned
		{ apu: drained.events.fold(apu0, |apu, event| handle_event(apu, bus, event)), bus: bus }
	}

	advance : Apu, Bus, U64 -> { apu : Apu, bus : Bus }
	advance = |apu0, bus0, cycles| {
		var bus = bus0
		var apu = apu0

		# waveform timers
		apu = {
			..apu,
			ch1: apu.ch1.advance_pulse(pulse_period(bus, 0xFF13, 0xFF14), cycles),
			ch2: apu.ch2.advance_pulse(pulse_period(bus, 0xFF18, 0xFF19), cycles),
			ch3: apu.ch3.advance_wave(wave_period(bus), cycles),
			ch4: apu.ch4.advance_noise(noise_period(bus), Register.noise_width7(bus.read_raw(0xFF22)), cycles),
		}

		# frame sequencer
		var fs = apu.fs_timer.plus(cycles)
		while fs >= 8192 {
			fs = fs.minus(8192)
			step = apu.fs_step.plus(1).bitwise_and(0x07)
			apu = { ..apu, fs_step: step }
			clocks = sequencer_clocks(step)
			if clocks.length {
				apu = clock_lengths(apu, bus)
			} else {
				{}
			}
			if clocks.sweep {
				s = clock_sweep(apu, bus)
				apu = s.apu
				bus = s.bus
			} else {
				{}
			}
			if clocks.envelope {
				apu = clock_envelopes(apu, bus)
			} else {
				{}
			}
		}
		apu = { ..apu, fs_timer: fs }

		# NR52 status bits (before the sample block: it consumes `apu`)
		bus2 =
			if bus.apu_powered() {
				bus.poke(0xFF26, status_byte(apu, bus))
			} else {
				bus
			}

		# sample emission: all samples within one advance share the end-of-tick
		# state, so mix once and push into the bus-side buffer (the bus stays
		# uniquely owned through the step pipeline, so appends are in-place)
		pair = mix(apu, bus2)
		var acc = apu.sample_acc.plus(cycles * 48000)
		var bus3 = bus2
		while acc >= 4194304 {
			acc = acc.minus(4194304)
			bus3 = bus3.push_sample_pair(pair.left, pair.right)
		}
		{ apu: { ..apu, sample_acc: acc }, bus: bus3 }
	}

	status_byte : Apu, Bus -> U8
	status_byte = |apu, bus| {
		power = (bus.read_raw(0xFF26)).bitwise_and(0x80)
		power
			.bitwise_or(
				if apu.ch1.enabled {
					0x01
				} else {
					0x00
				},
			)
			.bitwise_or(
				if apu.ch2.enabled {
					0x02
				} else {
					0x00
				},
			)
			.bitwise_or(
				if apu.ch3.enabled {
					0x04
				} else {
					0x00
				},
			)
			.bitwise_or(
				if apu.ch4.enabled {
					0x08
				} else {
					0x00
				},
			)
	}

	# --- events ---

	# Events are the low byte of the written register's address (see
	# Bus.apu_reg_event), plus 0xF0/0xF1 for power off/on
	handle_event : Apu, Bus, U8 -> Apu
	handle_event = |apu, bus, event|
		match event {
			0x10 => { ..apu, ch1: nr10_gate(apu.ch1, bus) }
			0x11 => { ..apu, ch1: { ..apu.ch1, length: Register.len64(bus.read_raw(0xFF11)) } }
			0x12 => { ..apu, ch1: apu.ch1.dac_gate(Register.dac_on(bus.read_raw(0xFF12))) }
			0x14 => { ..apu, ch1: nrx4(apu, apu.ch1, bus, 0xFF14, 64, Ch1) }
			0x16 => { ..apu, ch2: { ..apu.ch2, length: Register.len64(bus.read_raw(0xFF16)) } }
			0x17 => { ..apu, ch2: apu.ch2.dac_gate(Register.dac_on(bus.read_raw(0xFF17))) }
			0x19 => { ..apu, ch2: nrx4(apu, apu.ch2, bus, 0xFF19, 64, Ch2) }
			0x1A => { ..apu, ch3: apu.ch3.dac_gate(Register.wave_dac_on(bus.read_raw(0xFF1A))) }
			0x1B => { ..apu, ch3: { ..apu.ch3, length: Register.len256(bus.read_raw(0xFF1B)) } }
			0x1E => { ..apu, ch3: nrx4(apu, apu.ch3, bus, 0xFF1E, 256, Wave) }
			0x20 => { ..apu, ch4: { ..apu.ch4, length: Register.len64(bus.read_raw(0xFF20)) } }
			0x21 => { ..apu, ch4: apu.ch4.dac_gate(Register.dac_on(bus.read_raw(0xFF21))) }
			0x23 => { ..apu, ch4: nrx4(apu, apu.ch4, bus, 0xFF23, 64, Noise) }
			0xF0 => power_off(apu, bus)
			0xF1 => power_on(apu)
			_ => apu
		}

	# NR10: clearing negate after a negate-mode calculation kills CH1
	nr10_gate : Channel, Bus -> Channel
	nr10_gate = |ch, bus|
		if Register.sweep(bus.read_raw(0xFF10)).negate == Bool.False and ch.sweep_neg_used {
			{ ..ch, enabled: Bool.False }
		} else {
			ch
		}

	# The next frame-sequencer step won't clock lengths (fs_step holds the
	# step that fired last; lengths clock on even steps)
	first_half : Apu -> Bool
	first_half = |apu| apu.fs_step.bitwise_and(0x01) == 0x00

	# NRx4 write: length-enable edge clocking, then trigger on bit 7. A
	# trigger only reloads an expired counter — to max, or max-1 when
	# enabling in the first half of the length period.
	nrx4 : Apu, Channel, Bus, U16, U16, [Ch1, Ch2, Wave, Noise] -> Channel
	nrx4 = |apu, ch0, bus, addr, max_len, which| {
		v = bus.read_raw(addr)
		new_en = Register.length_enabled(v)
		trigger = Register.trigger(v)
		fh = first_half(apu)
		edge =
			if new_en and ch0.len_en == Bool.False and fh and ch0.length > 0 {
				rem = ch0.length.minus(1)
				if rem == 0 and trigger == Bool.False {
					{ ..ch0, length: 0, enabled: Bool.False }
				} else {
					{ ..ch0, length: rem }
				}
			} else {
				ch0
			}
		done =
			if trigger {
				t =
					match which {
						Ch1 => trigger_ch1(edge, bus)
						Ch2 => trigger_ch2(edge, bus)
						Wave => trigger_wave(edge, bus)
						Noise => trigger_noise(edge, bus)
					}
				if t.length == 0 {
					{
						..t,
						length: if new_en and fh {
							max_len.minus(1)
						} else {
							max_len
						},
					}
				} else {
					t
				}
			} else {
				edge
			}
		{ ..done, len_en: new_en }
	}

	# Power off clears channel state; on DMG the length counters survive
	power_off : Apu, Bus -> Apu
	power_off = |apu, bus|
		if bus.is_cgb() {
			{ ..apu, ch1: Channel.blank({}), ch2: Channel.blank({}), ch3: Channel.blank({}), ch4: Channel.blank({}) }
		} else {
			{
				..apu,
				ch1: { ..Channel.blank({}), length: apu.ch1.length },
				ch2: { ..Channel.blank({}), length: apu.ch2.length },
				ch3: { ..Channel.blank({}), length: apu.ch3.length },
				ch4: { ..Channel.blank({}), length: apu.ch4.length },
			}
		}

	# Power on restarts the frame sequencer at step 0 and rewinds waveforms
	power_on : Apu -> Apu
	power_on = |apu| {
		..apu,
		fs_step: 7,
		fs_timer: 0,
		ch1: { ..apu.ch1, duty_pos: 0 },
		ch2: { ..apu.ch2, duty_pos: 0 },
		ch3: { ..apu.ch3, wave_pos: 0 },
	}

	trigger_pulse : Channel, Bus, U16, U64 -> Channel
	trigger_pulse = |ch, bus, nrx2, period| {
		env = Register.envelope(bus.read_raw(nrx2))
		{
			..ch,
			enabled: Register.dac_on(bus.read_raw(nrx2)),
			timer: period,
			volume: env.volume,
			env_timer: env.period,
		}
	}

	trigger_ch2 : Channel, Bus -> Channel
	trigger_ch2 = |ch, bus| trigger_pulse(ch, bus, 0xFF17, pulse_period(bus, 0xFF18, 0xFF19))

	trigger_ch1 : Channel, Bus -> Channel
	trigger_ch1 = |ch0, bus| {
		ch = trigger_pulse(ch0, bus, 0xFF12, pulse_period(bus, 0xFF13, 0xFF14))
		sw = Register.sweep(bus.read_raw(0xFF10))
		shadow = raw_frequency(bus, 0xFF13, 0xFF14)
		armed = {
			..ch,
			sweep_shadow: shadow,
			sweep_timer: if sw.period == 0 {
				8
			} else {
				sw.period
			},
			sweep_enabled: sw.period != 0 or sw.shift != 0,
			sweep_neg_used: Bool.False,
		}
		# immediate calculation when a shift is set (counts for the negate quirk)
		if sw.shift != 0 {
			calc = { ..armed, sweep_neg_used: sw.negate }
			if Register.sweep_next(calc.sweep_shadow, sw) > 2047 {
				{ ..calc, enabled: Bool.False }
			} else {
				calc
			}
		} else {
			armed
		}
	}

	trigger_wave : Channel, Bus -> Channel
	trigger_wave = |ch, bus| {
		..ch,
		enabled: Register.wave_dac_on(bus.read_raw(0xFF1A)),
		timer: wave_period(bus),
		wave_pos: 0,
	}

	trigger_noise : Channel, Bus -> Channel
	trigger_noise = |ch, bus| {
		env = Register.envelope(bus.read_raw(0xFF21))
		{
			..ch,
			enabled: Register.dac_on(bus.read_raw(0xFF21)),
			timer: noise_period(bus),
			volume: env.volume,
			env_timer: env.period,
			lfsr: 0x7FFF,
		}
	}

	# --- register map: address + format compositions (the formats live in
	# Apu/Register; the addresses are this module's knowledge) ---

	raw_frequency : Bus, U16, U16 -> U16
	raw_frequency = |bus, lo, hi| Register.frequency(bus.read_raw(lo), bus.read_raw(hi))

	pulse_period : Bus, U16, U16 -> U64
	pulse_period = |bus, lo, hi| Register.pulse_period(raw_frequency(bus, lo, hi))

	wave_period : Bus -> U64
	wave_period = |bus| Register.wave_period(raw_frequency(bus, 0xFF1D, 0xFF1E))

	noise_period : Bus -> U64
	noise_period = |bus| Register.noise_period(bus.read_raw(0xFF22))

	# --- frame sequencer clocks ---

	# The 512 Hz sequencer's schedule, transcribed from the Pandocs table
	sequencer_clocks : U8 -> { length : Bool, sweep : Bool, envelope : Bool }
	sequencer_clocks = |step|
		match step {
			0 => { length: Bool.True, sweep: Bool.False, envelope: Bool.False }
			1 => { length: Bool.False, sweep: Bool.False, envelope: Bool.False }
			2 => { length: Bool.True, sweep: Bool.True, envelope: Bool.False }
			3 => { length: Bool.False, sweep: Bool.False, envelope: Bool.False }
			4 => { length: Bool.True, sweep: Bool.False, envelope: Bool.False }
			5 => { length: Bool.False, sweep: Bool.False, envelope: Bool.False }
			6 => { length: Bool.True, sweep: Bool.True, envelope: Bool.False }
			_ => { length: Bool.False, sweep: Bool.False, envelope: Bool.True }
		}

	clock_lengths : Apu, Bus -> Apu
	clock_lengths = |apu, bus| {
		..apu,
		ch1: apu.ch1.clock_length(Register.length_enabled(bus.read_raw(0xFF14))),
		ch2: apu.ch2.clock_length(Register.length_enabled(bus.read_raw(0xFF19))),
		ch3: apu.ch3.clock_length(Register.length_enabled(bus.read_raw(0xFF1E))),
		ch4: apu.ch4.clock_length(Register.length_enabled(bus.read_raw(0xFF23))),
	}

	clock_envelopes : Apu, Bus -> Apu
	clock_envelopes = |apu, bus| {
		..apu,
		ch1: apu.ch1.clock_envelope(Register.envelope(bus.read_raw(0xFF12))),
		ch2: apu.ch2.clock_envelope(Register.envelope(bus.read_raw(0xFF17))),
		ch4: apu.ch4.clock_envelope(Register.envelope(bus.read_raw(0xFF21))),
	}

	clock_sweep : Apu, Bus -> { apu : Apu, bus : Bus }
	clock_sweep = |apu, bus| {
		ch = apu.ch1
		sw = Register.sweep(bus.read_raw(0xFF10))
		if ch.sweep_timer > 1 {
			{ apu: { ..apu, ch1: { ..ch, sweep_timer: ch.sweep_timer.minus(1) } }, bus: bus }
		} else {
			reloaded = {
				..ch,
				sweep_timer: if sw.period == 0 {
					8
				} else {
					sw.period
				},
			}
			if reloaded.sweep_enabled and sw.period != 0 {
				# any calculation in negate mode arms the NR10 negate-clear quirk
				calc = { ..reloaded, sweep_neg_used: reloaded.sweep_neg_used or sw.negate }
				next = Register.sweep_next(calc.sweep_shadow, sw)
				if next > 2047 {
					{ apu: { ..apu, ch1: { ..calc, enabled: Bool.False } }, bus: bus }
				} else if sw.shift != 0 {
					bus2 = bus
						.poke(0xFF13, next.to_u8_wrap())
						.poke(0xFF14, bus.read_raw(0xFF14).bitwise_and(0xF8).bitwise_or(next.shr_zf_wrap(8).to_u8_wrap()))
					updated = { ..calc, sweep_shadow: next }
					if Register.sweep_next(next, sw) > 2047 {
						{ apu: { ..apu, ch1: { ..updated, enabled: Bool.False } }, bus: bus2 }
					} else {
						{ apu: { ..apu, ch1: updated }, bus: bus2 }
					}
				} else {
					{ apu: { ..apu, ch1: calc }, bus: bus }
				}
			} else {
				{ apu: { ..apu, ch1: reloaded }, bus: bus }
			}
		}
	}

	# --- output ---

	wave_out : Channel, Bus -> U8
	wave_out = |ch, bus|
		if ch.enabled {
			byte = bus.read_raw(U16.plus(0xFF30, ch.wave_pos.shr_zf_wrap(1).to_u16()))
			nibble =
				if ch.wave_pos.bitwise_and(0x01) == 0x00 {
					byte.shr_zf_wrap(4)
				} else {
					byte.bitwise_and(0x0F)
				}
			match Register.wave_volume(bus.read_raw(0xFF1C)) {
				Mute => 0
				Shift(s) => nibble.shr_zf_wrap(s)
			}
		} else {
			0
		}

	# DAC: 0-15 maps to [-1, 1]; each channel contributes a quarter
	dac : U8, Bool -> F32
	dac = |out, on|
		if on {
			(out.to_f32() - 7.5) / 7.5 * 0.25
		} else {
			0.0
		}

	mix : Apu, Bus -> { left : F32, right : F32 }
	mix = |apu, bus| {
		o1 = dac(apu.ch1.pulse_out(Register.duty(bus.read_raw(0xFF11))), apu.ch1.enabled)
		o2 = dac(apu.ch2.pulse_out(Register.duty(bus.read_raw(0xFF16))), apu.ch2.enabled)
		o3 = dac(wave_out(apu.ch3, bus), apu.ch3.enabled)
		o4 = dac(apu.ch4.noise_out(), apu.ch4.enabled)
		nr51 = bus.read_raw(0xFF25)
		master = Register.master_volume(bus.read_raw(0xFF24))
		{
			left: (route(o1, nr51, 4) + route(o2, nr51, 5) + route(o3, nr51, 6) + route(o4, nr51, 7)) * master.left,
			right: (route(o1, nr51, 0) + route(o2, nr51, 1) + route(o3, nr51, 2) + route(o4, nr51, 3)) * master.right,
		}
	}

	route : F32, U8, U8 -> F32
	route = |sample, nr51, bit|
		if Register.routed(nr51, bit) {
			sample
		} else {
			0.0
		}
}

# --- expects ---

test_rom : List(U8)
test_rom = List.repeat(0x00, 0x8000)

fresh : {} -> { apu : Apu, bus : Bus }
fresh = |_| { apu: Apu.init({}), bus: Bus.init(test_rom) }

# Length expiry silences a channel and clears its NR52 bit
expect {
	f = fresh({})
	m = f.bus.write(0xFF16, 0x3F).write(0xFF17, 0xF0).write(0xFF19, 0xC0) # len 1, vol 15, trigger+len-enable
	r = f.apu.tick(m, 4)
	after = r.apu.tick(r.bus, 16384) # at least one 256 Hz length clock
	r.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x02
		and after.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Envelope steps down at 64 Hz
expect {
	f = fresh({})
	m = f.bus.write(0xFF17, 0xF1).write(0xFF19, 0x80) # vol 15, decrease, period 1
	r = f.apu.tick(m, 4)
	after = r.apu.tick(r.bus, 8192 * 8) # one full sequencer round: one envelope clock
	r.apu.ch2.volume == 15 and after.apu.ch2.volume == 14
}

# Duty walker advances with the frequency timer (freq 0x7FF: period 4)
expect {
	f = fresh({})
	m = f.bus.write(0xFF18, 0xFF).write(0xFF17, 0xF0).write(0xFF19, 0x87)
	r = f.apu.tick(m, 0) # drain the trigger only; timer loaded, no steps yet
	after = r.apu.tick(r.bus, 144) # 36 steps of period 4: duty_pos 36 % 8 = 4
	after.apu.ch2.duty_pos == 4
}

# The frame-sequencer schedule: lengths on even steps, sweep on 2 and 6,
# envelope only on 7
expect {
	[0, 1, 2, 3, 4, 5, 6, 7].fold(
		Bool.True,
		|ok, s| {
			c = Apu.sequencer_clocks(s)
			ok
				and c.length == (s.bitwise_and(0x01) == 0x00)
					and c.sweep == (s == 2 or s == 6)
						and c.envelope == (s == 7)
		},
	)
}

# Sweep overflow disables CH1 at trigger
expect {
	f = fresh({})
	m = f.bus.write(0xFF10, 0x01).write(0xFF12, 0xF0).write(0xFF13, 0xFF).write(0xFF14, 0x87)
	r = f.apu.tick(m, 4)
	r.apu.ch1.enabled == Bool.False and r.bus.read_raw(0xFF26).bitwise_and(0x01) == 0x00
}

# Sweep with room keeps the channel alive
expect {
	f = fresh({})
	m = f.bus.write(0xFF10, 0x11).write(0xFF12, 0xF0).write(0xFF13, 0x00).write(0xFF14, 0x84)
	r = f.apu.tick(m, 4)
	r.apu.ch1.enabled == Bool.True
}

# Wave channel follows wave RAM nibbles
expect {
	f = fresh({})
	m = f.bus
		.write(0xFF30, 0xF0) # nibble 0 = 15, nibble 1 = 0
		.write(0xFF1A, 0x80) # DAC on
		.write(0xFF1C, 0x20) # volume 100%
		.write(0xFF1D, 0x00)
		.write(0xFF1E, 0x87) # trigger, freq 0x700: period (2048-1792)*2 = 512
	r = f.apu.tick(m, 4)
	w0 = Apu.wave_out(r.apu.ch3, r.bus)
	stepped = r.apu.tick(r.bus, 512)
	w1 = Apu.wave_out(stepped.apu.ch3, stepped.bus)
	w0 == 15 and w1 == 0
}

# Panning: CH2 routed left only produces silent right samples
expect {
	f = fresh({})
	m = f.bus
		.write(0xFF25, 0x20) # CH2 left only
		.write(0xFF16, 0x00) # duty 0: high at pos 0 right after trigger
		.write(0xFF17, 0xF0)
		.write(0xFF18, 0x00)
		.write(0xFF19, 0x80) # trigger, freq 0: period 8192
	r = f.apu.tick(m, 4)
	out = r.apu.tick(r.bus, 128) # one batched advance: exactly one stereo pair
	samples = out.bus.take_samples().samples
	samples.len() == 2 and (samples.get(0) ?? 0.0) > 0.0 and (samples.get(1) ?? 0.0) == 0.0
}

# One video frame yields ~800 stereo pairs at 48 kHz
expect {
	f = fresh({})
	r = f.apu.tick(f.bus, 70224)
	n = r.bus.take_samples().samples.len()
	n >= 1600 and n <= 1610
}

# Power off blanks every channel
expect {
	f = fresh({})
	m = f.bus.write(0xFF17, 0xF0).write(0xFF19, 0x80)
	r = f.apu.tick(m, 4)
	off = r.apu.tick(r.bus.write(0xFF26, 0x00), 4)
	r.apu.ch2.enabled == Bool.True and off.apu.ch2.enabled == Bool.False
}

# NRx1 reloads the length counter mid-note: a rewritten short length expires
expect {
	f = fresh({})
	m = f.bus.write(0xFF17, 0xF0).write(0xFF19, 0xC0) # trigger with length enabled: counter 64
	r = f.apu.tick(m, 4)
	r2 = r.apu.tick(r.bus.write(0xFF16, 0x3F), 4) # rewrite mid-note: counter 1
	after = r2.apu.tick(r2.bus, 16384)
	r2.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x02
		and after.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Turning the DAC off silences the channel without waiting for a length clock
expect {
	f = fresh({})
	m = f.bus.write(0xFF17, 0xF0).write(0xFF19, 0x80)
	r = f.apu.tick(m, 4)
	off = r.apu.tick(r.bus.write(0xFF17, 0x00), 4)
	r.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x02
		and off.bus.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Clearing sweep negate after a negate-mode calculation disables CH1
expect {
	f = fresh({})
	m = f.bus.write(0xFF10, 0x09).write(0xFF12, 0xF0).write(0xFF13, 0x40).write(0xFF14, 0x84)
	r = f.apu.tick(m, 4) # trigger runs an immediate negate-mode calculation
	cleared = r.apu.tick(r.bus.write(0xFF10, 0x01), 4)
	r.apu.ch1.enabled == Bool.True and cleared.apu.ch1.enabled == Bool.False
}

# Enabling length in the first half of the length period clocks it once
expect {
	f = fresh({})
	m = f.bus.write(0xFF16, 0x3E).write(0xFF17, 0xF0).write(0xFF19, 0x80) # counter 2, no length enable
	r = f.apu.tick(m, 4)
	synced = r.apu.tick(r.bus, 8192) # step 0 fires: next step won't clock lengths
	en = synced.apu.tick(synced.bus.write(0xFF19, 0x40), 4) # enable without trigger
	synced.apu.ch2.length == 2 and en.apu.ch2.length == 1
}

# A first-half trigger reloads an expired counter to max-1
expect {
	f = fresh({})
	m = f.bus.write(0xFF17, 0xF0)
	r = f.apu.tick(m, 4)
	synced = r.apu.tick(r.bus, 8192) # step 0 fires
	trig = synced.apu.tick(synced.bus.write(0xFF19, 0xC0), 4)
	trig.apu.ch2.length == 63
}

# DMG: length counters survive a power cycle
expect {
	f = fresh({})
	m = f.bus.write(0xFF16, 0x30).write(0xFF17, 0xF0).write(0xFF19, 0x80) # counter 16
	r = f.apu.tick(m, 4)
	off = r.apu.tick(r.bus.write(0xFF26, 0x00), 4)
	on = off.apu.tick(off.bus.write(0xFF26, 0x80), 4)
	on.apu.ch2.length == 16
}

# CGB: power off clears length counters
expect {
	m0 = Bus.init(test_rom.set(0x0143, 0x80) ?? test_rom)
	m = m0.write(0xFF16, 0x30).write(0xFF17, 0xF0).write(0xFF19, 0x80)
	r = Apu.init({}).tick(m, 4)
	off = r.apu.tick(r.bus.write(0xFF26, 0x00), 4)
	r.apu.ch2.length == 16 and off.apu.ch2.length == 0
}

# Power-on restarts the frame sequencer at step 0
expect {
	f = fresh({})
	r = f.apu.tick(f.bus, 12288) # one step fired, timer mid-flight
	cycled = r.apu.tick(r.bus.write(0xFF26, 0x00).write(0xFF26, 0x80), 4)
	cycled.apu.fs_step == 7 and cycled.apu.fs_timer <= 8
}
