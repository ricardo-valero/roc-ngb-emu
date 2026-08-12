# DMG audio processing unit: four channels driven by a 512 Hz frame
# sequencer, ticked from CPU-step cycles like the timer and PPU. Registers
# live in the bus (with read-back masks applied there); this record holds the
# hidden state: timers, counters, LFSR, and the generated sample buffer.
# https://gbdev.io/pandocs/Audio_details.html

import /Mmu

# One record shape serves all four channels; unused fields stay idle
Channel : {
    enabled : Bool,
    length : U16,
    timer : U64, # cycles until the next waveform step
    duty_pos : U8,
    volume : U8,
    env_timer : U8,
    sweep_timer : U8,
    sweep_shadow : U16,
    sweep_enabled : Bool,
    wave_pos : U8,
    lfsr : U16,
    len_en : Bool, # last seen NRx4 length-enable, for edge-clock detection
    sweep_neg_used : Bool, # a sweep calc used negate mode since the last trigger
}

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
    blank : {} -> Channel
    blank = |_| {
        enabled: Bool.False,
        length: 0,
        timer: 8192,
        duty_pos: 0,
        volume: 0,
        env_timer: 0,
        sweep_timer: 0,
        sweep_shadow: 0,
        sweep_enabled: Bool.False,
        wave_pos: 0,
        lfsr: 0x7FFF,
        len_en: Bool.False,
        sweep_neg_used: Bool.False,
    }

    init : {} -> Apu
    init = |_| {
        apu : Apu
        apu = {
            ch1: blank({}),
            ch2: blank({}),
            ch3: blank({}),
            ch4: blank({}),
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
    tick : Apu, Mmu, U64 -> { apu : Apu, mmu : Mmu }
    tick = |apu, mmu, cycles| {
        if mmu.apu_events.len() > 0 {
            # the writes happened during this step: advance the batched time
            # first so the quirks observe the frame sequencer as of the write
            pre = advance({ ..apu, pending: 0 }, mmu, apu.pending)
            applied = apply_events(pre.apu, pre.mmu)
            advance(applied.apu, applied.mmu, cycles)
        } else {
            total = apu.pending.plus(cycles)
            # flush when the batch crosses a frame-sequencer step so NR52
            # status updates within an instruction of a length clock — test
            # ROMs poll NR52 to sync against the sequencer phase
            if total >= 128 or apu.fs_timer.plus(total) >= 8192 {
                advance({ ..apu, pending: 0 }, mmu, total)
            } else {
                { apu: { ..apu, pending: total }, mmu: mmu }
            }
        }
    }

    apply_events : Apu, Mmu -> { apu : Apu, mmu : Mmu }
    apply_events = |apu0, mmu0| {
        drained = mmu0.take_apu_events()
        events = drained.events
        mmu = drained.mmu # last use of `drained`: the bus stays uniquely owned
        var apu = apu0
        var i = 0
        while i < events.len() {
            apu = handle_event(apu, mmu, events.get(i) ?? 0xFF)
            i = i.plus(1)
        }
        { apu: apu, mmu: mmu }
    }

    advance : Apu, Mmu, U64 -> { apu : Apu, mmu : Mmu }
    advance = |apu0, mmu0, cycles| {
        var mmu = mmu0
        var apu = apu0

        # waveform timers
        apu = { ..apu,
            ch1: advance_pulse(apu.ch1, pulse_period(mmu, 0xFF13, 0xFF14), cycles),
            ch2: advance_pulse(apu.ch2, pulse_period(mmu, 0xFF18, 0xFF19), cycles),
            ch3: advance_wave(apu.ch3, wave_period(mmu), cycles),
            ch4: advance_noise(apu.ch4, noise_period(mmu), noise_width7(mmu), cycles),
        }

        # frame sequencer
        var fs = apu.fs_timer.plus(cycles)
        while fs >= 8192 {
            fs = fs.minus(8192)
            step = apu.fs_step.plus(1).bitwise_and(0x07)
            apu = { ..apu, fs_step: step }
            if step.bitwise_and(0x01) == 0x00 {
                apu = clock_lengths(apu, mmu)
            } else {
                {}
            }
            if step == 2 or step == 6 {
                s = clock_sweep(apu, mmu)
                apu = s.apu
                mmu = s.mmu
            } else {
                {}
            }
            if step == 7 {
                apu = clock_envelopes(apu, mmu)
            } else {
                {}
            }
        }
        apu = { ..apu, fs_timer: fs }

        # NR52 status bits (before the sample block: it consumes `apu`)
        mmu2 =
            if mmu.apu_powered() {
                mmu.poke(0xFF26, status_byte(apu, mmu))
            } else {
                mmu
            }

        # sample emission: all samples within one advance share the end-of-tick
        # state, so mix once and push into the bus-side buffer (the bus stays
        # uniquely owned through the step pipeline, so appends are in-place)
        pair = mix(apu, mmu2)
        var acc = apu.sample_acc.plus(cycles * 48000)
        var mmu3 = mmu2
        while acc >= 4194304 {
            acc = acc.minus(4194304)
            mmu3 = mmu3.push_sample_pair(pair.left, pair.right)
        }
        { apu: { ..apu, sample_acc: acc }, mmu: mmu3 }
    }

    status_byte : Apu, Mmu -> U8
    status_byte = |apu, mmu| {
        power = (mmu.read_raw(0xFF26)).bitwise_and(0x80)
        power
            .bitwise_or(if apu.ch1.enabled { 0x01 } else { 0x00 })
            .bitwise_or(if apu.ch2.enabled { 0x02 } else { 0x00 })
            .bitwise_or(if apu.ch3.enabled { 0x04 } else { 0x00 })
            .bitwise_or(if apu.ch4.enabled { 0x08 } else { 0x00 })
    }

    # --- events ---

    # Events are the low byte of the written register's address (see
    # Mmu.apu_reg_event), plus 0xF0/0xF1 for power off/on
    handle_event : Apu, Mmu, U8 -> Apu
    handle_event = |apu, mmu, event|
        match event {
            0x10 => { ..apu, ch1: nr10_gate(apu.ch1, mmu) }
            0x11 => { ..apu, ch1: { ..apu.ch1, length: len64(mmu, 0xFF11) } }
            0x12 => { ..apu, ch1: dac_gate(apu.ch1, dac_on(mmu, 0xFF12)) }
            0x14 => { ..apu, ch1: nrx4(apu, apu.ch1, mmu, 0xFF14, 64, Ch1) }
            0x16 => { ..apu, ch2: { ..apu.ch2, length: len64(mmu, 0xFF16) } }
            0x17 => { ..apu, ch2: dac_gate(apu.ch2, dac_on(mmu, 0xFF17)) }
            0x19 => { ..apu, ch2: nrx4(apu, apu.ch2, mmu, 0xFF19, 64, Ch2) }
            0x1A => { ..apu, ch3: dac_gate(apu.ch3, wave_dac_on(mmu)) }
            0x1B => { ..apu, ch3: { ..apu.ch3, length: U16.minus(256, mmu.read_raw(0xFF1B).to_u16()) } }
            0x1E => { ..apu, ch3: nrx4(apu, apu.ch3, mmu, 0xFF1E, 256, Wave) }
            0x20 => { ..apu, ch4: { ..apu.ch4, length: len64(mmu, 0xFF20) } }
            0x21 => { ..apu, ch4: dac_gate(apu.ch4, dac_on(mmu, 0xFF21)) }
            0x23 => { ..apu, ch4: nrx4(apu, apu.ch4, mmu, 0xFF23, 64, Noise) }
            0xF0 => power_off(apu, mmu)
            0xF1 => power_on(apu)
            _ => apu
        }

    dac_on : Mmu, U16 -> Bool
    dac_on = |mmu, nrx2| mmu.read_raw(nrx2).bitwise_and(0xF8) != 0x00

    wave_dac_on : Mmu -> Bool
    wave_dac_on = |mmu| mmu.read_raw(0xFF1A).bitwise_and(0x80) != 0x00

    # NRx1: the length counter loads at write time, playing or not
    len64 : Mmu, U16 -> U16
    len64 = |mmu, nrx1| U16.minus(64, mmu.read_raw(nrx1).bitwise_and(0x3F).to_u16())

    # NRx2/NR30: turning the DAC off silences the channel immediately
    dac_gate : Channel, Bool -> Channel
    dac_gate = |ch, on| if on { ch } else { { ..ch, enabled: Bool.False } }

    # NR10: clearing negate after a negate-mode calculation kills CH1
    nr10_gate : Channel, Mmu -> Channel
    nr10_gate = |ch, mmu|
        if mmu.read_raw(0xFF10).bitwise_and(0x08) == 0x00 and ch.sweep_neg_used {
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
    nrx4 : Apu, Channel, Mmu, U16, U16, [Ch1, Ch2, Wave, Noise] -> Channel
    nrx4 = |apu, ch0, mmu, addr, max_len, which| {
        v = mmu.read_raw(addr)
        new_en = v.bitwise_and(0x40) != 0x00
        trigger = v.bitwise_and(0x80) != 0x00
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
                        Ch1 => trigger_ch1(edge, mmu)
                        Ch2 => trigger_ch2(edge, mmu)
                        Wave => trigger_wave(edge, mmu)
                        Noise => trigger_noise(edge, mmu)
                    }
                if t.length == 0 {
                    { ..t, length: if new_en and fh { max_len.minus(1) } else { max_len } }
                } else {
                    t
                }
            } else {
                edge
            }
        { ..done, len_en: new_en }
    }

    # Power off clears channel state; on DMG the length counters survive
    power_off : Apu, Mmu -> Apu
    power_off = |apu, mmu|
        if mmu.is_cgb() {
            { ..apu, ch1: blank({}), ch2: blank({}), ch3: blank({}), ch4: blank({}) }
        } else {
            { ..apu,
                ch1: { ..blank({}), length: apu.ch1.length },
                ch2: { ..blank({}), length: apu.ch2.length },
                ch3: { ..blank({}), length: apu.ch3.length },
                ch4: { ..blank({}), length: apu.ch4.length },
            }
        }

    # Power on restarts the frame sequencer at step 0 and rewinds waveforms
    power_on : Apu -> Apu
    power_on = |apu| { ..apu,
        fs_step: 7,
        fs_timer: 0,
        ch1: { ..apu.ch1, duty_pos: 0 },
        ch2: { ..apu.ch2, duty_pos: 0 },
        ch3: { ..apu.ch3, wave_pos: 0 },
    }

    trigger_pulse : Channel, Mmu, U16, U64 -> Channel
    trigger_pulse = |ch, mmu, nrx2, period| {
        env = mmu.read_raw(nrx2)
        { ..ch,
            enabled: dac_on(mmu, nrx2),
            timer: period,
            volume: env.shr_zf_wrap(4),
            env_timer: env.bitwise_and(0x07),
        }
    }

    trigger_ch2 : Channel, Mmu -> Channel
    trigger_ch2 = |ch, mmu| trigger_pulse(ch, mmu, 0xFF17, pulse_period(mmu, 0xFF18, 0xFF19))

    trigger_ch1 : Channel, Mmu -> Channel
    trigger_ch1 = |ch0, mmu| {
        ch = trigger_pulse(ch0, mmu, 0xFF12, pulse_period(mmu, 0xFF13, 0xFF14))
        nr10 = mmu.read_raw(0xFF10)
        period = nr10.shr_zf_wrap(4).bitwise_and(0x07)
        shift = nr10.bitwise_and(0x07)
        shadow = raw_frequency(mmu, 0xFF13, 0xFF14)
        armed = { ..ch,
            sweep_shadow: shadow,
            sweep_timer: if period == 0 { 8 } else { period },
            sweep_enabled: period != 0 or shift != 0,
            sweep_neg_used: Bool.False,
        }
        # immediate calculation when a shift is set (counts for the negate quirk)
        if shift != 0 {
            calc = { ..armed, sweep_neg_used: nr10.bitwise_and(0x08) != 0x00 }
            if sweep_next(calc.sweep_shadow, nr10) > 2047 {
                { ..calc, enabled: Bool.False }
            } else {
                calc
            }
        } else {
            armed
        }
    }

    trigger_wave : Channel, Mmu -> Channel
    trigger_wave = |ch, mmu| { ..ch,
        enabled: wave_dac_on(mmu),
        timer: wave_period(mmu),
        wave_pos: 0,
    }

    trigger_noise : Channel, Mmu -> Channel
    trigger_noise = |ch, mmu| {
        env = mmu.read_raw(0xFF21)
        { ..ch,
            enabled: dac_on(mmu, 0xFF21),
            timer: noise_period(mmu),
            volume: env.shr_zf_wrap(4),
            env_timer: env.bitwise_and(0x07),
            lfsr: 0x7FFF,
        }
    }

    # --- waveform timing ---

    raw_frequency : Mmu, U16, U16 -> U16
    raw_frequency = |mmu, lo, hi|
        mmu.read_raw(hi).bitwise_and(0x07).to_u16().shl_wrap(8).bitwise_or(mmu.read_raw(lo).to_u16())

    pulse_period : Mmu, U16, U16 -> U64
    pulse_period = |mmu, lo, hi|
        U16.minus(2048, raw_frequency(mmu, lo, hi)).to_u64().shl_wrap(2)

    wave_period : Mmu -> U64
    wave_period = |mmu|
        U16.minus(2048, raw_frequency(mmu, 0xFF1D, 0xFF1E)).to_u64().shl_wrap(1)

    noise_period : Mmu -> U64
    noise_period = |mmu| {
        nr43 = mmu.read_raw(0xFF22)
        divisor =
            match nr43.bitwise_and(0x07) {
                0 => 8
                d => d.to_u64().shl_wrap(4)
            }
        divisor.shl_wrap(nr43.shr_zf_wrap(4))
    }

    noise_width7 : Mmu -> Bool
    noise_width7 = |mmu| mmu.read_raw(0xFF22).bitwise_and(0x08) != 0x00

    advance_pulse : Channel, U64, U64 -> Channel
    advance_pulse = |ch0, period, cycles| {
        var ch = ch0
        var rem = cycles
        while rem >= ch.timer {
            rem = rem.minus(ch.timer)
            ch = { ..ch, timer: period, duty_pos: ch.duty_pos.plus(1).bitwise_and(0x07) }
        }
        { ..ch, timer: ch.timer.minus(rem) }
    }

    advance_wave : Channel, U64, U64 -> Channel
    advance_wave = |ch0, period, cycles| {
        var ch = ch0
        var rem = cycles
        while rem >= ch.timer {
            rem = rem.minus(ch.timer)
            ch = { ..ch, timer: period, wave_pos: ch.wave_pos.plus(1).bitwise_and(0x1F) }
        }
        { ..ch, timer: ch.timer.minus(rem) }
    }

    advance_noise : Channel, U64, Bool, U64 -> Channel
    advance_noise = |ch0, period, width7, cycles| {
        var ch = ch0
        var rem = cycles
        while rem >= ch.timer {
            rem = rem.minus(ch.timer)
            ch = { ..ch, timer: period, lfsr: clock_lfsr(ch.lfsr, width7) }
        }
        { ..ch, timer: ch.timer.minus(rem) }
    }

    clock_lfsr : U16, Bool -> U16
    clock_lfsr = |lfsr, width7| {
        bit = lfsr.bitwise_xor(lfsr.shr_zf_wrap(1)).bitwise_and(0x0001)
        next = lfsr.shr_zf_wrap(1).bitwise_or(bit.shl_wrap(14))
        if width7 {
            next.bitwise_and(0xFFBF).bitwise_or(bit.shl_wrap(6))
        } else {
            next
        }
    }

    # --- frame sequencer clocks ---

    length_enabled : Mmu, U16 -> Bool
    length_enabled = |mmu, addr| mmu.read_raw(addr).bitwise_and(0x40) != 0x00

    clock_length : Channel, Bool -> Channel
    clock_length = |ch, enable|
        if enable and ch.length > 0 {
            remaining = ch.length.minus(1)
            if remaining == 0 {
                { ..ch, length: 0, enabled: Bool.False }
            } else {
                { ..ch, length: remaining }
            }
        } else {
            ch
        }

    clock_lengths : Apu, Mmu -> Apu
    clock_lengths = |apu, mmu| { ..apu,
        ch1: clock_length(apu.ch1, length_enabled(mmu, 0xFF14)),
        ch2: clock_length(apu.ch2, length_enabled(mmu, 0xFF19)),
        ch3: clock_length(apu.ch3, length_enabled(mmu, 0xFF1E)),
        ch4: clock_length(apu.ch4, length_enabled(mmu, 0xFF23)),
    }

    clock_envelope : Channel, U8 -> Channel
    clock_envelope = |ch, nrx2| {
        period = nrx2.bitwise_and(0x07)
        if period == 0 or ch.enabled == Bool.False {
            ch
        } else if ch.env_timer <= 1 {
            increase = nrx2.bitwise_and(0x08) != 0x00
            volume =
                if increase {
                    if ch.volume < 15 { ch.volume.plus(1) } else { ch.volume }
                } else {
                    if ch.volume > 0 { ch.volume.minus(1) } else { ch.volume }
                }
            { ..ch, env_timer: period, volume: volume }
        } else {
            { ..ch, env_timer: ch.env_timer.minus(1) }
        }
    }

    clock_envelopes : Apu, Mmu -> Apu
    clock_envelopes = |apu, mmu| { ..apu,
        ch1: clock_envelope(apu.ch1, mmu.read_raw(0xFF12)),
        ch2: clock_envelope(apu.ch2, mmu.read_raw(0xFF17)),
        ch4: clock_envelope(apu.ch4, mmu.read_raw(0xFF21)),
    }

    sweep_next : U16, U8 -> U16
    sweep_next = |shadow, nr10| {
        delta = shadow.shr_zf_wrap(nr10.bitwise_and(0x07))
        if nr10.bitwise_and(0x08) != 0x00 {
            shadow.minus_wrap(delta)
        } else {
            shadow.plus_wrap(delta)
        }
    }

    clock_sweep : Apu, Mmu -> { apu : Apu, mmu : Mmu }
    clock_sweep = |apu, mmu| {
        ch = apu.ch1
        nr10 = mmu.read_raw(0xFF10)
        period = nr10.shr_zf_wrap(4).bitwise_and(0x07)
        shift = nr10.bitwise_and(0x07)
        if ch.sweep_timer > 1 {
            { apu: { ..apu, ch1: { ..ch, sweep_timer: ch.sweep_timer.minus(1) } }, mmu: mmu }
        } else {
            reloaded = { ..ch, sweep_timer: if period == 0 { 8 } else { period } }
            if reloaded.sweep_enabled and period != 0 {
                # any calculation in negate mode arms the NR10 negate-clear quirk
                calc = { ..reloaded, sweep_neg_used: reloaded.sweep_neg_used or nr10.bitwise_and(0x08) != 0x00 }
                next = sweep_next(calc.sweep_shadow, nr10)
                if next > 2047 {
                    { apu: { ..apu, ch1: { ..calc, enabled: Bool.False } }, mmu: mmu }
                } else if shift != 0 {
                    mmu2 = mmu
                        .poke(0xFF13, next.to_u8_wrap())
                        .poke(0xFF14, mmu.read_raw(0xFF14).bitwise_and(0xF8).bitwise_or(next.shr_zf_wrap(8).to_u8_wrap()))
                    updated = { ..calc, sweep_shadow: next }
                    if sweep_next(next, nr10) > 2047 {
                        { apu: { ..apu, ch1: { ..updated, enabled: Bool.False } }, mmu: mmu2 }
                    } else {
                        { apu: { ..apu, ch1: updated }, mmu: mmu2 }
                    }
                } else {
                    { apu: { ..apu, ch1: calc }, mmu: mmu }
                }
            } else {
                { apu: { ..apu, ch1: reloaded }, mmu: mmu }
            }
        }
    }

    # --- output ---

    # Duty patterns as bit masks read at duty_pos (LSB first):
    # 12.5%, 25%, 50%, 75%
    pulse_wave : U8, U8 -> Bool
    pulse_wave = |duty, pos| {
        pattern : U8
        pattern =
            match duty {
                0 => 0x01
                1 => 0x03
                2 => 0x0F
                _ => 0xFC
            }
        pattern.bitwise_and(U8.shl_wrap(1, pos)) != 0x00
    }

    pulse_out : Channel, U8 -> U8
    pulse_out = |ch, nrx1|
        if ch.enabled and pulse_wave(nrx1.shr_zf_wrap(6), ch.duty_pos) {
            ch.volume
        } else {
            0
        }

    wave_out : Channel, Mmu -> U8
    wave_out = |ch, mmu|
        if ch.enabled {
            byte = mmu.read_raw(U16.plus(0xFF30, ch.wave_pos.shr_zf_wrap(1).to_u16()))
            nibble =
                if ch.wave_pos.bitwise_and(0x01) == 0x00 {
                    byte.shr_zf_wrap(4)
                } else {
                    byte.bitwise_and(0x0F)
                }
            match mmu.read_raw(0xFF1C).shr_zf_wrap(5).bitwise_and(0x03) {
                0 => 0
                1 => nibble
                2 => nibble.shr_zf_wrap(1)
                _ => nibble.shr_zf_wrap(2)
            }
        } else {
            0
        }

    noise_out : Channel -> U8
    noise_out = |ch|
        if ch.enabled and ch.lfsr.bitwise_and(0x0001) == 0x0000 {
            ch.volume
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

    mix : Apu, Mmu -> { left : F32, right : F32 }
    mix = |apu, mmu| {
        o1 = dac(pulse_out(apu.ch1, mmu.read_raw(0xFF11)), apu.ch1.enabled)
        o2 = dac(pulse_out(apu.ch2, mmu.read_raw(0xFF16)), apu.ch2.enabled)
        o3 = dac(wave_out(apu.ch3, mmu), apu.ch3.enabled)
        o4 = dac(noise_out(apu.ch4), apu.ch4.enabled)
        nr51 = mmu.read_raw(0xFF25)
        nr50 = mmu.read_raw(0xFF24)
        left_vol = (nr50.shr_zf_wrap(4).bitwise_and(0x07).to_f32() + 1.0) / 8.0
        right_vol = (nr50.bitwise_and(0x07).to_f32() + 1.0) / 8.0
        {
            left: (route(o1, nr51, 4) + route(o2, nr51, 5) + route(o3, nr51, 6) + route(o4, nr51, 7)) * left_vol,
            right: (route(o1, nr51, 0) + route(o2, nr51, 1) + route(o3, nr51, 2) + route(o4, nr51, 3)) * right_vol,
        }
    }

    route : F32, U8, U8 -> F32
    route = |sample, nr51, bit|
        if nr51.bitwise_and(U8.shl_wrap(1, bit)) != 0x00 {
            sample
        } else {
            0.0
        }
}

# --- expects ---

test_rom : List(U8)
test_rom = List.repeat(0x00, 0x8000)

fresh : {} -> { apu : Apu, mmu : Mmu }
fresh = |_| { apu: Apu.init({}), mmu: Mmu.init(test_rom) }

# Length expiry silences a channel and clears its NR52 bit
expect {
    f = fresh({})
    m = f.mmu.write(0xFF16, 0x3F).write(0xFF17, 0xF0).write(0xFF19, 0xC0) # len 1, vol 15, trigger+len-enable
    r = f.apu.tick(m, 4)
    after = r.apu.tick(r.mmu, 16384) # at least one 256 Hz length clock
    r.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x02
    and after.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Envelope steps down at 64 Hz
expect {
    f = fresh({})
    m = f.mmu.write(0xFF17, 0xF1).write(0xFF19, 0x80) # vol 15, decrease, period 1
    r = f.apu.tick(m, 4)
    after = r.apu.tick(r.mmu, 8192 * 8) # one full sequencer round: one envelope clock
    r.apu.ch2.volume == 15 and after.apu.ch2.volume == 14
}

# Duty walker advances with the frequency timer (freq 0x7FF: period 4)
expect {
    f = fresh({})
    m = f.mmu.write(0xFF18, 0xFF).write(0xFF17, 0xF0).write(0xFF19, 0x87)
    r = f.apu.tick(m, 0) # drain the trigger only; timer loaded, no steps yet
    after = r.apu.tick(r.mmu, 144) # 36 steps of period 4: duty_pos 36 % 8 = 4
    after.apu.ch2.duty_pos == 4
}

# Duty ratios: 50% pattern is high for 4 of 8 steps; 12.5% for 1
expect {
    highs = |duty| [0, 1, 2, 3, 4, 5, 6, 7].fold(0, |n, pos| if Apu.pulse_wave(duty, pos) { n.plus(1) } else { n })
    highs(0) == 1 and highs(1) == 2 and highs(2) == 4 and highs(3) == 6
}

# Sweep overflow disables CH1 at trigger
expect {
    f = fresh({})
    m = f.mmu.write(0xFF10, 0x01).write(0xFF12, 0xF0).write(0xFF13, 0xFF).write(0xFF14, 0x87)
    r = f.apu.tick(m, 4)
    r.apu.ch1.enabled == Bool.False and r.mmu.read_raw(0xFF26).bitwise_and(0x01) == 0x00
}

# Sweep with room keeps the channel alive
expect {
    f = fresh({})
    m = f.mmu.write(0xFF10, 0x11).write(0xFF12, 0xF0).write(0xFF13, 0x00).write(0xFF14, 0x84)
    r = f.apu.tick(m, 4)
    r.apu.ch1.enabled == Bool.True
}

# LFSR: documented first steps from all-ones, and 7-bit mode feedback
expect Apu.clock_lfsr(0x7FFF, Bool.False) == 0x3FFF
expect Apu.clock_lfsr(0x3FFF, Bool.False) == 0x1FFF
expect Apu.clock_lfsr(0x0001, Bool.False) == 0x4000
expect Apu.clock_lfsr(0x7FFF, Bool.True) == 0x3FBF

# Wave channel follows wave RAM nibbles
expect {
    f = fresh({})
    m = f.mmu
        .write(0xFF30, 0xF0) # nibble 0 = 15, nibble 1 = 0
        .write(0xFF1A, 0x80) # DAC on
        .write(0xFF1C, 0x20) # volume 100%
        .write(0xFF1D, 0x00)
        .write(0xFF1E, 0x87) # trigger, freq 0x700: period (2048-1792)*2 = 512
    r = f.apu.tick(m, 4)
    w0 = Apu.wave_out(r.apu.ch3, r.mmu)
    stepped = r.apu.tick(r.mmu, 512)
    w1 = Apu.wave_out(stepped.apu.ch3, stepped.mmu)
    w0 == 15 and w1 == 0
}

# Panning: CH2 routed left only produces silent right samples
expect {
    f = fresh({})
    m = f.mmu
        .write(0xFF25, 0x20) # CH2 left only
        .write(0xFF16, 0x00) # duty 0: high at pos 0 right after trigger
        .write(0xFF17, 0xF0)
        .write(0xFF18, 0x00)
        .write(0xFF19, 0x80) # trigger, freq 0: period 8192
    r = f.apu.tick(m, 4)
    out = r.apu.tick(r.mmu, 128) # one batched advance: exactly one stereo pair
    samples = out.mmu.take_samples().samples
    samples.len() == 2 and (samples.get(0) ?? 0.0) > 0.0 and (samples.get(1) ?? 0.0) == 0.0
}

# One video frame yields ~800 stereo pairs at 48 kHz
expect {
    f = fresh({})
    r = f.apu.tick(f.mmu, 70224)
    n = r.mmu.take_samples().samples.len()
    n >= 1600 and n <= 1610
}

# Power off blanks every channel
expect {
    f = fresh({})
    m = f.mmu.write(0xFF17, 0xF0).write(0xFF19, 0x80)
    r = f.apu.tick(m, 4)
    off = r.apu.tick(r.mmu.write(0xFF26, 0x00), 4)
    r.apu.ch2.enabled == Bool.True and off.apu.ch2.enabled == Bool.False
}

# NRx1 reloads the length counter mid-note: a rewritten short length expires
expect {
    f = fresh({})
    m = f.mmu.write(0xFF17, 0xF0).write(0xFF19, 0xC0) # trigger with length enabled: counter 64
    r = f.apu.tick(m, 4)
    r2 = r.apu.tick(r.mmu.write(0xFF16, 0x3F), 4) # rewrite mid-note: counter 1
    after = r2.apu.tick(r2.mmu, 16384)
    r2.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x02
    and after.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Turning the DAC off silences the channel without waiting for a length clock
expect {
    f = fresh({})
    m = f.mmu.write(0xFF17, 0xF0).write(0xFF19, 0x80)
    r = f.apu.tick(m, 4)
    off = r.apu.tick(r.mmu.write(0xFF17, 0x00), 4)
    r.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x02
    and off.mmu.read_raw(0xFF26).bitwise_and(0x02) == 0x00
}

# Clearing sweep negate after a negate-mode calculation disables CH1
expect {
    f = fresh({})
    m = f.mmu.write(0xFF10, 0x09).write(0xFF12, 0xF0).write(0xFF13, 0x40).write(0xFF14, 0x84)
    r = f.apu.tick(m, 4) # trigger runs an immediate negate-mode calculation
    cleared = r.apu.tick(r.mmu.write(0xFF10, 0x01), 4)
    r.apu.ch1.enabled == Bool.True and cleared.apu.ch1.enabled == Bool.False
}

# Enabling length in the first half of the length period clocks it once
expect {
    f = fresh({})
    m = f.mmu.write(0xFF16, 0x3E).write(0xFF17, 0xF0).write(0xFF19, 0x80) # counter 2, no length enable
    r = f.apu.tick(m, 4)
    synced = r.apu.tick(r.mmu, 8192) # step 0 fires: next step won't clock lengths
    en = synced.apu.tick(synced.mmu.write(0xFF19, 0x40), 4) # enable without trigger
    synced.apu.ch2.length == 2 and en.apu.ch2.length == 1
}

# A first-half trigger reloads an expired counter to max-1
expect {
    f = fresh({})
    m = f.mmu.write(0xFF17, 0xF0)
    r = f.apu.tick(m, 4)
    synced = r.apu.tick(r.mmu, 8192) # step 0 fires
    trig = synced.apu.tick(synced.mmu.write(0xFF19, 0xC0), 4)
    trig.apu.ch2.length == 63
}

# DMG: length counters survive a power cycle
expect {
    f = fresh({})
    m = f.mmu.write(0xFF16, 0x30).write(0xFF17, 0xF0).write(0xFF19, 0x80) # counter 16
    r = f.apu.tick(m, 4)
    off = r.apu.tick(r.mmu.write(0xFF26, 0x00), 4)
    on = off.apu.tick(off.mmu.write(0xFF26, 0x80), 4)
    on.apu.ch2.length == 16
}

# CGB: power off clears length counters
expect {
    m0 = Mmu.init(test_rom.set(0x0143, 0x80) ?? test_rom)
    m = m0.write(0xFF16, 0x30).write(0xFF17, 0xF0).write(0xFF19, 0x80)
    r = Apu.init({}).tick(m, 4)
    off = r.apu.tick(r.mmu.write(0xFF26, 0x00), 4)
    r.apu.ch2.length == 16 and off.apu.ch2.length == 0
}

# Power-on restarts the frame sequencer at step 0
expect {
    f = fresh({})
    r = f.apu.tick(f.mmu, 12288) # one step fired, timer mid-flight
    cycled = r.apu.tick(r.mmu.write(0xFF26, 0x00).write(0xFF26, 0x80), 4)
    cycled.apu.fs_step == 7 and cycled.apu.fs_timer <= 8
}
