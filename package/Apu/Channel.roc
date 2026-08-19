# One APU channel's hidden state — what the hardware keeps outside the
# memory map: waveform timers, the duty walker, the LFSR, sweep shadow,
# and the envelope/length counters. The audio parallel of Cpu/Register:
# state the bus cannot reach gets a module to own it. One record shape
# serves all four channels; unused fields stay idle. Everything here is
# pure over the channel plus scalar arguments — register decoding happens
# in Apu/Register, bus traffic in Apu.
# https://gbdev.io/pandocs/Audio_details.html

import /Apu/Register

Channel := {
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

    clock_envelope : Channel, { volume : U8, increase : Bool, period : U8 } -> Channel
    clock_envelope = |ch, env|
        if env.period == 0 or ch.enabled == Bool.False {
            ch
        } else if ch.env_timer <= 1 {
            volume =
                if env.increase {
                    if ch.volume < 15 { ch.volume.plus(1) } else { ch.volume }
                } else {
                    if ch.volume > 0 { ch.volume.minus(1) } else { ch.volume }
                }
            { ..ch, env_timer: env.period, volume: volume }
        } else {
            { ..ch, env_timer: ch.env_timer.minus(1) }
        }

    # NRx2/NR30: turning the DAC off silences the channel immediately
    dac_gate : Channel, Bool -> Channel
    dac_gate = |ch, on| if on { ch } else { { ..ch, enabled: Bool.False } }

    pulse_out : Channel, U8 -> U8
    pulse_out = |ch, duty_index|
        if ch.enabled and Register.pulse_wave(duty_index, ch.duty_pos) {
            ch.volume
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
}

# LFSR: documented first steps from all-ones, and 7-bit mode feedback
expect Channel.clock_lfsr(0x7FFF, Bool.False) == 0x3FFF
expect Channel.clock_lfsr(0x3FFF, Bool.False) == 0x1FFF
expect Channel.clock_lfsr(0x0001, Bool.False) == 0x4000
expect Channel.clock_lfsr(0x7FFF, Bool.True) == 0x3FBF
