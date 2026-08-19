# NRxx register formats: pure byte -> meaning decoders, the audio
# equivalent of Cpu/Register/Status's Delta — the register map's notation
# captured in types, so channel code reads like the reference material.
# The map itself (which address holds which register) stays in Apu.roc;
# nothing here touches the Bus.
# https://gbdev.io/pandocs/Audio_Registers.html

Register :: [].{
    # NRx2 / NR12-NR42: envelope — volume in the high nibble, direction bit 3,
    # period in the low three bits
    envelope : U8 -> { volume : U8, increase : Bool, period : U8 }
    envelope = |nrx2| {
        volume: nrx2.shr_zf_wrap(4),
        increase: nrx2.bitwise_and(0x08) != 0x00,
        period: nrx2.bitwise_and(0x07),
    }

    # The DAC is on while NRx2's top five bits are nonzero (volume or direction)
    dac_on : U8 -> Bool
    dac_on = |nrx2| nrx2.bitwise_and(0xF8) != 0x00

    # NR30: the wave channel's DAC is its own top bit
    wave_dac_on : U8 -> Bool
    wave_dac_on = |nr30| nr30.bitwise_and(0x80) != 0x00

    # NR10: sweep — period bits 4-6, negate bit 3, shift bits 0-2
    sweep : U8 -> { period : U8, negate : Bool, shift : U8 }
    sweep = |nr10| {
        period: nr10.shr_zf_wrap(4).bitwise_and(0x07),
        negate: nr10.bitwise_and(0x08) != 0x00,
        shift: nr10.bitwise_and(0x07),
    }

    # One sweep calculation: shift the shadow frequency and add or subtract
    sweep_next : U16, { period : U8, negate : Bool, shift : U8 } -> U16
    sweep_next = |shadow, sw| {
        delta = shadow.shr_zf_wrap(sw.shift)
        if sw.negate {
            shadow.minus_wrap(delta)
        } else {
            shadow.plus_wrap(delta)
        }
    }

    # NRx3/NRx4: the 11-bit frequency, low byte plus NRx4's low three bits
    frequency : U8, U8 -> U16
    frequency = |lo, hi|
        hi.bitwise_and(0x07).to_u16().shl_wrap(8).bitwise_or(lo.to_u16())

    pulse_period : U16 -> U64
    pulse_period = |freq| U16.minus(2048, freq).to_u64().shl_wrap(2)

    wave_period : U16 -> U64
    wave_period = |freq| U16.minus(2048, freq).to_u64().shl_wrap(1)

    # NR43: divisor code in the low three bits (0 means 8), shifted left by the
    # clock shift in the high nibble
    noise_period : U8 -> U64
    noise_period = |nr43| {
        divisor =
            match nr43.bitwise_and(0x07) {
                0 => 8
                d => d.to_u64().shl_wrap(4)
            }
        divisor.shl_wrap(nr43.shr_zf_wrap(4))
    }

    noise_width7 : U8 -> Bool
    noise_width7 = |nr43| nr43.bitwise_and(0x08) != 0x00

    # NRx4 bits: trigger (7) and length-enable (6)
    trigger : U8 -> Bool
    trigger = |nrx4| nrx4.bitwise_and(0x80) != 0x00

    length_enabled : U8 -> Bool
    length_enabled = |nrx4| nrx4.bitwise_and(0x40) != 0x00

    # NRx1: the length counter loads as 64 minus the low six bits; the wave
    # channel's NR31 uses the full byte against 256
    len64 : U8 -> U16
    len64 = |nrx1| U16.minus(64, nrx1.bitwise_and(0x3F).to_u16())

    len256 : U8 -> U16
    len256 = |nr31| U16.minus(256, nr31.to_u16())

    # NRx1 bits 6-7: duty index into the four patterns, as bit masks read at
    # duty_pos (LSB first): 12.5%, 25%, 50%, 75%
    duty : U8 -> U8
    duty = |nrx1| nrx1.shr_zf_wrap(6)

    pulse_wave : U8, U8 -> Bool
    pulse_wave = |duty_index, pos| {
        pattern : U8
        pattern =
            match duty_index {
                0 => 0x01
                1 => 0x03
                2 => 0x0F
                _ => 0xFC
            }
        pattern.bitwise_and(U8.shl_wrap(1, pos)) != 0x00
    }

    # NR32 bits 5-6: wave output level as a nibble shift
    wave_volume : U8 -> [Mute, Shift(U8)]
    wave_volume = |nr32|
        match nr32.shr_zf_wrap(5).bitwise_and(0x03) {
            0 => Mute
            1 => Shift(0)
            2 => Shift(1)
            _ => Shift(2)
        }

    # NR51: channel-to-terminal routing, one bit per channel per side
    routed : U8, U8 -> Bool
    routed = |nr51, bit| nr51.bitwise_and(U8.shl_wrap(1, bit)) != 0x00

    # NR50: master volume per terminal, 1/8 to 8/8
    master_volume : U8 -> { left : F32, right : F32 }
    master_volume = |nr50| {
        left: (nr50.shr_zf_wrap(4).bitwise_and(0x07).to_f32() + 1.0) / 8.0,
        right: (nr50.bitwise_and(0x07).to_f32() + 1.0) / 8.0,
    }
}

# --- expects ---

# Envelope: volume 15, decrease, period 3 from 0xF3 (the DMG boot NR12)
expect Register.envelope(0xF3) == { volume: 15, increase: Bool.False, period: 3 }
expect Register.envelope(0x08) == { volume: 0, increase: Bool.True, period: 0 }
expect Register.dac_on(0xF3) and Register.dac_on(0x08) and Register.dac_on(0x10) and Register.dac_on(0x07) == Bool.False

# Sweep: period 7, negate, shift 5 from 0x7D
expect Register.sweep(0x7D) == { period: 7, negate: Bool.True, shift: 5 }
expect Register.sweep_next(0x0100, { period: 1, negate: Bool.False, shift: 1 }) == 0x0180
expect Register.sweep_next(0x0100, { period: 1, negate: Bool.True, shift: 1 }) == 0x0080

# Frequency: 11 bits reassemble; only NRx4's low three bits contribute
expect Register.frequency(0xFF, 0xC7) == 0x07FF
expect Register.frequency(0x00, 0x04) == 0x0400
expect Register.pulse_period(0x07FF) == 4 and Register.wave_period(0x0700) == 512

# NR43: divisor code 0 means 8; shift multiplies by powers of two
expect Register.noise_period(0x00) == 8
expect Register.noise_period(0x01) == 16
expect Register.noise_period(0x11) == 32
expect Register.noise_width7(0x08) and Register.noise_width7(0x00) == Bool.False

# Duty ratios: high for 1, 2, 4, 6 of 8 steps
expect {
    highs = |d| [0, 1, 2, 3, 4, 5, 6, 7].fold(0, |n, pos| if Register.pulse_wave(d, pos) { n.plus(1) } else { n })
    highs(0) == 1 and highs(1) == 2 and highs(2) == 4 and highs(3) == 6
}

# Length loads: NRx1 counts down from 64, NR31 from 256
expect Register.len64(0x3F) == 1 and Register.len64(0x00) == 64
expect Register.len256(0xFF) == 1 and Register.len256(0x00) == 256

# Wave volume: mute, full, half, quarter
expect Register.wave_volume(0x00) == Mute and Register.wave_volume(0x20) == Shift(0)
expect Register.wave_volume(0x40) == Shift(1) and Register.wave_volume(0x60) == Shift(2)
