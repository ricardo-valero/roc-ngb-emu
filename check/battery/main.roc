app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	ngb: "../../package/main.roc",
}

import pf.OsStr
import pf.Stdout
import ngb.GameBoy

# Battery persistence check: `.sav` round trips, the MBC3 RTC footer
# (48-byte written, 44-byte legacy accepted), wall-clock catch-up on
# load, halt semantics, and the save-event counter — all against
# synthetic carts with fixed `now` values, so the whole slice is
# deterministic and needs no ROMs or network.
#
#   roc check/battery/main.roc

set_byte : List(U8), U64, U8 -> List(U8)
set_byte = |l, i, v| l.set(i, v) ?? l

# MBC1+Ram+Battery (0x03), 8 KiB declared
battery_rom : {} -> List(U8)
battery_rom = |_| set_byte(set_byte(List.repeat(0x00, 0x8000), 0x0147, 0x03), 0x0149, 0x02)

# MBC3+Timer+Ram+Battery (0x10), 32 KiB declared
rtc_rom : {} -> List(U8)
rtc_rom = |_| set_byte(set_byte(List.repeat(0x00, 0x8000), 0x0147, 0x10), 0x0149, 0x03)

# Supply a wall-clock second through the public per-frame input (the ROM
# is all NOPs, so the frame is inert)
at_time : GameBoy, U64 -> GameBoy
at_time = |gb, now| gb.run_frame({ ..GameBoy.no_input({}), now: now })

# Extract, inject into a fresh machine, extract again: byte-identical
check_ram_roundtrip : {} -> Bool
check_ram_roundtrip = |_| {
	rom = battery_rom({})
	gb = GameBoy.init(rom).poke(0x0000, 0x0A).poke(0xA000, 0x5A).poke(0xA123, 0xC3)
	sav = gb.battery()
	again = GameBoy.init(rom).with_battery(sav).battery()
	sav.len() == 8192 and (sav.get(0) ?? 0x00) == 0x5A and sav == again
}

# RTC carts append the 48-byte footer; the round trip stays identical
check_rtc_roundtrip : {} -> Bool
check_rtc_roundtrip = |_| {
	rom = rtc_rom({})
	gb = at_time(GameBoy.init(rom), 1000).poke(0x0000, 0x0A).poke(0xA000, 0x42)
	sav = gb.battery()
	again = GameBoy.init(rom).with_battery(sav).battery()
	sav.len() == 32816 and sav == again # 32 KiB + 48
}

# A hand-built legacy 44-byte footer (32-bit timestamp 1234, clock at
# 03:02:10 day 4) loads, and re-saving writes the 48-byte form
check_legacy_footer : {} -> Bool
check_legacy_footer = |_| {
	f44 = [
		10, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0,
		10, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0,
		0xD2, 0x04, 0, 0,
	]
	sav44 = List.repeat(0x00.U8, 32768).concat(f44)
	resaved = GameBoy.init(rtc_rom({})).with_battery(sav44).battery()
	ts_lo = (resaved.get(32768 + 40) ?? 0).to_u64().plus((resaved.get(32768 + 41) ?? 0).to_u64().shl_wrap(8))
	ts_hi = resaved.get(32768 + 44) ?? 0xFF
	resaved.len() == 32816 and (resaved.get(32768) ?? 0) == 10 and ts_lo == 1234 and ts_hi == 0
}

# Elapsed wall time since the footer timestamp advances the clock at the
# next latch: 90 s -> 1 min 30 s
check_catchup : {} -> Bool
check_catchup = |_| {
	rom = rtc_rom({})
	sav = at_time(GameBoy.init(rom), 1000).battery()
	gb =
		at_time(GameBoy.init(rom).with_battery(sav), 1090)
			.poke(0x6000, 0x00)
			.poke(0x6000, 0x01)
			.poke(0x0000, 0x0A)
			.poke(0x4000, 0x08)
	gb.peek(0xA000) == 30 and gb.poke(0x4000, 0x09).peek(0xA000) == 1
}

# A halted clock (dh bit 6) stays put across save, load, and elapsed time
check_halt : {} -> Bool
check_halt = |_| {
	rom = rtc_rom({})
	sav =
		at_time(GameBoy.init(rom), 1000)
			.poke(0x0000, 0x0A)
			.poke(0x4000, 0x0C)
			.poke(0xA000, 0x40)
			.battery()
	gb =
		at_time(GameBoy.init(rom).with_battery(sav), 999999)
			.poke(0x6000, 0x00)
			.poke(0x6000, 0x01)
			.poke(0x0000, 0x0A)
			.poke(0x4000, 0x08)
	gb.peek(0xA000) == 0x00 and gb.poke(0x4000, 0x0C).peek(0xA000) == 0x40
}

# 512 elapsed days overflow the 9-bit day counter into the sticky carry
check_day_carry : {} -> Bool
check_day_carry = |_| {
	rom = rtc_rom({})
	sav = at_time(GameBoy.init(rom), 1000).battery()
	gb =
		at_time(GameBoy.init(rom).with_battery(sav), 44237800) # 1000 + 512 days
			.poke(0x6000, 0x00)
			.poke(0x6000, 0x01)
			.poke(0x0000, 0x0A)
			.poke(0x4000, 0x0C)
	gb.peek(0xA000) == 0x80 and gb.poke(0x4000, 0x0B).peek(0xA000) == 0x00
}

# The save-event counter bumps on disable-after-write, once, and not on
# idle enable/disable cycles
check_save_signal : {} -> Bool
check_save_signal = |_| {
	gb = GameBoy.init(battery_rom({})).poke(0x0000, 0x0A).poke(0xA000, 0x01)
	flushed = gb.poke(0x0000, 0x00)
	idle = flushed.poke(0x0000, 0x0A).poke(0x0000, 0x00)
	gb.save_events() == 0 and flushed.save_events() == 1 and idle.save_events() == 1
}

checks : {} -> List((Str, Bool))
checks = |_| [
	("ram round-trip", check_ram_roundtrip({})),
	("rtc footer round-trip", check_rtc_roundtrip({})),
	("44-byte footer fixture", check_legacy_footer({})),
	("rtc catch-up on load", check_catchup({})),
	("halted clock stays put", check_halt({})),
	("day counter carry", check_day_carry({})),
	("save-event counter", check_save_signal({})),
]

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	results = checks({})
	var failures = 0.U64
	var i = 0.U64
	while i < results.len() {
		match results.get(i) {
			Ok((name, pass)) => {
				if pass {
					Stdout.line!("ok   ${name}") ?? {}
				} else {
					Stdout.line!("FAIL ${name}") ?? {}
					failures = failures.plus(1)
				}
			}

			Err(_) => {}
		}
		i = i.plus(1)
	}
	if failures == 0 {
		Stdout.line!("battery: all ${results.len().to_str()} scenarios pass")
	} else {
		Err(BatteryChecksFailed(failures))
	}
}
