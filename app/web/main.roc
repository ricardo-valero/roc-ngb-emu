# Play a Game Boy ROM in the browser on the roc-web platform.
# ROMs load at runtime: the page fetches play.gb by default; drop any .gb
# file onto the page (or use the picker) to swap games — no rebuild.
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select.
# The platform release pairs with the flake's roc nightly and the vendored
# lib/ — bump all three together (roc-web v0.4.0 ↔ nightly-2026-08-20).
app [Model, program] {
	web: platform "https://github.com/ricardo-valero/roc-web/releases/download/v0.4.0/2TPZfBReC6hZB6YTx5Np7bBXb9r6aHcs71ezEYWRyqJk.tar.zst",
	ngb: "../../package/main.roc",
}

import web.App
import web.Host
import ngb.GameBoy

Model : { gb : Box(GameBoy), frames : U64, save_seen : U64, last_sav : List(U8), sav_note : Str }

program = { init, render! }

init = App.init(
	App.default
		.with_title("roc-ngb-emu")
		.with_screen({ width: 160, height: 144 })
		.with_scale(4)
		.with_renderer(Auto)
	# CGB LCD curve; grays are fixed points, so DMG output is untouched
		.with_color_correction(Cgb),
	# sav is the page's stored battery bytes for this cartridge (empty on
	# a clean start); with_battery loads forgivingly, but a size mismatch
	# is noted and logged on the first frame instead of passing silently.
	# state is the save-state channel — unused until the core can decode one.
	|rom, sav, _state| {
		fresh = GameBoy.init(rom)
		note =
			match fresh.battery_fit(sav) {
				Short(missing) => "sav is ${missing.to_str()} bytes short of the declared RAM — padded with zeros"
				Long(extra) => "sav is ${extra.to_str()} bytes over the declared RAM — extra ignored"
				_ => ""
			}
		{ gb: Box.box(fresh.with_battery(sav)), frames: 0, save_seen: 0, last_sav: [], sav_note: note }
	},
)

render! : Model, Host => Model
render! = |model, host| {
	buttons = {
		up: host.key_down(KeyUp),
		down: host.key_down(KeyDown),
		left: host.key_down(KeyLeft),
		right: host.key_down(KeyRight),
		a: host.key_down(KeyX),
		b: host.key_down(KeyZ),
		start: host.key_down(KeyEnter),
		select: host.key_down(KeyBackspace),
	}
	ran = Box.unbox(model.gb).run_frame({ buttons: buttons, now: host.unix_time() })
	drained = ran.take_samples()
	# Battery bytes out on the "game just saved" edge (RAM disabled after a
	# write — flush now) plus a ~1 s debounced dirty check without flush,
	# keeping the page's tab-hide copy fresh. The page retains the last
	# push, so per-frame pushing (a full cart-RAM copy across the wasm
	# boundary each frame) buys nothing. Batteryless carts never push:
	# battery() is empty and stays equal to last_sav.
	events = drained.gb.save_events()
	saved_edge = events != model.save_seen
	last_sav =
		if saved_edge {
			sav = drained.gb.battery()
			# batteryless carts with RAM can still hit the disable edge;
			# battery() is empty for them — nothing to persist
			if sav.len() > 0 {
				host.push_battery!(sav, Bool.True)
				sav
			} else {
				model.last_sav
			}
		} else if model.frames % 64 == 63 {
			sav = drained.gb.battery()
			if sav != model.last_sav {
				host.push_battery!(sav, Bool.False)
				sav
			} else {
				model.last_sav
			}
		} else {
			model.last_sav
		}
	if model.frames == 0 and model.sav_note != "" {
		host.log!("[battery] ${model.sav_note}")
	} else {
		{}
	}
	# Once a second, log the machine state the way a debugger would ask
	# for it — the fastest answer to "why is the screen blank"
	if model.frames % 60 == 0 {
		host.log!(debug_line(drained.gb))
	} else {
		{}
	}
	host.blit!(rgba(drained.gb.framebuffer()))
	host.queue_audio!(drained.samples)
	{ gb: Box.box(drained.gb), frames: model.frames + 1, save_seen: events, last_sav, sav_note: model.sav_note }
}

hex4 : U16 -> Str
hex4 = |v| {
	nib = |n| {
		c = n.bitwise_and(0x0F).to_u8_wrap()
		if c < 10 {
			c.plus(48)
		} else {
			c.plus(87)
		}
	}
	Str.from_utf8([nib(v.shr_zf_wrap(12)), nib(v.shr_zf_wrap(8)), nib(v.shr_zf_wrap(4)), nib(v)]) ?? "????"
}

hex2 : U8 -> Str
hex2 = |v| hex4(v.to_u16())

debug_line : GameBoy -> Str
debug_line = |gb| {
	pc = gb.cpu.reg.read16(ProgramCounter)
	"pc=${hex4(pc)} op=${hex2(gb.peek(pc))} ly=${hex2(gb.peek(0xFF44))} lcdc=${hex2(gb.peek(0xFF40))} stat=${hex2(gb.peek(0xFF41))} ie=${hex2(gb.peek(0xFFFF))} if=${hex2(gb.peek(0xFF0F))} key1=${hex2(gb.peek(0xFF4D))} ime=${
		if gb.cpu.ime {
			"1"
		} else {
			"0"
		}
	} halted=${
		if gb.cpu.halted {
			"1"
		} else {
			"0"
		}
	}"
}

# BGR555 framebuffer to RGBA8 (5-bit channels expanded to 8)
rgba : List(U16) -> List(U8)
rgba = |pixels| {
	var out = List.repeat(255.U8, 92160)
	var i = 0.U64
	while i < 23040 {
		px = pixels.get(i) ?? 0
		j = i * 4
		out = out.set(j, expand5(px)) ?? out
		out = out.set(j + 1, expand5(px.shr_zf_wrap(5))) ?? out
		out = out.set(j + 2, expand5(px.shr_zf_wrap(10))) ?? out
		i = i + 1
	}
	out
}

expand5 : U16 -> U8
expand5 = |v| {
	c = v.bitwise_and(0x1F).to_u8_wrap()
	c.shl_wrap(3).bitwise_or(c.shr_zf_wrap(2))
}
