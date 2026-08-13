# Play a Game Boy ROM in a window. The ROM is read from disk at startup —
# the first program argument (`./ray game.gb`), else rom/play.gbc — so
# swapping games needs no rebuild.
#
# The platform is the local roc-ray fork checkout (../../roc-ray, branch
# file-io), which adds the binary read_bytes!/write_bytes! effects and PCM
# audio streaming; build its host once with `zig build` there before building
# this app.
#
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
# Esc exits.
app [Model, program] {
	ray: platform "../../roc-ray/platform/main.roc",
	ngb: "../package/main.roc",
}

import ray.App
import ray.Assets
import ray.Audio
import ray.Color
import ray.Draw
import ray.Host
import ngb.GameBoy

# GameBoy is boxed: passing the large nested record itself through the
# host's model round-trip crashes in the platform's refcount walk
# (roc_llvm_rc_incref reading past an allocation).
Model : {
	gb : Box(GameBoy),
	screen : Assets.Texture,
	speaker : Audio.Stream,
}

scale : F32
scale = 4

# Speaker depth emulation refills to each tick: ~60 ms at 48 kHz, on top of
# the host's device buffering (2 x 1024 frames)
target_depth : U64
target_depth = 2880

program = { init!, render! }

init! : App.Init(Model, [ResourceLimit, TextureGenerationFailed])
init! = App.init(
	App.default
		.with_title("roc-ngb-emu")
		.with_size({ width: 160 * 4, height: 144 * 4 })
		.with_frame_pacing(Capped(60)),
	|host| {
		rom_path = host.args!().get(0) ?? "rom/play.gbc"
		rom = match host.read_bytes!(rom_path) {
			Ok(bytes) => bytes
			Err(_) => crash("no ROM at ${rom_path} — copy a Game Boy ROM there, or pass a path: ./ray game.gb")
		}
		screen = Assets.Texture.generate_color!({ width: 160, height: 144, color: Color.black })?
		screen.set_filter!(Point)
		screen.set_wrap!(Clamp)
		speaker = match Audio.create_stream!({ sample_rate: 48000, channels: 2 }) {
			Ok(stream) => stream
			Err(_) => crash("could not open a 48 kHz stereo audio stream — is an output device available?")
		}
		Ok({ gb: Box.box(GameBoy.init(rom)), screen, speaker })
	},
)

render! : Model, Host, Draw.Frame => Try(Model, [Exit(I64), PixelCountMismatch, ..])
render! = |model, host, frame| {
	if host.key_pressed(KeyEscape) {
		host.exit!(0)
	}

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

	# Audio-clock pacing (wasmboy-style, mirroring app/web): run emulated
	# frames until the speaker holds ~60 ms, bounded per tick. Emulation locks
	# to the audio clock, so the 60 Hz cap vs 59.73 Hz Game Boy drift shows up
	# as a rare repeated video frame instead of audio drops.
	var gb = Box.unbox(model.gb)
	var ran = 0
	var queued = model.speaker.buffered!()
	while queued < target_depth and ran < 4 {
		stepped = gb.run_frame(buttons)
		drained = stepped.take_samples()
		gb = drained.gb
		model.speaker.push!(drained.samples)
		queued = model.speaker.buffered!()
		ran = ran + 1
	}

	model.screen.update!(gb.framebuffer().map(shade_color))?

	frame.clear!(Color.black)
	frame.texture!({
		texture: model.screen.view(),
		source: model.screen.rect(),
		dest: { x: 0, y: 0, width: 160 * scale, height: 144 * scale },
		origin: { x: 0, y: 0 },
		rotation: 0,
		tint: Color.white,
	})

	Ok({ ..model, gb: Box.box(gb) })
}

# BGR555 framebuffer pixel to screen color (5-bit channels expanded to 8)
shade_color : U16 -> Color.Rgba
shade_color = |px| {
	r = expand5(px).to_u32()
	g = expand5(px.shr_zf_wrap(5)).to_u32()
	b = expand5(px.shr_zf_wrap(10)).to_u32()
	Color.from_hex_rgb(r.shl_wrap(16).bitwise_or(g.shl_wrap(8)).bitwise_or(b))
}

expand5 : U16 -> U8
expand5 = |v| {
	c = v.bitwise_and(0x1F).to_u8_wrap()
	c.shl_wrap(3).bitwise_or(c.shr_zf_wrap(2))
}
