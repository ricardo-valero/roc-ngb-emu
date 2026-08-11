# Play a Game Boy ROM in a window. The ROM at rom/play.gb is embedded at
# build time (run `nix run .#fetch-roms` once to seed it with dmg-acid2;
# drop any game ROM there and rebuild to play it).
#
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select,
# Esc exits.
app [Model, program] {
    ray: platform "https://github.com/lukewilliamboswell/roc-ray/releases/download/0.9.0/3sKTYuHvxSV77dDyZrxuUYgfrAarL6ZtasWMPeH32udh.tar.zst",
    ngb: "../package/main.roc",
}

import ray.App
import ray.Assets
import ray.Color
import ray.Draw
import ray.Host
import ngb.GameBoy
import "../rom/play.gb" as rom : List(U8)

# GameBoy is boxed: passing the large nested record itself through the
# host's model round-trip crashes in the platform's refcount walk
# (roc_llvm_rc_incref reading past an allocation).
Model : {
    gb : Box(GameBoy),
    screen : Assets.Texture,
}

scale : F32
scale = 4

program = { init!, render! }

init! : App.Init(Model, [ResourceLimit, TextureGenerationFailed])
init! = App.init(
    App.default
        .with_title("roc-ngb-emu")
        .with_size({ width: 160 * 4, height: 144 * 4 })
        .with_frame_pacing(Capped(60)),
    |_host| {
        screen = Assets.Texture.generate_color!({ width: 160, height: 144, color: Color.black })?
        screen.set_filter!(Point)
        screen.set_wrap!(Clamp)
        Ok({ gb: Box.box(GameBoy.init(rom)), screen })
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

    ran = Box.unbox(model.gb).run_frame(buttons)
    # No speaker path yet (roc-ray has no PCM streaming): drop the APU
    # samples each frame so the ring stays parked
    drained = ran.take_samples()
    gb = drained.gb

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

# The classic DMG green LCD, light to dark
shade_color : U8 -> Color
shade_color = |shade|
    match shade {
        0 => Color.from_hex_rgb(0x9BBC0F)
        1 => Color.from_hex_rgb(0x8BAC0F)
        2 => Color.from_hex_rgb(0x306230)
        _ => Color.from_hex_rgb(0x0F380F)
    }
