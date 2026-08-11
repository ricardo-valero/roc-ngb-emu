# Play a Game Boy ROM in the browser on the roc-web platform.
# ROMs load at runtime: the page fetches play.gb by default; drop any .gb
# file onto the page (or use the picker) to swap games — no rebuild.
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select.
app [Model, program] {
    web: platform "https://github.com/ricardo-valero/roc-web/releases/download/v0.2.1/8P65Tbg3xjD6MpJx33quQKJES9SkV9aRGRn59juib6sz.tar.zst",
    ngb: "../../package/main.roc",
}

import web.App
import web.Host
import ngb.GameBoy

Model : Box(GameBoy)

program = { init, render! }

init = App.init(
    App.default
        .with_title("roc-ngb-emu")
        .with_screen({ width: 160, height: 144 })
        .with_scale(4)
        .with_renderer(Auto),
    |rom| Box.box(GameBoy.init(rom)),
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
    ran = Box.unbox(model).run_frame(buttons)
    drained = ran.take_samples()
    host.blit!(rgba(drained.gb.framebuffer()))
    host.queue_audio!(drained.samples)
    Box.box(drained.gb)
}

# DMG shades (0..3) to RGBA8 grayscale
rgba : List(U8) -> List(U8)
rgba = |shades| {
    var out = List.repeat(255.U8, 92160)
    var i = 0.U64
    while i < 23040 {
        gray =
            match shades.get(i) ?? 0 {
                0 => 255.U8
                1 => 170
                2 => 85
                _ => 0
            }
        j = i * 4
        out = out.set(j, gray) ?? out
        out = out.set(j + 1, gray) ?? out
        out = out.set(j + 2, gray) ?? out
        i = i + 1
    }
    out
}
