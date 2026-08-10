# Play a Game Boy ROM in the browser on the roc-canvas platform (WebGPU).
# Expects a sibling checkout of roc-canvas (see README). Like play.roc, the
# ROM at rom/play.gb is embedded at build time.
# Build: roc build example/web.roc --output=web/play.wasm
# Buttons arrive as a packed u32: bit0..7 = Right,Left,Up,Down,A,B,Select,Start.
app [Model, main] {
    canvas: platform "../../roc-canvas/platform/main.roc",
    ngb: "../package/main.roc",
}

import canvas.Host
import ngb.GameBoy
import "../rom/play.gb" as rom : List(U8)

Model : Box(GameBoy)

main = { init!, frame! }

init! : () => Model
init! = || {
    Host.log!("roc-ngb-emu web: init (${rom.len().to_str()} byte ROM)")
    Box.box(GameBoy.init(rom))
}

frame! : Model, U32 => Model
frame! = |boxed, bits| {
    gb = Box.unbox(boxed).run_frame(decode_buttons(bits))
    Host.blit!(rgba(gb.framebuffer()), 160, 144)
    Box.box(gb)
}

decode_buttons = |bits| {
    b = |n| bits.shr_zf_wrap(n).bitwise_and(1.U32) == 1.U32
    { right: b(0), left: b(1), up: b(2), down: b(3), a: b(4), b: b(5), select: b(6), start: b(7) }
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
