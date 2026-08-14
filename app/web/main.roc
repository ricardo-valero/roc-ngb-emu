# Play a Game Boy ROM in the browser on the roc-web platform.
# ROMs load at runtime: the page fetches play.gb by default; drop any .gb
# file onto the page (or use the picker) to swap games — no rebuild.
# Controls: arrows = d-pad, X = A, Z = B, Enter = Start, Backspace = Select.
# NOTE: pre-release pairing — the battery/clock contract (load! taking sav
# bytes, host.unix_time, push_battery!) is in the local roc-web checkout;
# repoint at the release-bundle URL once v0.4.0 is cut and lib/ re-vendored.
app [Model, program] {
    web: platform "../../../roc-web/platform/main.roc",
    ngb: "../../package/main.roc",
}

import web.App
import web.Host
import ngb.GameBoy

Model : { gb : Box(GameBoy), frames : U64, save_seen : U64 }

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
    # a clean start); with_battery no-ops for batteryless carts
    |rom, sav| { gb: Box.box(GameBoy.init(rom).with_battery(sav)), frames: 0, save_seen: 0 },
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
    # Battery bytes out every frame so the page always holds current save
    # state (it persists on tab-hide); the flush flag marks the "game just
    # saved" edge for an immediate write. Empty for batteryless carts.
    events = drained.gb.save_events()
    host.push_battery!(drained.gb.battery(), events != model.save_seen)
    # Once a second, log the machine state the way a debugger would ask
    # for it — the fastest answer to "why is the screen blank"
    if model.frames % 60 == 0 {
        host.log!(debug_line(drained.gb))
    } else {
        {}
    }
    host.blit!(rgba(drained.gb.framebuffer()))
    host.queue_audio!(drained.samples)
    { gb: Box.box(drained.gb), frames: model.frames + 1, save_seen: events }
}

hex4 : U16 -> Str
hex4 = |v| {
    nib = |n| {
        c = n.bitwise_and(0x0F).to_u8_wrap()
        if c < 10 { c.plus(48) } else { c.plus(87) }
    }
    Str.from_utf8([nib(v.shr_zf_wrap(12)), nib(v.shr_zf_wrap(8)), nib(v.shr_zf_wrap(4)), nib(v)]) ?? "????"
}

hex2 : U8 -> Str
hex2 = |v| hex4(v.to_u16())

debug_line : GameBoy -> Str
debug_line = |gb| {
    pc = gb.reg.read16(ProgramCounter)
    "pc=${hex4(pc)} op=${hex2(gb.peek(pc))} ly=${hex2(gb.peek(0xFF44))} lcdc=${hex2(gb.peek(0xFF40))} stat=${hex2(gb.peek(0xFF41))} ie=${hex2(gb.peek(0xFFFF))} if=${hex2(gb.peek(0xFF0F))} key1=${hex2(gb.peek(0xFF4D))} ime=${if gb.ime { "1" } else { "0" }} halted=${if gb.halted { "1" } else { "0" }}"
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
