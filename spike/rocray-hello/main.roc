# Spike: verify roc-ray builds and runs against the flake-pinned Roc nightly.
# roc-ray 0.9.0 pairs with nightly-2026-August-05-24f0b47 (see SPIKE.md).
app [Model, program] { rr: platform "https://github.com/lukewilliamboswell/roc-ray/releases/download/0.9.0/3sKTYuHvxSV77dDyZrxuUYgfrAarL6ZtasWMPeH32udh.tar.zst" }

import rr.App
import rr.Color
import rr.Draw
import rr.Host
import rr.Text

Model : {
    title : Text.Prepared,
}

program = { init!, render! }

init! : App.Init(Model, [ResourceLimit])
init! = App.init(
    App.default.with_title("roc-ngb-emu spike").with_frame_pacing(Capped(60)),
    |_host|
        Ok({
            title: Text.from("roc-ray + roc-ngb-emu toolchain spike").size(28).prepare!()?,
        }),
)

render! : Model, Host, Draw.Frame => Try(Model, [Exit(I64), ..])
render! = |model, host, frame| {
    if host.key_pressed(KeyEscape) {
        host.exit!(0)
    }

    frame.clear!(Color.from_hex_rgb(0x0d1425))
    model.title.draw!(frame, { pos: { x: 400, y: 220 }, color: Color.white, align: Text.align_top_center })

    Ok(model)
}
