# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** the SM83 CPU core is complete and passes all 11 of Blargg's
`cpu_instrs` test ROMs (full base + CB instruction set, hardware-correct
flags, interrupts, timer, serial). Next up: PPU.

Run the Blargg verification suite (fetches the test ROMs on first run):

```bash
nix run .#run-blargg          # or plain `run-blargg` inside `nix develop`
```

Run a single test ROM or inspect a cartridge header:

```bash
roc run examples/blargg.roc -- roms/cpu_instrs/06-ld\ r,r.gb
roc run examples/cartridge.roc -- <rom-path>.gb
```

Develop:

```bash
roc check package/main.roc    # type-check the package
roc test package/main.roc     # run all inline expects
```

Development uses the new (Zig-based) Roc compiler, pinned via the Nix flake.
The pre-migration code (2024 Roc syntax and toolchain) lives on the `legacy`
branch. Rendering will target [roc-ray](https://github.com/lukewilliamboswell/roc-ray)
(see `spike/rocray-hello/SPIKE.md` for the toolchain pairing notes).

Get in touch and let's work on this together!

I'm using these resources:

- 📄 [Gameboy Pan Docs](https://gbdev.io/pandocs)
- 🎥 [Gameboy Emulator Development - Low Level Devel](https://www.youtube.com/watch?v=e87qKixKFME&list=PLVxiWMqQvhg_yk4qy2cSC3457wZJga_e5)
- 📝 [Writing an emulator](https://blog.tigris.fr/2019/07/09/writing-an-emulator-the-first-steps/)
- 🧪 [Blargg's test ROMs](https://github.com/retrio/gb-test-roms) (CPU verification oracle)

Heavily inspired by:

- [Elmboy](https://github.com/Malax/elmboy)
