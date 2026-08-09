# roc-ngb-emu

A Game Boy (DMG) emulator written in [Roc](https://www.roc-lang.org). WIP!

**Status:** playable! The SM83 CPU passes all 11 of Blargg's `cpu_instrs`
test ROMs, the PPU renders [dmg-acid2](https://github.com/mattcurrie/dmg-acid2)
pixel-perfect against the reference image, and a
[roc-ray](https://github.com/lukewilliamboswell/roc-ray) app plays ROMs in a
window with keyboard input. MBC1 and MBC3 cartridges work (banked ROM +
cartridge RAM), so most of the DMG library runs — no audio or battery saves
yet, and the combined Blargg `cpu_instrs.gb` is part of the 12-ROM suite.

Play a ROM (the app embeds `rom/play.gb` at build time):

```bash
nix run .#fetch-roms                        # seeds rom/play.gb with dmg-acid2
cp your-game.gb rom/play.gb                 # optional: play your own ROM
roc build example/play.roc && ./play        # arrows, X=A, Z=B, Enter=Start, Esc quits
```

Run the verification suites (test ROMs are fetched on first run):

```bash
nix run .#run-blargg          # CPU: Blargg cpu_instrs, 11 ROMs
nix run .#check-acid2         # PPU: dmg-acid2 vs frozen reference digest
```

Dump any ROM's screen to an image, or inspect a cartridge header:

```bash
roc run example/frame.roc -- rom/dmg-acid2.gb out.ppm 120
roc run example/cartridge.roc -- <rom-path>.gb
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
