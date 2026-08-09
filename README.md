# roc-ngb-emu

Attempt to write an emulator with roc! WIP!

Run to get cartridge info:

```bash
roc run examples/cartridge.roc -- <rom-path>.gb
```

Development uses the new (Zig-based) Roc compiler, pinned via the Nix flake:

```bash
nix develop                   # shell with roc nightly + nixd
roc check package/main.roc    # type-check the package
roc test package/main.roc     # run all inline expects
```

The pre-migration code (2024 Roc syntax and toolchain) lives on the `legacy` branch.

Get in touch and let's work on this together!

I'm using these resources:

- 📄 [Gameboy Pan Docs](https://gbdev.io/pandocs)
- 🎥 [Gameboy Emulator Development - Low Level Devel](https://www.youtube.com/watch?v=e87qKixKFME&list=PLVxiWMqQvhg_yk4qy2cSC3457wZJga_e5)
- 📝 [Writing an emulator](https://blog.tigris.fr/2019/07/09/writing-an-emulator-the-first-steps/)

Heavily inspired by:

- [Elmboy](https://github.com/Malax/elmboy)
