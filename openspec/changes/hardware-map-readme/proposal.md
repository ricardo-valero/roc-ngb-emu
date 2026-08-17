# Hardware Map in the README

## Why

The physical-packaging view of the console (what lives on the DMG-CPU die,
what hangs off the external bus, why VRAM locking and IRQ lines exist) is
currently only discoverable in an out-of-repo diagram. The file tree
deliberately encodes ownership rather than packaging, so the repo should
carry the mapping between the two views where every reader finds it: the
README.

## What Changes

- New README section **"Hardware map"** between the status list and
  Development, containing:
  - A short preface stating the two-views principle: the module tree
    encodes ownership/dependency; the physical SoC boundary lives in
    `GameBoy.roc` (the composition module *is* the DMG-CPU chip).
  - A block-to-module table: SM83 core → `Cpu/`; address/data bus, RAMs +
    I/O page → `Bus.roc`; timers → `Timer.roc`; joypad → `Joypad.roc`;
    PPU → `Ppu.roc`; APU → `Apu.roc`; cartridge → `Cartridge.roc` +
    `Cartridge/Header.roc`; LCD/speaker/buttons/battery → `app/`.
  - One line per module noting where it physically lives ("on the DMG-CPU
    die", "external SRAM chips", "cartridge PCB", "not hardware — the host").
  - A line on the deliberate mismatches (OAM bytes and the peripheral
    registers live with the bus even though the silicon sits elsewhere)
    so the table doesn't read as an error.
- Uses the post-`extract-bus-peripherals` module names (`Bus.roc`,
  `Timer.roc`, `Joypad.roc`) — sequenced after that change lands.

## Capabilities

### New Capabilities

None — documentation only.

### Modified Capabilities

None — no requirement changes. This change sets `skip_specs: true` in
`.openspec.yaml` accordingly.

## Impact

- `README.md` only. No code, no checks affected.
- Sequencing dependency: implement after `extract-bus-peripherals` so the
  documented names match the tree (or update the section in that change's
  final task if orders swap).
