# Tasks — Hardware Map in the README

## 1. Write the section

- [x] 1.1 Confirm `extract-bus-peripherals` has landed (module names
      `Bus.roc` / `Timer.roc` / `Joypad.roc` exist); if not, pause and
      resequence
- [x] 1.2 Add the "Hardware map" section to `README.md` per the proposal:
      two-views preface, block-to-module table with a physical-location
      line per module, and the deliberate-mismatches note (OAM and
      peripheral register bytes owned by the bus)
- [x] 1.3 Cross-check every module named in the table against the actual
      `package/` tree and every physical claim against Pan Docs (SoC die
      contents, external SRAM, cartridge PCB)

## 2. Verify

- [x] 2.1 Table renders correctly (GitHub Markdown preview) and all file
      paths in it resolve in the repo
