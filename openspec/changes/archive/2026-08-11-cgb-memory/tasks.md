# Tasks — cgb-memory

## 1. Mode + banking

- [x] 1.1 `Mmu`: `cgb` flag from header byte 0x0143; VRAM bank 1 + WRAM banks 2–7 storage; VBK/SVBK dispatch in `read`/`write`; `read_vram(bank, addr)` accessor; expects (bank independence, readbacks, zero-selects-one)
- [x] 1.2 `GameBoy.init`: `A = 0x11` in CGB mode; expects for both modes

## 2. Palette RAM + register file

- [x] 2.1 BCPS/BCPD + OCPS/OCPD with auto-increment; KEY1/OPRI stores; expects (auto-increment walk, independent memories, specifier readback)
- [x] 2.2 DMG inertness expects for every new register

## 3. Regression

- [x] 3.1 All four checks byte-identical green; `roc test package/main.roc` and `roc test check/lib/main.roc`; both apps build
