# Design — cgb-memory

## Context

First increment of the CGB arc (exploration 2026-08-11). Current facts that shape it: VRAM and WRAM live undifferentiated inside `Mmu`'s flat 64 KiB `mem`; `Register.init` hardcodes DMG post-boot values; `Header.cgb_flag` is already parsed; the PPU reads VRAM through `mmu.read`. Standing decision for increment 2, recorded here for continuity: framebuffer representation becomes RGB555 `List(U16)` (option B — core speaks hardware, edges colorize).

## Goals / Non-Goals

**Goals:** all CGB memory infrastructure in place and tested; every DMG check byte-identical; CGB carts boot into CGB mode (rendering knowingly wrong until increment 2).

**Non-Goals:** any rendering change; speed switching (KEY1 stores, doesn't switch); HDMA; DMG-game colorization; palette-RAM access blocking during mode 3 (accuracy polish, later).

## Decisions

- **Flat `mem` remains bank-0/bank-1 storage; only the extra banks are new fields.** VRAM bank 1 = new 8 KiB list; WRAM banks 2–7 = new 24 KiB list (bank 1 stays the `mem` 0xD000 region). Minimal diff to every existing code path; DMG mode never touches the new fields.
- **Dispatch at the `read`/`write` seams**: 0x8000–0x9FFF and 0xD000–0xDFFF consult the bank registers only in CGB mode before falling through to today's `mem` path. `poke` (raw) stays bank-unaware — it is the DMG-region backdoor used by tests and internals; new expects use the public `write`.
- **PPU untouched this increment, accessor provided**: `Mmu` gains a bank-explicit VRAM reader (`read_vram : bank, addr`) that increment 2's PPU will use for tiles (bank 0) and attributes (bank 1). Until then the PPU keeps reading through `mmu.read` — correct in DMG mode (VBK inert), knowingly wrong for a CGB game that flips VBK mid-frame, which cannot render correctly yet anyway.
- **Console model as `model : [Dmg, Cgb]` on `Mmu`** (user review: an enum names the hardware family and extends to Mgb/Sgb/... — a Bool bakes in "not-CGB means DMG"). Stored on the Mmu *pragmatically*, not conceptually: the bus is the substrate every component already holds — register gating consults it constantly and the PPU reads through the Mmu — while threading a GameBoy-level model into every `read`/`write` would be invasive for zero gain. Consumers use the `is_cgb` predicate; `GameBoy.init` reads it to set `A = 0x11`; `Register.init` stays DMG-pure.
- **Palette RAM as two 64-byte lists + two specifier fields**; data-port semantics implemented exactly (6-bit index, bit-7 auto-increment on write, readback of specifier includes the auto-increment bit). Initialized to 0xFF (white) — the CGB boot ROM's palette work is increment 2's concern.
- **KEY1 stores bit 0 (prepare), reads back 0x7E | prepare** (bit 7 = current speed, always normal for now). OPRI stores bit 0. Both CGB-only.

## Risks / Trade-offs

- [Regression risk to the hot `read`/`write` dispatch] → new branches are behind `mmu.cgb` (false for every existing test and check); the byte-identical requirement on all four checks is the gate.
- [CGB games will run visibly wrong until increment 2] → expected and stated; the web app's white-screen class of reports may become "wrong colors/garbage" for CGB carts — strictly closer to working.

## Open Questions

- None blocking; palette-access timing quirks and DMG-colorization deliberately out of scope.

## Noted for increment 2 (cgb-ppu): the render-path escape hatch

The mode flag is nearly free here, but it gets dense in the PPU: attribute
fetches, palette lookups, and the changed priority resolution interleave
with DMG logic. If `render_line` becomes an unreadable braid of `if cgb`,
the sanctioned refactor is **two `render_line` variants sharing the
tile/fetch helpers** — a fork inside the module, not a second emulator.
(Hardware rationale recorded 2026-08-11: CGB is the same silicon with
gated extensions — one core with a model flag is both the hardware truth
and universal emulator practice; a separate core is the right boundary
only for a genuinely different machine, e.g. GBA's ARM7.)
