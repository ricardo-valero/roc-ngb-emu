# Blargg conformance: cpu_instrs, the timing ROMs, and the dmg_sound
# singles, run through the shared suite runner against ./passlist.
# Run: nix run .#check-blargg
{
  lib,
  writeShellApplication,
  fetchFromGitHub,
  roc,
}: let
  gb-test-roms = import ../gb-test-roms.nix {inherit fetchFromGitHub;};
  roms = [
    "cpu_instrs/cpu_instrs.gb"
    "cpu_instrs/individual/01-special.gb"
    "cpu_instrs/individual/02-interrupts.gb"
    "cpu_instrs/individual/03-op sp,hl.gb"
    "cpu_instrs/individual/04-op r,imm.gb"
    "cpu_instrs/individual/05-op rp.gb"
    "cpu_instrs/individual/06-ld r,r.gb"
    "cpu_instrs/individual/07-jr,jp,call,ret,rst.gb"
    "cpu_instrs/individual/08-misc instrs.gb"
    "cpu_instrs/individual/09-op r,r.gb"
    "cpu_instrs/individual/10-bit ops.gb"
    "cpu_instrs/individual/11-op a,(hl).gb"
    "dmg_sound/rom_singles/01-registers.gb"
    "dmg_sound/rom_singles/02-len ctr.gb"
    "dmg_sound/rom_singles/03-trigger.gb"
    "dmg_sound/rom_singles/04-sweep.gb"
    "dmg_sound/rom_singles/05-sweep details.gb"
    "dmg_sound/rom_singles/06-overflow on trigger.gb"
    "dmg_sound/rom_singles/07-len sweep period sync.gb"
    "dmg_sound/rom_singles/08-len ctr during power.gb"
    "dmg_sound/rom_singles/09-wave read while on.gb"
    "dmg_sound/rom_singles/10-wave trigger while on.gb"
    "dmg_sound/rom_singles/11-regs after power.gb"
    "dmg_sound/rom_singles/12-wave write while on.gb"
    "instr_timing/instr_timing.gb"
    "mem_timing/mem_timing.gb"
    "mem_timing/individual/01-read_timing.gb"
    "mem_timing/individual/02-write_timing.gb"
    "mem_timing/individual/03-modify_timing.gb"
  ];
in
  writeShellApplication {
    name = "check-blargg";
    runtimeInputs = [roc];
    text = ''
      roc run check/run.roc -- check/blargg/passlist ${lib.concatMapStringsSep " " (p: ''"${gb-test-roms}/${p}"'') roms}
    '';
  }
