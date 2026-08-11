# CPU conformance: Blargg's cpu_instrs plus his timing ROMs, run through
# the shared suite runner against ./passlist. Run: nix run .#check-blargg
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
