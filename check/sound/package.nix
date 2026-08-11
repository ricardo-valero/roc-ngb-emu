# APU conformance + regression: the dmg_sound singles through the sound
# check program — ./passlist gates (01-registers), the rest informative,
# then the golden WAV digest. Run: nix run .#check-sound
{
  lib,
  writeShellApplication,
  fetchFromGitHub,
  roc,
}: let
  gb-test-roms = import ../gb-test-roms.nix {inherit fetchFromGitHub;};
  roms = [
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
  ];
in
  writeShellApplication {
    name = "check-sound";
    runtimeInputs = [roc];
    text = ''
      roc run check/sound/main.roc -- check/sound/passlist ${lib.concatMapStringsSep " " (p: ''"${gb-test-roms}/${p}"'') roms}
    '';
  }
