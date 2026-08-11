# APU audio regression: the golden WAV digest of 01-registers (the
# dmg_sound conformance suite lives in check/blargg). Run:
# nix run .#check-sound
{
  writeShellApplication,
  fetchFromGitHub,
  roc,
}: let
  gb-test-roms = import ../gb-test-roms.nix {inherit fetchFromGitHub;};
in
  writeShellApplication {
    name = "check-sound";
    runtimeInputs = [roc];
    text = ''
      roc run check/sound/main.roc -- "${gb-test-roms}/dmg_sound/rom_singles/01-registers.gb"
    '';
  }
