# PPU rendering, both console models: dmg-acid2 and cgb-acid2 rendered and
# compared against their goldens (compare-or-create; blessing and mismatch
# artifacts land next to each golden and must be verified visually against
# the published references). Run: nix run .#check-acid2
{
  writeShellApplication,
  fetchurl,
  roc,
}: let
  dmg_rom = fetchurl {
    url = "https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb";
    hash = "sha256-Rk4Ut9Quf+6gt+3kK+cHHciJE/dbn/pEQplCS2PR3/E=";
  };
  cgb_rom = fetchurl {
    url = "https://github.com/mattcurrie/cgb-acid2/releases/download/v1.1/cgb-acid2.gbc";
    hash = "sha256-GX+wvOxUTwQAUn/HB+CpT1VDWXSYbmmGtCSs5d6Bcg4=";
  };
in
  writeShellApplication {
    name = "check-acid2";
    runtimeInputs = [roc];
    text = ''
      runner="$(mktemp -t roc-acid2-XXXXXX)"
      roc build check/acid2/main.roc --output="$runner" >/dev/null
      "$runner" --check "${dmg_rom}" check/acid2/golden
      "$runner" --check "${cgb_rom}" check/acid2/cgb-golden
      rm -f "$runner"
    '';
  }
