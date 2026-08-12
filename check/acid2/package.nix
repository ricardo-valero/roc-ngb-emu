# PPU rendering: render dmg-acid2 and compare against ./golden.sha256
# (compare-or-create; blessing and mismatch artifacts are written next to
# the golden by the check program). Run: nix run .#check-acid2
{
  writeShellApplication,
  fetchurl,
  roc,
}: let
  rom = fetchurl {
    url = "https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb";
    hash = "sha256-Rk4Ut9Quf+6gt+3kK+cHHciJE/dbn/pEQplCS2PR3/E=";
  };
in
  writeShellApplication {
    name = "check-acid2";
    runtimeInputs = [roc];
    text = ''
      runner="$(mktemp -t roc-acid2-XXXXXX)"
      roc build check/acid2/main.roc --output="$runner" >/dev/null
      "$runner" --check "${rom}"
      rm -f "$runner"
    '';
  }
