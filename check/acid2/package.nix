# PPU rendering: render dmg-acid2 for 120 frames and compare the frame
# digest against ./golden.sha256 with compare-or-create semantics.
# Blessing = delete the golden and re-run (writes a reviewable PPM, exits 3
# so CI can never bless silently). Run: nix run .#check-acid2
{
  writeShellApplication,
  fetchurl,
  coreutils,
  roc,
}: let
  rom = fetchurl {
    url = "https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb";
    hash = "sha256-Rk4Ut9Quf+6gt+3kK+cHHciJE/dbn/pEQplCS2PR3/E=";
  };
in
  writeShellApplication {
    name = "check-acid2";
    runtimeInputs = [coreutils roc];
    text = ''
      if [ ! -f check/acid2/main.roc ]; then
        echo "check-acid2: run from the repo root" >&2
        exit 2
      fi

      golden="check/acid2/golden.sha256"
      out="$(mktemp -t acid2XXXX).ppm"
      roc run check/acid2/main.roc -- ${rom} "$out" 120
      actual="$(sha256sum "$out" | cut -d" " -f1)"

      if [ ! -f "$golden" ]; then
        echo "$actual" > "$golden"
        cp "$out" check/acid2/golden.ppm
        rm -f "$out"
        echo "GOLDEN CREATED  $golden"
        echo "      review check/acid2/golden.ppm against the published reference, then commit the .sha256"
        echo "      (exit 3: a bless must be a deliberate dev-shell act, never a CI pass)"
        exit 3
      fi

      expected="$(cat "$golden")"
      if [ "$actual" = "$expected" ]; then
        echo "PASS  dmg-acid2 render matches the golden digest"
        rm -f "$out"
      else
        cp "$out" check/acid2/actual.ppm
        rm -f "$out"
        echo "FAIL  dmg-acid2 digest mismatch"
        echo "      expected: $expected"
        echo "      actual:   $actual"
        echo "      actual frame kept at check/acid2/actual.ppm"
        exit 1
      fi
    '';
  }
