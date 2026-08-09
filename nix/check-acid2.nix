# PPU rendering regression check: render dmg-acid2 for 120 frames and compare
# the framebuffer digest against the frozen known-good value. The digest was
# frozen after a 0/23040-pixel-mismatch comparison with the published
# reference image (mattcurrie/dmg-acid2 img/reference-dmg.png).
{
  writeShellApplication,
  coreutils,
  roc,
  fetch-roms,
}:
writeShellApplication {
  name = "check-acid2";
  runtimeInputs = [coreutils roc fetch-roms];
  text = ''
    expected="88dcd6f4df8466b02060126342b672b1751e0f37d21f2e2b99e7ca5b4e9a8c21"

    if [ ! -f example/frame.roc ]; then
      echo "check-acid2: run from the repo root (example/frame.roc not found)" >&2
      exit 2
    fi

    fetch-roms

    out="$(mktemp -t acid2XXXX).ppm"
    roc run example/frame.roc -- rom/dmg-acid2.gb "$out" 120
    actual="$(sha256sum "$out" | cut -d' ' -f1)"

    if [ "$actual" = "$expected" ]; then
      echo "PASS  dmg-acid2 render matches the frozen reference digest"
      rm -f "$out"
    else
      echo "FAIL  dmg-acid2 digest mismatch"
      echo "      expected: $expected"
      echo "      actual:   $actual"
      echo "      image kept at $out for inspection"
      exit 1
    fi
  '';
}
