# PPU rendering regression check with compare-or-create golden semantics.
# The golden digest lives in golden/acid2.sha256 (seeded 2026-08-10 from the
# value frozen after a 0/23040-pixel-mismatch comparison with the published
# reference image, mattcurrie/dmg-acid2 img/reference-dmg.png).
# Blessing a new golden = delete golden/acid2.sha256 and re-run: the check
# writes the digest plus a viewable golden/acid2.ppm and exits 3 so CI can
# never bless silently. On mismatch the actual frame is kept as
# golden/acid2.actual.ppm for side-by-side review.
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
    golden="golden/acid2.sha256"

    if [ ! -f example/frame.roc ]; then
      echo "check-acid2: run from the repo root (example/frame.roc not found)" >&2
      exit 2
    fi

    fetch-roms
    mkdir -p golden

    out="$(mktemp -t acid2XXXX).ppm"
    roc run example/frame.roc -- rom/dmg-acid2.gb "$out" 120
    actual="$(sha256sum "$out" | cut -d' ' -f1)"

    if [ ! -f "$golden" ]; then
      echo "$actual" > "$golden"
      cp "$out" golden/acid2.ppm
      rm -f "$out"
      echo "GOLDEN CREATED  $golden"
      echo "      review golden/acid2.ppm against the published reference, then commit the .sha256"
      echo "      (exit 3: a bless must be a deliberate dev-shell act, never a CI pass)"
      exit 3
    fi

    expected="$(cat "$golden")"
    if [ "$actual" = "$expected" ]; then
      echo "PASS  dmg-acid2 render matches the golden digest"
      rm -f "$out"
    else
      cp "$out" golden/acid2.actual.ppm
      rm -f "$out"
      echo "FAIL  dmg-acid2 digest mismatch"
      echo "      expected: $expected"
      echo "      actual:   $actual"
      echo "      actual frame kept at golden/acid2.actual.ppm (blessed image: golden/acid2.ppm)"
      exit 1
    fi
  '';
}
