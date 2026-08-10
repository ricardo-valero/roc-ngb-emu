# APU regression check: dmg_sound 01-registers must pass, and the WAV
# rendered from it must match the frozen digest (checked numerically for
# determinism and spectral sanity when frozen; re-freeze consciously after
# intentional synthesis changes). The remaining dmg_sound singles run as an
# informative report — they test hardware quirks beyond audible correctness
# and do not gate.
{
  writeShellApplication,
  coreutils,
  roc,
  fetch-roms,
}:
writeShellApplication {
  name = "check-sound";
  runtimeInputs = [coreutils roc fetch-roms];
  text = ''
    expected="286d12ba375331bc5eb4063a3a4fca6e112e8f6a03b7b0791ee63788950220b6"

    if [ ! -f example/wav.roc ]; then
      echo "check-sound: run from the repo root (example/wav.roc not found)" >&2
      exit 2
    fi

    fetch-roms

    echo "== conformance gate: 01-registers"
    roc run example/blargg.roc -- "rom/dmg_sound/01-registers.gb" >/dev/null
    echo "PASS  01-registers"

    echo "== frozen WAV digest"
    out="$(mktemp -t sndXXXX).wav"
    roc run example/wav.roc -- "rom/dmg_sound/01-registers.gb" "$out" 180 >/dev/null
    actual="$(sha256sum "$out" | cut -d' ' -f1)"
    if [ "$actual" = "$expected" ]; then
      echo "PASS  WAV matches the frozen digest"
      rm -f "$out"
    else
      echo "FAIL  WAV digest mismatch"
      echo "      expected: $expected"
      echo "      actual:   $actual"
      echo "      kept at $out"
      exit 1
    fi

    echo "== informative: remaining dmg_sound singles (non-gating)"
    for rom in rom/dmg_sound/*.gb; do
      name="$(basename "$rom")"
      if [ "$name" = "01-registers.gb" ]; then continue; fi
      if roc run example/blargg.roc -- "$rom" >/dev/null 2>&1; then
        echo "pass  $name"
      else
        echo "fail  $name"
      fi
    done
    echo "(informative section never fails the check)"
  '';
}
