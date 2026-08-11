# APU regression check: dmg_sound 01-registers must pass, and the WAV
# rendered from it must match the golden digest, with compare-or-create
# semantics. The golden lives in golden/sound.sha256 (seeded 2026-08-10 from
# the value frozen after numeric determinism/spectral checks and one-time ear
# verification). Blessing = delete golden/sound.sha256 and re-run: the check
# writes the digest, keeps golden/sound.wav for ear verification, and exits 3
# so CI can never bless silently. On mismatch the actual WAV is kept as
# golden/sound.actual.wav for listening comparison. The remaining dmg_sound
# singles run as an informative report — they test hardware quirks beyond
# audible correctness and do not gate.
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
    golden="golden/sound.sha256"

    if [ ! -f example/wav.roc ]; then
      echo "check-sound: run from the repo root (example/wav.roc not found)" >&2
      exit 2
    fi

    fetch-roms
    mkdir -p golden

    echo "== conformance gate: 01-registers"
    roc run verify/blargg/main.roc -- "rom/dmg_sound/01-registers.gb" >/dev/null
    echo "PASS  01-registers"

    echo "== golden WAV digest"
    out="$(mktemp -t sndXXXX).wav"
    roc run example/wav.roc -- "rom/dmg_sound/01-registers.gb" "$out" 180 >/dev/null
    actual="$(sha256sum "$out" | cut -d' ' -f1)"

    if [ ! -f "$golden" ]; then
      echo "$actual" > "$golden"
      cp "$out" golden/sound.wav
      rm -f "$out"
      echo "GOLDEN CREATED  $golden"
      echo "      listen to golden/sound.wav before committing the .sha256"
      echo "      (exit 3: a bless must be a deliberate dev-shell act, never a CI pass)"
      exit 3
    fi

    expected="$(cat "$golden")"
    if [ "$actual" = "$expected" ]; then
      echo "PASS  WAV matches the golden digest"
      rm -f "$out"
    else
      cp "$out" golden/sound.actual.wav
      rm -f "$out"
      echo "FAIL  WAV digest mismatch"
      echo "      expected: $expected"
      echo "      actual:   $actual"
      echo "      actual WAV kept at golden/sound.actual.wav for listening"
      exit 1
    fi

    echo "== informative: remaining dmg_sound singles (non-gating)"
    for rom in rom/dmg_sound/*.gb; do
      name="$(basename "$rom")"
      if [ "$name" = "01-registers.gb" ]; then continue; fi
      if roc run verify/blargg/main.roc -- "$rom" >/dev/null 2>&1; then
        echo "pass  $name"
      else
        echo "fail  $name"
      fi
    done
    echo "(informative section never fails the check)"
  '';
}
