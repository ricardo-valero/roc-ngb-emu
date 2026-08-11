# APU conformance + regression: the dmg_sound singles run through the
# verdict runner (./passlist gates — 01-registers; the rest informative),
# then a deterministic WAV render is compared against ./golden.sha256 with
# compare-or-create semantics (bless = delete + re-run, keeps the WAV for
# ear verification, exits 3). Run: nix run .#check-sound
{
  lib,
  writeShellApplication,
  fetchurl,
  coreutils,
  roc,
}: let
  base = "https://github.com/retrio/gb-test-roms/raw/master/dmg_sound/rom_singles";
  roms = [
    { url = "${base}/01-registers.gb"; hash = "sha256-xrn6S52dJpGbM+vnim7xmtLfhUGGz3QconRhecyfw/E="; }
    { url = "${base}/02-len ctr.gb"; hash = "sha256-dFVEElpQZXKcqyJJSnn2XDg25EJvieTMQ9Mwr+cRtBM="; }
    { url = "${base}/03-trigger.gb"; hash = "sha256-uxHnJmpxQ7r7iqKnPKcJV8YBHzbNTgtqrwZ4N4KU51w="; }
    { url = "${base}/04-sweep.gb"; hash = "sha256-WLwUVB2RuwIMd2G0I4JbNDLP3n7i+k0RFhJuTLlXPH4="; }
    { url = "${base}/05-sweep details.gb"; hash = "sha256-9YLKOgslRLlRB5fX1N1WoX9T4ABRGwroimNTIA3NkWc="; }
    { url = "${base}/06-overflow on trigger.gb"; hash = "sha256-GlEelehO1v5gd8q/RRqYtOAf3vWcra4++8F4ET6wZLE="; }
    { url = "${base}/07-len sweep period sync.gb"; hash = "sha256-Vr9bDBi5lpKcmwUrpaArRQy2GbtsJMwdNOoglRyXe/c="; }
    { url = "${base}/08-len ctr during power.gb"; hash = "sha256-MctB974QanCOwLyU8qnQtQbSR81ZHp0KKmqWDcO/ZZU="; }
    { url = "${base}/09-wave read while on.gb"; hash = "sha256-N4uG9qJdqhaFUmDX7wwk5IFGugXfQ0oLVBOYOhRH6HU="; }
    { url = "${base}/10-wave trigger while on.gb"; hash = "sha256-+mPI7XRzQR5UKF0xjjO/I/9tY37SyqdVXd6vgFeOMnk="; }
    { url = "${base}/11-regs after power.gb"; hash = "sha256-0n2rRui4gQKHI/GXUyhXLTjiXSlSibl0zWmJFqC+Xas="; }
    { url = "${base}/12-wave write while on.gb"; hash = "sha256-Lvvs0sbUCSjUT0XaT2NGJrzSeQFls5Sa7f43fHORN3Q="; }
  ];
  fetch = {url, hash}:
    fetchurl {
      inherit hash;
      url = lib.replaceStrings [" "] ["%20"] url;
      name = lib.replaceStrings [" " "," "(" ")"] ["_" "_" "_" "_"] (baseNameOf url);
    };
  registers_rom = fetch (builtins.head roms);
  romlist = lib.concatMapStringsSep "\n" (r: "${baseNameOf r.url}|${fetch r}") roms;
in
  writeShellApplication {
    name = "check-sound";
    runtimeInputs = [coreutils roc];
    text = ''
      if [ ! -f check/run.roc ]; then
        echo "check-sound: run from the repo root" >&2
        exit 2
      fi

      echo "== conformance (passlist gates)"
      gate_fail=0
      promote=()
      while IFS="|" read -r name path; do
        if out="$(roc run check/run.roc -- "$path" 2>&1)"; then
          result=pass
        else
          result=fail
        fi
        if grep -qxF "$name" check/sound/passlist; then
          if [ "$result" = pass ]; then
            echo "PASS  $name"
          else
            gate_fail=$((gate_fail + 1))
            echo "FAIL  $name (gating)"
            while IFS= read -r line; do echo "      $line"; done <<<"$out"
          fi
        else
          echo "$result  $name (informative)"
          if [ "$result" = pass ]; then promote+=("$name"); fi
        fi
      done <<'ROMS'
      ${romlist}
      ROMS
      if [ "''${#promote[@]}" -gt 0 ]; then
        echo "promotable (add to check/sound/passlist):"
        printf "  %s\n" "''${promote[@]}"
      fi

      echo "== golden WAV digest"
      golden="check/sound/golden.sha256"
      out="$(mktemp -t sndXXXX).wav"
      roc run check/sound/wav.roc -- ${registers_rom} "$out" 180 >/dev/null
      actual="$(sha256sum "$out" | cut -d" " -f1)"

      if [ ! -f "$golden" ]; then
        echo "$actual" > "$golden"
        cp "$out" check/sound/golden.wav
        rm -f "$out"
        echo "GOLDEN CREATED  $golden"
        echo "      listen to check/sound/golden.wav before committing the .sha256"
        echo "      (exit 3: a bless must be a deliberate dev-shell act, never a CI pass)"
        exit 3
      fi

      expected="$(cat "$golden")"
      if [ "$actual" = "$expected" ]; then
        echo "PASS  WAV matches the golden digest"
        rm -f "$out"
      else
        cp "$out" check/sound/actual.wav
        rm -f "$out"
        echo "FAIL  WAV digest mismatch"
        echo "      expected: $expected"
        echo "      actual:   $actual"
        echo "      actual WAV kept at check/sound/actual.wav for listening"
        exit 1
      fi

      echo "----"
      if [ "$gate_fail" -ne 0 ]; then
        echo "FAILED: $gate_fail gating ROM(s) regressed"
        exit 1
      fi
      echo "ok ($(grep -c . check/sound/passlist) gating)"
    '';
  }
