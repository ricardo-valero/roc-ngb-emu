# Timing/halt conformance: the mooneye-gb acceptance subset, extracted from
# the official suite tarball. ROMs listed in ./passlist gate; the rest
# report informatively until promoted. Run: nix run .#check-mooneye
{
  writeShellApplication,
  fetchurl,
  runCommand,
  xz,
  coreutils,
  roc,
}: let
  mts = "mts-20240926-1737-443f6e1";
  tarball = fetchurl {
    url = "https://gekkio.fi/files/mooneye-test-suite/${mts}/${mts}.tar.xz";
    hash = "sha256-2asRoBNR4Ost6khSNwJ8TdZsBSjHB9If94YEFXuWeDc=";
  };
  roms = runCommand "mooneye-roms" {nativeBuildInputs = [xz];} ''
    tar -xJf ${tarball}
    mkdir -p $out
    cp ${mts}/acceptance/timer/*.gb $out/
    cp ${mts}/acceptance/halt_ime0_ei.gb \
       ${mts}/acceptance/halt_ime0_nointr_timing.gb \
       ${mts}/acceptance/halt_ime1_timing.gb $out/
  '';
in
  writeShellApplication {
    name = "check-mooneye";
    runtimeInputs = [coreutils roc];
    text = ''
      if [ ! -f checks/run.roc ]; then
        echo "check-mooneye: run from the repo root" >&2
        exit 2
      fi

      gate_fail=0
      promote=()
      for path in ${roms}/*.gb; do
        name="$(basename "$path")"
        if out="$(roc run checks/run.roc -- "$path" 2>&1)"; then
          result=pass
        else
          result=fail
        fi
        if grep -qxF "$name" checks/mooneye/passlist; then
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
      done

      echo "----"
      if [ "''${#promote[@]}" -gt 0 ]; then
        echo "promotable (add to checks/mooneye/passlist):"
        printf "  %s\n" "''${promote[@]}"
      fi
      if [ "$gate_fail" -ne 0 ]; then
        echo "FAILED: $gate_fail gating ROM(s) regressed"
        exit 1
      fi
      echo "ok ($(grep -c . checks/mooneye/passlist) gating)"
    '';
  }
