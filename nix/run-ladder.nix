# Accuracy ladder: run the Blargg timing and mooneye halt/timer ROMs under
# rom/ladder/. ROMs listed in golden/ladder.passlist gate (a listed ROM that
# fails breaks the run — the passing set never shrinks); unlisted ROMs report
# informatively. When an unlisted ROM passes, promote it by adding its
# basename to the passlist.
{
  writeShellApplication,
  coreutils,
  findutils,
  roc,
  fetch-roms,
}:
writeShellApplication {
  name = "run-ladder";
  runtimeInputs = [coreutils findutils roc fetch-roms];
  text = ''
    passlist="golden/ladder.passlist"

    if [ ! -f verify/blargg/main.roc ]; then
      echo "run-ladder: run from the repo root (verify/blargg/main.roc not found)" >&2
      exit 2
    fi

    fetch-roms
    touch "$passlist"

    gate_fail=0
    promote=()
    while IFS= read -r romfile; do
      name="$(basename "$romfile")"
      if roc run verify/blargg/main.roc -- "$romfile" >/dev/null 2>&1; then
        result=pass
      else
        result=fail
      fi
      if grep -qxF "$name" "$passlist"; then
        if [ "$result" = pass ]; then
          echo "PASS  $name"
        else
          echo "FAIL  $name (gating: listed in $passlist)"
          gate_fail=1
        fi
      else
        echo "$result  $name (informative)"
        if [ "$result" = pass ]; then
          promote+=("$name")
        fi
      fi
    done < <(find rom/ladder -name '*.gb' | sort)

    echo "----"
    if [ "''${#promote[@]}" -gt 0 ]; then
      echo "promotable (add to $passlist):"
      printf '  %s\n' "''${promote[@]}"
    fi
    if [ "$gate_fail" -ne 0 ]; then
      echo "LADDER FAILED: a previously-passing ROM regressed"
      exit 1
    fi
    echo "ladder ok ($(grep -c . "$passlist" || true) gating)"
  '';
}
