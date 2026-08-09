# Run all Blargg cpu_instrs individual ROMs through the headless runner.
# Must be invoked from the repo root (it runs example/blargg.roc).
{
  writeShellApplication,
  coreutils,
  roc,
  fetch-roms,
}:
writeShellApplication {
  name = "run-blargg";
  runtimeInputs = [coreutils roc fetch-roms];
  text = ''
    if [ ! -f example/blargg.roc ]; then
      echo "run-blargg: run from the repo root (example/blargg.roc not found)" >&2
      exit 2
    fi

    fetch-roms

    failures=0
    total=0
    for rom in rom/cpu_instrs/*.gb; do
      total=$((total + 1))
      name="$(basename "$rom")"
      if out="$(roc run example/blargg.roc -- "$rom" 2>&1)"; then
        echo "PASS  $name"
      else
        failures=$((failures + 1))
        echo "FAIL  $name"
        while IFS= read -r line; do echo "      $line"; done <<<"$out"
      fi
    done

    echo "----"
    echo "$((total - failures))/$total passed"
    [ "$failures" -eq 0 ]
  '';
}
