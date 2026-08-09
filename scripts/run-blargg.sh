#!/usr/bin/env bash
# Run all Blargg cpu_instrs individual ROMs through the headless runner.
# Requires the devshell (nix develop) so `roc` is on PATH.
set -uo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
"$root/scripts/fetch-roms.sh"

failures=0
total=0
for rom in "$root"/roms/cpu_instrs/*.gb; do
  total=$((total + 1))
  name="$(basename "$rom")"
  if out="$(roc run "$root/examples/blargg.roc" -- "$rom" 2>&1)"; then
    echo "PASS  $name"
  else
    failures=$((failures + 1))
    echo "FAIL  $name"
    echo "$out" | sed 's/^/      /'
  fi
done

echo "----"
echo "$((total - failures))/$total passed"
[ "$failures" -eq 0 ]
