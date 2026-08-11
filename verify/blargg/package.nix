# The blargg verify slice: fetches only the cpu_instrs ROMs and runs the
# full suite through the slice's runner. Invoke from the repo root as
# `nix run .#verify-blargg`.
{
  writeShellApplication,
  coreutils,
  curl,
  roc,
  fetch-lib,
}: let
  base = "https://github.com/retrio/gb-test-roms/raw/master/cpu_instrs";
  singles = [
    "01-special.gb"
    "02-interrupts.gb"
    "03-op sp,hl.gb"
    "04-op r,imm.gb"
    "05-op rp.gb"
    "06-ld r,r.gb"
    "07-jr,jp,call,ret,rst.gb"
    "08-misc instrs.gb"
    "09-op r,r.gb"
    "10-bit ops.gb"
    "11-op a,(hl).gb"
  ];
  roms =
    [
      {
        name = "cpu_instrs.gb";
        url = "${base}/cpu_instrs.gb";
      }
    ]
    ++ map (name: {
      inherit name;
      url = "${base}/individual/${name}";
    })
    singles;
in
  writeShellApplication {
    name = "verify-blargg";
    runtimeInputs = [coreutils curl roc];
    text = ''
      if [ ! -f verify/blargg/main.roc ]; then
        echo "verify-blargg: run from the repo root (verify/blargg/main.roc not found)" >&2
        exit 2
      fi

      ${fetch-lib.fetchRoms {
        dir = "cpu_instrs";
        inherit roms;
      }}

      failures=0
      total=0
      for rom in rom/cpu_instrs/*.gb; do
        total=$((total + 1))
        name="$(basename "$rom")"
        if out="$(roc run verify/blargg/main.roc -- "$rom" 2>&1)"; then
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
