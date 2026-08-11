# CPU conformance: Blargg's cpu_instrs plus his timing ROMs. ROMs listed in
# ./passlist gate (the passing set never shrinks); the rest report
# informatively until promoted. Run from the repo root: nix run .#check-blargg
{
  lib,
  writeShellApplication,
  fetchurl,
  coreutils,
  roc,
}: let
  base = "https://github.com/retrio/gb-test-roms/raw/master";
  roms = [
    { url = "${base}/cpu_instrs/cpu_instrs.gb"; hash = "sha256-jF4S9B4LpbvKeWlE+S/+beKICRmGgsQzLjjRs89W/PI="; }
    { url = "${base}/cpu_instrs/individual/01-special.gb"; hash = "sha256-/mE0nLruEMw4S1DzVuVByQ0bw4AYVxZwa12MRloDz4k="; }
    { url = "${base}/cpu_instrs/individual/02-interrupts.gb"; hash = "sha256-+5Cw0rlQGRDElwmr2h2OcPdX3BICDr+ECad3m7/RIik="; }
    { url = "${base}/cpu_instrs/individual/03-op sp,hl.gb"; hash = "sha256-ylU+YG2bnIb70xjxuRbG8LnfDPF3SCXUNho/3/LloTY="; }
    { url = "${base}/cpu_instrs/individual/04-op r,imm.gb"; hash = "sha256-doaqejnvPSUg7BA3NxtflNwoP7v9D1BR0fZNmHvdZnE="; }
    { url = "${base}/cpu_instrs/individual/05-op rp.gb"; hash = "sha256-1QSt+gpMR5NDahVPFEkvBE04s8bbnvxEE488mtE4t3U="; }
    { url = "${base}/cpu_instrs/individual/06-ld r,r.gb"; hash = "sha256-F62lSwucGjPNVCn85bdl5COSGJyjbaljEiIv/jCeftE="; }
    { url = "${base}/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb"; hash = "sha256-qzHT2qqjqYvb2TlbZPSMG9qoiaulsZ3Vqv9OwqfSKKM="; }
    { url = "${base}/cpu_instrs/individual/08-misc instrs.gb"; hash = "sha256-l0px/kxn9w9cxumNTcjAlgV/+KAot7+p96QzADjPi34="; }
    { url = "${base}/cpu_instrs/individual/09-op r,r.gb"; hash = "sha256-so4b5c2V8ivR7KzdM8bwPmB9aIcOMaR7FaAikDPVuio="; }
    { url = "${base}/cpu_instrs/individual/10-bit ops.gb"; hash = "sha256-f1uOSIxpiLWquowqdFKbfBgMVaWESdXuidYGoHxTUUo="; }
    { url = "${base}/cpu_instrs/individual/11-op a,(hl).gb"; hash = "sha256-DsDPn9o/AL7K76R232+1JsQ0q9nUpL6sI3wsJpLaxdM="; }
    { url = "${base}/instr_timing/instr_timing.gb"; hash = "sha256-ZGBns9bHn9qBDpw/HLfA79WrsKesBkN8VOZXIMFdmSU="; }
    { url = "${base}/mem_timing/mem_timing.gb"; hash = "sha256-eRy0GP0FS0gqatpQF+JAW5bCNzoPORTGsjr+7nb6mM4="; }
    { url = "${base}/mem_timing/individual/01-read_timing.gb"; hash = "sha256-UnJFMsVwnjjpR+tCkzfBJMOLxo83OHRDWnRgVICYthc="; }
    { url = "${base}/mem_timing/individual/02-write_timing.gb"; hash = "sha256-7qktP06VqrWRDg9wgJFqPEKiuN6uHuXUXR43UdZI8/Y="; }
    { url = "${base}/mem_timing/individual/03-modify_timing.gb"; hash = "sha256-LpBnxnD/i0WRa/MhZ3rQSmiW0GoFfby4KunyCKGunDQ="; }
  ];
  fetch = {url, hash}:
    fetchurl {
      inherit hash;
      url = lib.replaceStrings [" "] ["%20"] url;
      name = lib.replaceStrings [" " "," "(" ")"] ["_" "_" "_" "_"] (baseNameOf url);
    };
  # one "display name|store path" line per ROM
  romlist = lib.concatMapStringsSep "\n" (r: "${baseNameOf r.url}|${fetch r}") roms;
in
  writeShellApplication {
    name = "check-blargg";
    runtimeInputs = [coreutils roc];
    text = ''
      if [ ! -f check/run.roc ]; then
        echo "check-blargg: run from the repo root" >&2
        exit 2
      fi

      gate_fail=0
      promote=()
      while IFS="|" read -r name path; do
        if out="$(roc run check/run.roc -- "$path" 2>&1)"; then
          result=pass
        else
          result=fail
        fi
        if grep -qxF "$name" check/blargg/passlist; then
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

      echo "----"
      if [ "''${#promote[@]}" -gt 0 ]; then
        echo "promotable (add to check/blargg/passlist):"
        printf "  %s\n" "''${promote[@]}"
      fi
      if [ "$gate_fail" -ne 0 ]; then
        echo "FAILED: $gate_fail gating ROM(s) regressed"
        exit 1
      fi
      echo "ok ($(grep -c . check/blargg/passlist) gating)"
    '';
  }
