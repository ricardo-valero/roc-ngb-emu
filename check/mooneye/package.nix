# Timing/halt conformance: the mooneye-gb acceptance subset, extracted from
# the official suite tarball and run through the shared suite runner
# against ./passlist. Run: nix run .#check-mooneye
{
  writeShellApplication,
  fetchurl,
  runCommand,
  xz,
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
    runtimeInputs = [roc];
    text = ''
      runner="$(mktemp -t roc-runner-XXXXXX)"
      roc build check/run.roc --output="$runner" >/dev/null
      "$runner" check/mooneye/passlist "${roms}"/*.gb
      rm -f "$runner"
    '';
  }
