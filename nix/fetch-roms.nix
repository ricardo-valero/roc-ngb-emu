# Fetch the not-yet-sliced test ROMs into ./rom (untracked): dmg_sound,
# dmg-acid2, and the accuracy-ladder set. cpu_instrs now lives in the
# verify/blargg slice; the rest migrates as their suites become slices.
# Sources: https://github.com/retrio/gb-test-roms (Blargg),
# https://gekkio.fi/files/mooneye-test-suite/ (mooneye acceptance ROMs)
{
  writeShellApplication,
  curl,
  gnutar,
  xz,
}:
writeShellApplication {
  name = "fetch-roms";
  runtimeInputs = [curl gnutar xz];
  text = ''
    # dmg_sound: APU test ROMs (01-registers is the conformance gate)
    snd="$PWD/rom/dmg_sound"
    snd_base="https://github.com/retrio/gb-test-roms/raw/master/dmg_sound/rom_singles"
    snd_roms=(
      "01-registers.gb"
      "02-len ctr.gb"
      "03-trigger.gb"
      "04-sweep.gb"
      "05-sweep details.gb"
      "06-overflow on trigger.gb"
      "07-len sweep period sync.gb"
      "08-len ctr during power.gb"
      "09-wave read while on.gb"
      "10-wave trigger while on.gb"
      "11-regs after power.gb"
      "12-wave write while on.gb"
    )
    mkdir -p "$snd"
    for rom in "''${snd_roms[@]}"; do
      if [ ! -f "$snd/$rom" ]; then
        echo "fetching dmg_sound/$rom"
        curl -fsSL "$snd_base/''${rom// /%20}" -o "$snd/$rom"
      fi
    done

    # dmg-acid2: PPU rendering oracle (MIT, mattcurrie/dmg-acid2)
    acid="$PWD/rom/dmg-acid2.gb"
    if [ ! -f "$acid" ]; then
      echo "fetching dmg-acid2.gb"
      curl -fsSL "https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb" -o "$acid"
    fi

    # accuracy ladder: Blargg timing ROMs
    lad="$PWD/rom/ladder/blargg"
    mkdir -p "$lad"
    lad_roms=(
      "instr_timing/instr_timing.gb"
      "mem_timing/mem_timing.gb"
      "mem_timing/individual/01-read_timing.gb"
      "mem_timing/individual/02-write_timing.gb"
      "mem_timing/individual/03-modify_timing.gb"
    )
    for path in "''${lad_roms[@]}"; do
      rom="$(basename "$path")"
      if [ ! -f "$lad/$rom" ]; then
        echo "fetching ladder/blargg/$rom"
        curl -fsSL "https://github.com/retrio/gb-test-roms/raw/master/''${path// /%20}" -o "$lad/$rom"
      fi
    done

    # accuracy ladder: mooneye halt/timer acceptance ROMs (official tarball)
    moon="$PWD/rom/ladder/mooneye"
    mts="mts-20240926-1737-443f6e1"
    if [ ! -f "$moon/.done" ]; then
      echo "fetching mooneye test suite ($mts)"
      mkdir -p "$moon"
      tmp="$(mktemp -d)"
      curl -fsSL "https://gekkio.fi/files/mooneye-test-suite/$mts/$mts.tar.xz" -o "$tmp/mts.tar.xz"
      tar -xJf "$tmp/mts.tar.xz" -C "$tmp"
      cp "$tmp/$mts"/acceptance/timer/*.gb "$moon/"
      cp "$tmp/$mts"/acceptance/halt_ime0_ei.gb \
         "$tmp/$mts"/acceptance/halt_ime0_nointr_timing.gb \
         "$tmp/$mts"/acceptance/halt_ime1_timing.gb "$moon/"
      rm -rf "$tmp"
      touch "$moon/.done"
    fi

    echo "ROMs ready in $PWD/rom"
  '';
}
