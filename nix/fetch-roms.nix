# Fetch Blargg's cpu_instrs individual test ROMs into ./rom (untracked).
# Source: https://github.com/retrio/gb-test-roms
{
  writeShellApplication,
  curl,
}:
writeShellApplication {
  name = "fetch-roms";
  runtimeInputs = [curl];
  text = ''
    dir="$PWD/rom/cpu_instrs"
    base="https://github.com/retrio/gb-test-roms/raw/master/cpu_instrs/individual"

    roms=(
      "cpu_instrs.gb"
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
    )

    mkdir -p "$dir"
    for rom in "''${roms[@]}"; do
      if [ ! -f "$dir/$rom" ]; then
        echo "fetching $rom"
        if [ "$rom" = "cpu_instrs.gb" ]; then
          curl -fsSL "https://github.com/retrio/gb-test-roms/raw/master/cpu_instrs/cpu_instrs.gb" -o "$dir/$rom"
        else
          curl -fsSL "$base/''${rom// /%20}" -o "$dir/$rom"
        fi
      fi
    done

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

    # rom/play.gb: the ROM the play app embeds at build time.
    # Seeded from dmg-acid2; drop any game ROM here to play it instead.
    if [ ! -f "$PWD/rom/play.gb" ]; then
      cp "$acid" "$PWD/rom/play.gb"
      echo "seeded rom/play.gb from dmg-acid2"
    fi
    echo "ROMs ready in $PWD/rom"
  '';
}
