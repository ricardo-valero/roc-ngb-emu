# Shared helpers for verify slices.
{lib}: {
  # Shell text that fetches each ROM into rom/<dir>/ if missing. The ROM
  # cache is runtime state in the working tree (untracked), not a store
  # path, so this composes into a writeShellApplication rather than
  # building a derivation. `roms` is a list of { name, url }.
  fetchRoms = {
    dir,
    roms,
  }:
    ''
      mkdir -p "$PWD/rom/${dir}"
    ''
    + lib.concatMapStrings ({
      name,
      url,
    }: ''
      if [ ! -f "$PWD/rom/${dir}/${name}" ]; then
        echo "fetching ${dir}/${name}"
        curl -fsSL "${lib.replaceStrings [" "] ["%20"] url}" -o "$PWD/rom/${dir}/${name}"
      fi
    '')
    roms;
}
