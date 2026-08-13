# Fetch the sound-check ROM (retrio/gb-test-roms, pinned by commit) into
# check/sound/data/, verifying its SHA-256 so the pin is content-addressed
# like the nix fetch it replaces.
#
#   roc check/sound/fetch.roc
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
	chk: "../lib/main.roc",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request
import chk.Sha256

url : Str
url = "https://raw.githubusercontent.com/retrio/gb-test-roms/c240dd7d700e5c0b00a7bbba52b53e4ee67b5f15/dmg_sound/rom_singles/01-registers.gb"

sha256 : Str
sha256 = "c6b9fa4b9d9d26919b33ebe78a6ef19ad2df854186cf741ca2746179cc9fc3f1"

rom_path : Path
rom_path = Path.utf8("check/sound/data/01-registers.gb")

# Indirection on purpose: the flow analyzer constant-folds a `?? fallback`
# on an effectful call at the use site and warns; behind an effectful
# helper it does not (same workaround as the check runners).
present_and_valid! : Path => Bool
present_and_valid! = |path| {
	exists = path.is_file!() ?? Bool.False
	exists and Sha256.hex(path.read_bytes!() ?? List.repeat(0x00.U8, 0)) == sha256
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	Path.utf8("check/sound/data").create_dir!() ?? {}
	if present_and_valid!(rom_path) {
		Stdout.line!("01-registers.gb: already present")
	} else {
		response = Http.send!(Request.from_method(GET).with_uri(url))?
		if response.status() == 200 {
			body = response.body()
			actual = Sha256.hex(body)
			if actual == sha256 {
				rom_path.write_bytes!(body)?
				Stdout.line!("01-registers.gb: fetched (${body.len().to_str()} bytes)")
			} else {
				Stdout.line!("01-registers.gb: SHA-256 mismatch (${actual})")?
				Err(HashMismatch)
			}
		} else {
			Stdout.line!("01-registers.gb: HTTP ${response.status().to_str()}")?
			Err(FetchFailed)
		}
	}
}
