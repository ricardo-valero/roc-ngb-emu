# Fetch the mooneye acceptance subset into check/mooneye/data/. Upstream
# distributes the suite only as one archive, so this downloads the pinned
# .tar.gz from gekkio.fi, verifies the archive SHA-256, then unpacks it in
# pure Roc (chk.Inflate gunzip + chk.Tar) and verifies each extracted ROM
# against its own pinned SHA-256.
#
#   roc check/mooneye/fetch.roc
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
import chk.Inflate
import chk.Sha256
import chk.Tar

mts : Str
mts = "mts-20240926-1737-443f6e1"

archive_url : Str
archive_url = "https://gekkio.fi/files/mooneye-test-suite/mts-20240926-1737-443f6e1/mts-20240926-1737-443f6e1.tar.gz"

archive_sha256 : Str
archive_sha256 = "e5b1ed3d928d879263f5b852e4ba20514550d5bc7559775b140e8df4ab4dd4b3"

data_dir : Str
data_dir = "check/mooneye/data"

# The acceptance subset: timer ROMs plus the three halt ROMs.
files : List({ name : Str, entry : Str, sha256 : Str })
files = [
	{ name: "div_write.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/div_write.gb", sha256: "2be1e4da6fa24b9123d2a8bae47dd0d6f5e97e1855186c0c0f49e6d213eebfff" },
	{ name: "rapid_toggle.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/rapid_toggle.gb", sha256: "59fe311058895c39475f74bcffb7d4f29272edb74e75d2cfe860c1a9d033b68a" },
	{ name: "tim00.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim00.gb", sha256: "2193036c1628efd9ba86e5729292ef716d6ff3178cfa2abb9797709cd40252e8" },
	{ name: "tim00_div_trigger.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim00_div_trigger.gb", sha256: "5cafdf474dfa7507b0db596118ad7bc65a6d5d652ee4232263dd028e7f2462b7" },
	{ name: "tim01.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim01.gb", sha256: "b6f5043eae7fd2b2c3dc098ff16f664c8eb5699523616d84274669cf90c17fe7" },
	{ name: "tim01_div_trigger.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim01_div_trigger.gb", sha256: "73c1e2677a2a122a285aa052e232153b133873c68cbcbf1cf41aa3d0a1b80a96" },
	{ name: "tim10.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim10.gb", sha256: "fe3b0b292341d5ff26c9db3f6c9f3ba8a3d6e8b63977c61767a457962bd1faed" },
	{ name: "tim10_div_trigger.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim10_div_trigger.gb", sha256: "d83c7acf20a0315486ed77db4f873db40fff6099564f8e250df66274a304b1b9" },
	{ name: "tim11.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim11.gb", sha256: "624fd3ad3ede2790095162cfa212e488825072a0ecc287ff0e88da6c5d7040f1" },
	{ name: "tim11_div_trigger.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tim11_div_trigger.gb", sha256: "3f60bc3d2ba63bd9209706332dd6d0ef59ad09ada8dede0caa9e2e5132ed102e" },
	{ name: "tima_reload.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tima_reload.gb", sha256: "1ca70c725bd1e027b07d3058839bd140eccddd9f4ca41305c4f8ab3acaff8a98" },
	{ name: "tima_write_reloading.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tima_write_reloading.gb", sha256: "7d9a6d5ada792596621f8bfdf257112887b2dd01d98e0f91a253afd6e05d0540" },
	{ name: "tma_write_reloading.gb", entry: "mts-20240926-1737-443f6e1/acceptance/timer/tma_write_reloading.gb", sha256: "e48ff98d4f363b92e92bdabe86253fcf63f648964e3a61e73d52aedcba3e5ab2" },
	{ name: "halt_ime0_ei.gb", entry: "mts-20240926-1737-443f6e1/acceptance/halt_ime0_ei.gb", sha256: "0768fd3e698047f5ec2631b5a830558f0ac14bd9cb0ef0ebcc409b00d69adb4d" },
	{ name: "halt_ime0_nointr_timing.gb", entry: "mts-20240926-1737-443f6e1/acceptance/halt_ime0_nointr_timing.gb", sha256: "40a1e614d77a881672b7c20420aed6b121ad9d3977d71a08fd9bb92ee8f010cf" },
	{ name: "halt_ime1_timing.gb", entry: "mts-20240926-1737-443f6e1/acceptance/halt_ime1_timing.gb", sha256: "09d9be4ebdd7645a6b208f18b1354f4b75420ae567dcf27ac404ed6f934d2efa" },
]

# Indirection on purpose: the flow analyzer constant-folds a `?? fallback`
# on an effectful call at the use site and warns; behind an effectful
# helper it does not (same workaround as the check runners).
present_and_valid! = |path, expected| {
	exists = path.is_file!() ?? Bool.False
	exists and Sha256.hex(path.read_bytes!() ?? List.repeat(0x00.U8, 0)) == expected
}

# Returns Try rather than a bare Bool: a recursive effectful function with
# a non-Try return segfaults the pinned nightly compiler.
all_present! = |idx|
	match files.get(idx) {
		Err(_) => Ok(Bool.True)
		Ok(file) =>
			if present_and_valid!(Path.utf8("${data_dir}/${file.name}"), file.sha256) {
				all_present!(idx.plus(1))
			} else {
				Ok(Bool.False)
			}
		}

find_entry = |entries, wanted|
	entries.fold(
		List.repeat(0x00.U8, 0),
		|acc, entry|
			if entry.name == wanted {
				entry.bytes
			} else {
				acc
			},
	)

extract_all! = |entries, idx|
	match files.get(idx) {
		Err(_) => Ok({})
		Ok(file) => {
			bytes = find_entry(entries, file.entry)
			actual = Sha256.hex(bytes)
			if actual == file.sha256 {
				Path.utf8("${data_dir}/${file.name}").write_bytes!(bytes)?
				Stdout.line!("${file.name}: extracted (${bytes.len().to_str()} bytes)")?
				extract_all!(entries, idx.plus(1))
			} else {
				Stdout.line!("${file.name}: SHA-256 mismatch after extraction (${actual})")?
				Err(HashMismatch)
			}
		}
	}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	Path.utf8(data_dir).create_dir!() ?? {}
	if all_present!(0)? {
		Stdout.line!("mooneye test data already present in ${data_dir}/")
	} else {
		Stdout.line!("downloading ${archive_url}")?
		response = Http.send!(Request.from_method(GET).with_uri(archive_url))?
		if response.status() != 200 {
			Stdout.line!("archive: HTTP ${response.status().to_str()}")?
			Err(FetchFailed)
		} else {
			archive = response.body()
			archive_actual = Sha256.hex(archive)
			if archive_actual != archive_sha256 {
				Stdout.line!("archive: SHA-256 mismatch (${archive_actual})")?
				Err(HashMismatch)
			} else {
				tar = Inflate.gunzip(archive)?
				extract_all!(Tar.entries(tar), 0)?
				Stdout.line!("mooneye test data present in ${data_dir}/")
			}
		}
	}
}
