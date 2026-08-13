# Fetch the acid2 test ROMs (mattcurrie/dmg-acid2 v1.0 and cgb-acid2 v1.1)
# into check/acid2/data/, verifying each SHA-256. GitHub release assets sit
# behind a redirect, which the http package does not follow on its own, so
# this app follows Location hops manually before verifying.
#
#   roc check/acid2/fetch.roc
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

data_dir : Str
data_dir = "check/acid2/data"

files : List({ name : Str, url : Str, sha256 : Str })
files = [
	{
		name: "dmg-acid2.gb",
		url: "https://github.com/mattcurrie/dmg-acid2/releases/download/v1.0/dmg-acid2.gb",
		sha256: "464e14b7d42e7feea0b7ede42be7071dc88913f75b9ffa444299424b63d1dff1",
	},
	{
		name: "cgb-acid2.gbc",
		url: "https://github.com/mattcurrie/cgb-acid2/releases/download/v1.1/cgb-acid2.gbc",
		sha256: "197fb0bcec544f0400527fc707e0a94f55435974986e6986b424ace5de81720e",
	},
]

location_of = |response|
	response.headers().fold(
		"",
		|acc, header|
			if header.name.with_ascii_lowercased() == "location" {
				header.value
			} else {
				acc
			},
	)

# GET following up to `hops` redirects (release assets redirect once).
get_following! = |uri, hops| {
	response = Http.send!(Request.from_method(GET).with_uri(uri))?
	status = response.status()
	if (status == 301 or status == 302 or status == 307 or status == 308) and hops > 0 {
		get_following!(location_of(response), hops.minus(1))
	} else {
		Ok(response)
	}
}

# Indirection on purpose: the flow analyzer constant-folds a `?? fallback`
# on an effectful call at the use site and warns; behind an effectful
# helper it does not (same workaround as the check runners).
present_and_valid! = |path, expected| {
	exists = path.is_file!() ?? Bool.False
	exists and Sha256.hex(path.read_bytes!() ?? List.repeat(0x00.U8, 0)) == expected
}

fetch_one! = |file| {
	path = Path.utf8("${data_dir}/${file.name}")
	if present_and_valid!(path, file.sha256) {
		Stdout.line!("${file.name}: already present")
	} else {
		response = get_following!(file.url, 3)?
		if response.status() == 200 {
			body = response.body()
			actual = Sha256.hex(body)
			if actual == file.sha256 {
				path.write_bytes!(body)?
				Stdout.line!("${file.name}: fetched (${body.len().to_str()} bytes)")
			} else {
				Stdout.line!("${file.name}: SHA-256 mismatch (${actual})")?
				Err(HashMismatch)
			}
		} else {
			Stdout.line!("${file.name}: HTTP ${response.status().to_str()}")?
			Err(FetchFailed)
		}
	}
}

fetch_all! = |idx|
	match files.get(idx) {
		Err(_) => Ok({})
		Ok(file) => {
			fetch_one!(file)?
			fetch_all!(idx.plus(1))
		}
	}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	Path.utf8(data_dir).create_dir!() ?? {}
	fetch_all!(0)?
	Stdout.line!("acid2 test data present in ${data_dir}/")
}
