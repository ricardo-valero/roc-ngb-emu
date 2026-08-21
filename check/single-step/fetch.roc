# Fetch the SingleStepTests SM83 vectors into check/single-step/data/ —
# pure Roc, no curl required. One JSON file per opcode, ~1000 generated
# cases each: 244 base files (the 0xCB prefix byte and the 11 SM83
# illegals have none) plus 256 CB-prefixed files. Upstream names those
# "cb XX.json"; locally they land as cb-XX.json so paths stay space-free.
#
#   roc check/single-step/fetch.roc
#
# Already-present files are skipped, so an interrupted fetch resumes.
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	http: "https://github.com/roc-lang/http/releases/download/1.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.OsStr
import pf.Http
import pf.Path
import pf.Stdout
import http.Request

base_url : Str
base_url = "https://raw.githubusercontent.com/SingleStepTests/sm83/main/v1"

data_dir : Str
data_dir = "check/single-step/data"

# Opcodes with no vector file: the CB prefix byte and the SM83 illegals.
no_vector : List(U64)
no_vector = [0xCB, 0xD3, 0xDB, 0xDD, 0xE3, 0xE4, 0xEB, 0xEC, 0xED, 0xF4, 0xFC, 0xFD]

is_skipped : U64 -> Bool
is_skipped = |opcode| no_vector.fold(Bool.False, |acc, s| acc or s == opcode)

hex2 : U64 -> Str
hex2 = |n| {
	digit = |d| if d < 10 {
		d.plus(48)
	} else {
		d.plus(87)
	} # '0'.. / 'a'..
	hi = n.to_u8_wrap().shr_zf_wrap(4).to_u64()
	lo = n.bitwise_and(15)
	Str.from_utf8([digit(hi).to_u8_wrap(), digit(lo).to_u8_wrap()]) ?? "??"
}

# Indirection on purpose: the flow analyzer constant-folds a `?? fallback`
# on an effectful call at the use site and warns; behind an effectful
# helper it does not (same workaround as the check runners).
present! = |path| path.is_file!() ?? Bool.False

fetch_one! = |url_name, file_name| {
	path = Path.from_os_str(OsStr.from_str("${data_dir}/${file_name}"))
	if present!(path) {
		Stdout.line!("${file_name}: already present")
	} else {
		response = Http.send!(Request.from_method(GET).with_uri("${base_url}/${url_name}"))?
		if response.status() == 200 {
			path.write_bytes!(response.body())?
			Stdout.line!("${file_name}: fetched (${response.body().len().to_str()} bytes)")
		} else {
			Stdout.line!("${file_name}: HTTP ${response.status().to_str()}")?
			Err(FetchFailed(file_name))
		}
	}
}

fetch_base! = |opcode|
	if opcode > 255 {
		Ok({})
	} else if is_skipped(opcode) {
		fetch_base!(opcode.plus(1))
	} else {
		name = hex2(opcode)
		fetch_one!("${name}.json", "${name}.json")?
		fetch_base!(opcode.plus(1))
	}

fetch_cb! = |opcode|
	if opcode > 255 {
		Ok({})
	} else {
		name = hex2(opcode)
		fetch_one!("cb%20${name}.json", "cb-${name}.json")?
		fetch_cb!(opcode.plus(1))
	}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	fetch_base!(0)?
	fetch_cb!(0)?
	Stdout.line!("all 500 vector files present in ${data_dir}/")
}

expect hex2(0) == "00"
expect hex2(0xCB) == "cb"
expect hex2(255) == "ff"
expect is_skipped(0xCB)
expect is_skipped(0xFD)
expect !is_skipped(0x00)
