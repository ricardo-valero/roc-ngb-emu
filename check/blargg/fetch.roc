# Fetch the Blargg conformance ROMs (retrio/gb-test-roms, pinned by commit)
# into check/blargg/data/, verifying each file's SHA-256 so the pin is
# content-addressed like the nix fetch it replaces.
#
#   roc check/blargg/fetch.roc
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

base : Str
base = "https://raw.githubusercontent.com/retrio/gb-test-roms/c240dd7d700e5c0b00a7bbba52b53e4ee67b5f15"

data_dir : Str
data_dir = "check/blargg/data"

files : List({ name : Str, url_path : Str, sha256 : Str })
files = [
	{ name: "cpu_instrs.gb", url_path: "cpu_instrs/cpu_instrs.gb", sha256: "8c5e12f41e0ba5bbca796944f92ffe6de28809198682c4332e38d1b3cf56fcf2" },
	{ name: "01-special.gb", url_path: "cpu_instrs/individual/01-special.gb", sha256: "fe61349cbaee10cc384b50f356e541c90d1bc380185716706b5d8c465a03cf89" },
	{ name: "02-interrupts.gb", url_path: "cpu_instrs/individual/02-interrupts.gb", sha256: "fb90b0d2b9501910c49709abda1d8e70f757dc12020ebf8409a7779bbfd12229" },
	{ name: "03-op sp,hl.gb", url_path: "cpu_instrs/individual/03-op%20sp,hl.gb", sha256: "ca553e606d9b9c86fbd318f1b916c6f0b9df0cf1774825d4361a3fdff2e5a136" },
	{ name: "04-op r,imm.gb", url_path: "cpu_instrs/individual/04-op%20r,imm.gb", sha256: "7686aa7a39ef3d2520ec1037371b5f94dc283fbbfd0f5051d1f64d987bdd6671" },
	{ name: "05-op rp.gb", url_path: "cpu_instrs/individual/05-op%20rp.gb", sha256: "d504adfa0a4c4793436a154f14492f044d38b3c6db9efc44138f3c9ad138b775" },
	{ name: "06-ld r,r.gb", url_path: "cpu_instrs/individual/06-ld%20r,r.gb", sha256: "17ada54b0b9c1a33cd5429fce5b765e42392189ca36da96312222ffe309e7ed1" },
	{ name: "07-jr,jp,call,ret,rst.gb", url_path: "cpu_instrs/individual/07-jr,jp,call,ret,rst.gb", sha256: "ab31d3daaaa3a98bdbd9395b64f48c1bdaa889aba5b19dd5aaff4ec2a7d228a3" },
	{ name: "08-misc instrs.gb", url_path: "cpu_instrs/individual/08-misc%20instrs.gb", sha256: "974a71fe4c67f70f5cc6e98d4dc8c096057ff8a028b7bfa9f7a4330038cf8b7e" },
	{ name: "09-op r,r.gb", url_path: "cpu_instrs/individual/09-op%20r,r.gb", sha256: "b28e1be5cd95f22bd1ecacdd33c6f03e607d68870e31a47b15a0229033d5ba2a" },
	{ name: "10-bit ops.gb", url_path: "cpu_instrs/individual/10-bit%20ops.gb", sha256: "7f5b8e488c6988b5aaba8c2a74529b7c180c55a58449d5ee89d606a07c53514a" },
	{ name: "11-op a,(hl).gb", url_path: "cpu_instrs/individual/11-op%20a,(hl).gb", sha256: "0ec0cf9fda3f00becaefa476df6fb526c434abd9d4a4beac237c2c2692dac5d3" },
	{ name: "instr_timing.gb", url_path: "instr_timing/instr_timing.gb", sha256: "646067b3d6c79fda810e9c3f1cb7c0efd5abb0a7ac06437c54e65720c15d9925" },
	{ name: "mem_timing.gb", url_path: "mem_timing/mem_timing.gb", sha256: "791cb418fd054b482a6ada5017e2405b96c2373a0f3914c6b23afeee76fa98ce" },
	{ name: "01-read_timing.gb", url_path: "mem_timing/individual/01-read_timing.gb", sha256: "52724532c5709e38e947eb429337c124c38bc68f373874435a7460548098b617" },
	{ name: "02-write_timing.gb", url_path: "mem_timing/individual/02-write_timing.gb", sha256: "eea92d3f4e95aab5910e0f7080916a3c42a2b8deae1ee5d45d1e3751d648f3f6" },
	{ name: "03-modify_timing.gb", url_path: "mem_timing/individual/03-modify_timing.gb", sha256: "2e9067c670ff8b45916bf321677ad04a6896d06a057dbcb82ae9f208a1ae9c34" },
	{ name: "01-registers.gb", url_path: "dmg_sound/rom_singles/01-registers.gb", sha256: "c6b9fa4b9d9d26919b33ebe78a6ef19ad2df854186cf741ca2746179cc9fc3f1" },
	{ name: "02-len ctr.gb", url_path: "dmg_sound/rom_singles/02-len%20ctr.gb", sha256: "745544125a5065729cab22494a79f65c3836e4426f89e4cc43d330afe711b413" },
	{ name: "03-trigger.gb", url_path: "dmg_sound/rom_singles/03-trigger.gb", sha256: "bb11e7266a7143bafb8aa2a73ca70957c6011f36cd4e0b6aaf0678378294e75c" },
	{ name: "04-sweep.gb", url_path: "dmg_sound/rom_singles/04-sweep.gb", sha256: "58bc14541d91bb020c7761b423825b3432cfde7ee2fa4d1116126e4cb9573c7e" },
	{ name: "05-sweep details.gb", url_path: "dmg_sound/rom_singles/05-sweep%20details.gb", sha256: "f582ca3a0b2544b9510797d7d4dd56a17f53e000511b0ae88a6353200dcd9167" },
	{ name: "06-overflow on trigger.gb", url_path: "dmg_sound/rom_singles/06-overflow%20on%20trigger.gb", sha256: "1a511e95e84ed6fe6077cabf451a98b4e01fdef59cadae3efbc178113eb064b1" },
	{ name: "07-len sweep period sync.gb", url_path: "dmg_sound/rom_singles/07-len%20sweep%20period%20sync.gb", sha256: "56bf5b0c18b996929c9b052ba5a02b450cb619bb6c24cc1d34ea20951c977bf7" },
	{ name: "08-len ctr during power.gb", url_path: "dmg_sound/rom_singles/08-len%20ctr%20during%20power.gb", sha256: "31cb41f7be106a708ec0bc94f2a9d0b506d247cd591e9d0a2a6a960dc3bf6595" },
	{ name: "09-wave read while on.gb", url_path: "dmg_sound/rom_singles/09-wave%20read%20while%20on.gb", sha256: "378b86f6a25daa16855260d7ef0c24e48146ba05df434a0b5413983a1447e875" },
	{ name: "10-wave trigger while on.gb", url_path: "dmg_sound/rom_singles/10-wave%20trigger%20while%20on.gb", sha256: "fa63c8ed7473411e54285d318e33bf23ff6d637ed2caa7555ddeaf80578e3279" },
	{ name: "11-regs after power.gb", url_path: "dmg_sound/rom_singles/11-regs%20after%20power.gb", sha256: "d27dab46e8b881028723f1975328572d38e25d295289b974cd698916a0be5dab" },
	{ name: "12-wave write while on.gb", url_path: "dmg_sound/rom_singles/12-wave%20write%20while%20on.gb", sha256: "2efbecd2c6d40928d44f45da4f634626bcd2790165b3949aedfe377c73913774" },
]

fetch_one! = |file| {
	path = Path.utf8("${data_dir}/${file.name}")
	if (path.is_file!() ?? Bool.False) and (Sha256.hex(path.read_bytes!() ?? []) == file.sha256) {
		Stdout.line!("${file.name}: already present")
	} else {
		response = Http.send!(Request.from_method(GET).with_uri("${base}/${file.url_path}"))?
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
	Stdout.line!("blargg test data present in ${data_dir}/")
}
