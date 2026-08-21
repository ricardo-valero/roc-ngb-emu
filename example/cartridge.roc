app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.21.0/4rAQg8kUYZ3Vksr4qMQHpaFYNiHSn9GgS7gVxghd1XYV.tar.zst",
	ngb: "../package/main.roc",
}

import pf.OsStr
import pf.Path
import pf.Stdout
import ngb.Header

main! : List(OsStr) => Try({}, _)
main! = |args| {
	rom_path = read_arg_file_path(args)?
	# rom_path2 = rom_path.join("gb") # or gbc
	is_file = rom_path.is_file!()?
	cart = cart_load!(rom_path)?
	if is_file {
		# Stdout.line!("File with path: ${rom_path.display()}")?
		Stdout.line!("Header: ${Str.inspect(cart)}")?
	} else {
		Stdout.line!("Invalid file")?
	}
	Ok({})
}

read_arg_file_path : List(OsStr) -> Try(Path, [FailedToReadArgs(Str), ..])
read_arg_file_path = |args|
	match args {
		[_, path_arg, ..] => Ok(Path.from_os_str(path_arg))
		_ => Err(FailedToReadArgs("expected path argument"))
	}

cart_load! = |path| {
	rom_data = path.read_bytes!()?
	Ok(Header.read(rom_data))
}
