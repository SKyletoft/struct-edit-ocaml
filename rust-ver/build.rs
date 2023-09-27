use std::path::PathBuf;

fn main() {
	let dir: PathBuf = ["tree-sitter-typescript", "typescript", "src"]
		.iter()
		.collect();

	cc::Build::new()
		.include(&dir)
		.file(dir.join("parser.c"))
		.file(dir.join("scanner.c"))
		.opt_level(3)
		.warnings(false)
		.compile("tree-sitter-typescript");
}
