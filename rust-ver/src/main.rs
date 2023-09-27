use tree_sitter::{Language, Parser};

extern "C" {
	fn tree_sitter_typescript() -> Language;
}

fn main() {
	let mut parser = Parser::new();
	let language = unsafe { tree_sitter_typescript() };
	parser.set_language(language).unwrap();

	let source = include_str!("example.ts");
	
	let out = parser.parse(source, None).unwrap();
	dbg!(out);
}
