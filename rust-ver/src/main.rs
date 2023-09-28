use std::io::stdout;

use crossterm::{
	event::{self, read, KeyCode, KeyEvent},
	style::*,
	terminal::{Clear, ClearType},
	ExecutableCommand,
};
use tree_sitter::{Language, Parser};

extern "C" {
	fn tree_sitter_typescript() -> Language;
}

struct Cursor<'a> {
	ts_cur: tree_sitter::TreeCursor<'a>,
	cursor_stack: Vec<u8>,
}

fn run() -> std::io::Result<()> {
	let mut parser = Parser::new();
	let language = unsafe { tree_sitter_typescript() };
	parser.set_language(language).unwrap();

	let source = include_str!("example.ts");

	let out = parser.parse(source, None).unwrap();
	let root = out.root_node();

	let mut cur = out.walk();
	cur.goto_first_child();

	loop {
		let range = cur.node().byte_range();
		let start = range.start;
		let end = range.end;
		let before = &source[..start];
		let highlight = &source[range];
		let after = &source[end..];

		let stdout = stdout();
		let mut lock = stdout.lock();
		lock.execute(ResetColor)?.execute(Clear(ClearType::All))?;

		for line in before.lines() {
			lock.execute(Print("\n\r"))?.execute(Print(line))?;
		}
		lock.execute(SetAttribute(Attribute::Reverse))?;
		for line in highlight.lines() {
			lock.execute(Print("\n\r"))?.execute(Print(line))?;
		}
		lock.execute(SetAttribute(Attribute::NoReverse))?;
		for line in after.lines() {
			lock.execute(Print("\n\r"))?.execute(Print(line))?;
		}

		match read()? {
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('d'),
				..
			}) => {
				cur.goto_next_sibling();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('s'),
				..
			}) => {
				cur.goto_first_child();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('w'),
				..
			}) => {
				cur.goto_parent();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('q') | KeyCode::Esc,
				..
			}) => return Ok(()),
			_ => {}
		}
	}
}

fn main() {
	let _raw_mode = Raw::new();
	let _ = run();
}

/// Raw mode struct. This is a struct so we can rely on RAII to exit raw mode even if we panic
pub struct Raw(());
impl Raw {
	pub fn new() -> Self {
		crossterm::terminal::enable_raw_mode().unwrap();
		Self(())
	}
}
impl Default for Raw {
	fn default() -> Self {
		Self::new()
	}
}
impl Drop for Raw {
	fn drop(&mut self) {
		crossterm::terminal::disable_raw_mode().unwrap();
	}
}
