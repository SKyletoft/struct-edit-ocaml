use std::{io::stdout, ops::Range};

use crossterm::{
	cursor::MoveTo,
	event::{self, read, KeyCode, KeyEvent},
	style::*,
	terminal::{Clear, ClearType},
	ExecutableCommand,
};
use tree_sitter::{Language, Parser};

extern "C" {
	fn tree_sitter_typescript() -> Language;
}

type MovementRes = Result<(), ()>;

pub struct Cursor<'a> {
	ts_cur: tree_sitter::TreeCursor<'a>,
	cursor_stack: Vec<u8>,
}

impl<'a> Cursor<'a> {
	pub fn new(t: &'a tree_sitter::Tree) -> Self {
		Cursor {
			ts_cur: t.root_node().walk(),
			cursor_stack: Default::default(),
		}
	}

	pub fn up(&mut self) -> MovementRes {
		if self.ts_cur.goto_parent() {
			self.cursor_stack
				.pop()
				.expect("Should be in sync with ts_cur");
		}
		Ok(())
	}

	pub fn down(&mut self) -> MovementRes {
		if self.ts_cur.goto_first_child() {
			self.cursor_stack.push(0);
		}
		Ok(())
	}

	pub fn forwards(&mut self) -> MovementRes {
		if self.ts_cur.goto_next_sibling() {
			*self.cursor_stack.last_mut().unwrap() += 1;
		} else {
			self.up()?;
			self.forwards()?;
			self.down()?;
		}
		Ok(())
	}

	pub fn backwards(&mut self) -> MovementRes {
		match self.cursor_stack.last().copied() {
			None => Err(()),
			Some(0) => {
				self.up()?;
				self.backwards()?;
				self.down()?;
				let mut steps = 0;
				while self.ts_cur.goto_next_sibling() {
					steps += 1;
				}
				*self.cursor_stack.last_mut().unwrap() = steps;
				Ok(())
			}
			Some(n) => {
				self.up()?;
				self.down()?;
				for _ in 0..(n - 1) {
					self.forwards()?;
				}
				Ok(())
			}
		}
	}

	pub fn byte_range(&self) -> Range<usize> {
		self.ts_cur.node().byte_range()
	}
}

fn run() -> std::io::Result<()> {
	let mut parser = Parser::new();
	let language = unsafe { tree_sitter_typescript() };
	parser.set_language(language).unwrap();

	let source = include_str!("example.ts");

	let out = parser.parse(source, None).unwrap();
	let mut cur = Cursor::new(&out);

	loop {
		let range = cur.byte_range();
		let start = range.start;
		let end = range.end;
		let before = &source[..start];
		let highlight = &source[range];
		let after = &source[end..];

		let stdout = stdout();
		let mut lock = stdout.lock();
		lock.execute(ResetColor)?
			.execute(Clear(ClearType::All))?
			.execute(MoveTo(1, 1))?;

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
				let _ = cur.forwards();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('a'),
				..
			}) => {
				let _ = cur.backwards();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('s'),
				..
			}) => {
				let _ = cur.down();
			}
			event::Event::Key(KeyEvent {
				code: KeyCode::Char('w'),
				..
			}) => {
				let _ = cur.up();
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
