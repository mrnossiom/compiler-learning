//! Source code to tokens lexing logic
use std::{collections::VecDeque, iter::Peekable, str::Chars};

#[allow(clippy::enum_glob_use)]
use self::{BinOp::*, Delimiter::*, Keyword::*, Literal::*, TokenKind::*};
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
	Ident(Ident),
	Keyword(Keyword),
	Literal(Literal),

	BinOp(BinOp),

	Open(Delimiter),
	Close(Delimiter),

	/// `->`
	Arrow,
	Comma,
	Colon,
	Semi,
	Dot,

	/// `!`
	Not,

	/// `=`
	Eq,

	Unknown,

	/// Used to reduce boilerplate with Option
	Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
	Integer(String),
	Float(String),
	Str(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
	Fn,
	Extern,
	Const,

	If,
	Else,

	Loop,
	While,
	For,
	In,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
	Paren,
	Bracket,
	Brace,
	Angled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
	Plus,
	Minus,
	Mul,
	Div,
	Mod,

	/// `>`
	Gt,
	/// `>=`
	Ge,
	/// `<`
	Lt,
	/// `<=`
	Le,

	/// `==`
	EqEq,
	/// `!=`
	Ne,
}

impl BinOp {
	pub const fn precedence(self) -> u32 {
		match self {
			Self::Mul | Self::Div | Self::Mod => 40,
			Self::Minus | Self::Plus => 30,
			Self::Gt | Self::Ge | Self::Lt | Self::Le => 20,
			Self::Ne | Self::EqEq => 10,
		}
	}
}

struct SimpleLexer<'a> {
	chars: Peekable<Chars<'a>>,
}

const fn is_ident_start(c: char) -> bool {
	c.is_ascii_alphabetic() || c == '_'
}

const fn is_ident_continue(c: char) -> bool {
	c.is_ascii_alphanumeric() || c == '_'
}

impl SimpleLexer<'_> {
	pub fn next_simple_token(&mut self) -> Option<TokenKind> {
		loop {
			let chr = self.chars.next()?;

			let token = match chr {
				'#' => {
					// Eat the whole line
					while self.chars.next_if(|c| *c != '\n').is_some() {}
					continue;
				}

				c if is_ident_start(c) => {
					let mut ident = c.to_string();
					while let Some(c) = self.chars.next_if(|c| is_ident_continue(*c)) {
						ident.push(c);
					}

					match ident.as_str() {
						"fn" => Keyword(Fn),
						"extern" => Keyword(Extern),
						"const" => Keyword(Const),

						"if" => Keyword(If),
						"else" => Keyword(Else),

						"loop" => Keyword(Loop),
						"while" => Keyword(While),
						"for" => Keyword(For),
						"in" => Keyword(In),
						_ => TokenKind::Ident(Ident(ident)),
					}
				}

				// Int or Float
				c if c.is_ascii_digit() => {
					let mut num = c.to_string();
					while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
						num.push(c);
					}

					if self.chars.next_if_eq(&'.').is_some() {
						num.push('.');
						while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
							num.push(c);
						}
						Literal(Float(num))
					} else {
						Literal(Integer(num))
					}
				}

				c @ '"' => {
					let mut content = c.to_string();
					// TODO: handle backslashes
					while let Some(c) = self.chars.next_if_eq(&'"') {
						content.push(c);
					}
					Literal(Str(content))
				}

				// Non-significative whitespace
				c if c.is_ascii_whitespace() => {
					continue;
				}

				// Delimiters
				'(' => Open(Paren),
				')' => Close(Paren),
				'[' => Open(Bracket),
				']' => Close(Bracket),
				'{' => Open(Brace),
				'}' => Close(Brace),

				'+' => BinOp(Plus),
				'-' => BinOp(Minus),
				'*' => BinOp(Mul),
				'/' => match self.chars.peek() {
					Some('*') => {
						// TODO: cleanup
						while let (Some(c1), Some(c2)) = (self.chars.next(), self.chars.peek())
							&& (c1, *c2) != ('*', '/')
						{}
						_ = self.chars.next();
						continue;
					}
					_ => BinOp(Div),
				},
				'%' => BinOp(Mod),

				'>' => BinOp(Gt),
				'<' => BinOp(Lt),
				'=' => Eq,

				'!' => Not,

				',' => Comma,
				'.' => Dot,
				':' => Colon,
				';' => Semi,

				_ => Unknown,
			};

			return Some(token);
		}
	}
}

impl Iterator for SimpleLexer<'_> {
	type Item = TokenKind;
	fn next(&mut self) -> Option<Self::Item> {
		self.next_simple_token()
	}
}

pub struct Lexer<'a> {
	inner: Peekable<SimpleLexer<'a>>,

	buffer: VecDeque<TokenKind>,
}

impl TokenKind {
	const fn maybe_glue(&self, other: &Self) -> Option<Self> {
		let glued = match (self, other) {
			(Eq, Eq) => BinOp(EqEq),
			(BinOp(Gt), Eq) => BinOp(Ge),
			(BinOp(Lt), Eq) => BinOp(Le),

			(BinOp(Minus), BinOp(Lt)) => Arrow,
			(Not, Eq) => BinOp(Ne),

			(_, _) => return None,
		};
		Some(glued)
	}
}

impl Lexer<'_> {
	fn next_token(&mut self) -> Option<TokenKind> {
		let mut token = self.inner.next()?;
		loop {
			let Some(next) = self.inner.peek() else {
				return Some(token);
			};

			if let Some(glued_token) = token.maybe_glue(next) {
				_ = self.inner.next();
				token = glued_token;
			} else {
				return Some(token);
			}
		}
	}
}

impl<'a> Lexer<'a> {
	pub fn new(code: &'a str) -> Self {
		let chars = code.chars().peekable();
		let inner = SimpleLexer { chars }.peekable();
		Self {
			inner,
			buffer: VecDeque::new(),
		}
	}

	pub fn peek(&mut self) -> TokenKind {
		if let Some(tkn) = self.buffer.front() {
			tkn.clone()
		} else if let Some(tkn) = self.next_token() {
			self.buffer.push_back(tkn.clone());
			tkn
		} else {
			Eof
		}
	}

	pub fn look_ahead(&mut self, n: usize) -> TokenKind {
		if self.buffer.len() <= n {
			// load more tokens
			for _ in self.buffer.len()..=n {
				if let Some(tkn) = self.next_token() {
					self.buffer.push_back(tkn);
				} else {
					break;
				}
			}

			self.buffer.back().cloned().unwrap_or(Eof)
		} else if let Some(tkn) = self.buffer.get(n) {
			tkn.clone()
		} else {
			Eof
		}
	}

	pub fn next(&mut self) -> TokenKind {
		self.buffer
			.pop_front()
			.or_else(|| self.next_token())
			.unwrap_or(Eof)
	}

	pub fn bump(&mut self) {
		_ = self.next();
	}
}
