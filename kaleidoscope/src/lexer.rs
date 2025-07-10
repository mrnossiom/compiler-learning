use std::{iter::Peekable, str::Chars};

#[allow(clippy::enum_glob_use)]
use self::{Delimiter::*, Keyword::*, Literal::*, Operator::*, TokenKind::*};
use crate::Ident;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
	Ident(Ident),
	Keyword(Keyword),
	Literal(Literal),

	Operator(Operator),

	Open(Delimiter),
	Close(Delimiter),

	/// `->`
	Arrow,
	Comma,
	Colon,
	Semi,
	Dot,

	/// `=`
	Eq,

	Unknown,

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
pub enum Operator {
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

	/// `!`
	Not,
}

impl Operator {
	pub const fn precedence(self) -> Option<u32> {
		match self {
			Self::Mul | Self::Div | Self::Mod => Some(40),
			Self::Minus | Self::Plus => Some(30),
			Self::Gt | Self::Ge | Self::Lt | Self::Le => Some(20),
			Self::Ne | Self::EqEq => Some(10),
			Self::Not => None,
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

				'+' => Operator(Plus),
				'-' => Operator(Minus),
				'*' => Operator(Mul),
				'/' => match self.chars.peek() {
					Some('*') => {
						// TODO: cleanup
						while let (Some(c1), Some(c2)) = (self.chars.next(), self.chars.peek())
							&& (c1, *c2) != ('*', '/')
						{}
						_ = self.chars.next();
						continue;
					}
					_ => Operator(Div),
				},
				'%' => Operator(Mod),

				'>' => Operator(Gt),
				'<' => Operator(Lt),
				'=' => Eq,

				'!' => Operator(Not),

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
}

impl Iterator for Lexer<'_> {
	type Item = TokenKind;
	fn next(&mut self) -> Option<Self::Item> {
		self.next_token()
	}
}

impl TokenKind {
	const fn maybe_glue(&self, other: &Self) -> Option<Self> {
		let glued = match (self, other) {
			(Eq, Eq) => Operator(EqEq),
			(Operator(Gt), Eq) => Operator(Ge),
			(Operator(Lt), Eq) => Operator(Le),

			(Operator(Minus), Operator(Lt)) => Arrow,
			(Operator(Not), Eq) => Operator(Ne),

			(_, _) => return None,
		};
		Some(glued)
	}
}

impl<'a> Lexer<'a> {
	pub fn new(code: &'a str) -> Self {
		let chars = code.chars().peekable();
		let inner = SimpleLexer { chars }.peekable();
		Self { inner }
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
