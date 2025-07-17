//! Source code to tokens lexing logic

use core::fmt;
use std::{cmp, str::Chars};

use crate::ast::Ident;
use crate::session::{SessionCtx, Symbol};

#[allow(clippy::enum_glob_use)]
use self::{BinOp::*, Delimiter::*, Keyword::*, LiteralKind::*, TokenKind::*};

#[derive(Debug, PartialEq, Eq)]
pub enum Spacing {
	Alone,
	Joint,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
}

impl Span {
	const DUMMY: Self = Self::new(u32::MAX, u32::MAX);

	#[must_use]
	pub const fn new(start: u32, end: u32) -> Self {
		Self { start, end }
	}

	#[must_use]
	pub fn to(self, span: Self) -> Self {
		Self {
			start: cmp::min(self.start, span.start),
			end: cmp::max(self.end, span.end),
		}
	}

	#[must_use]
	pub const fn start(self) -> Self {
		Self {
			start: self.start,
			end: self.start,
		}
	}

	#[must_use]
	pub const fn end(self) -> Self {
		Self {
			start: self.end,
			end: self.end,
		}
	}
}

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "s#{}..{}", self.start, self.end)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

impl Token {
	pub const DUMMY: Self = Self::new(Eof, Span::DUMMY);

	#[must_use]
	pub const fn new(kind: TokenKind, span: Span) -> Self {
		Self { kind, span }
	}

	fn maybe_glue_joint(&self, next: &Self) -> Option<Self> {
		let glued_kind = match (self.kind, next.kind) {
			(Eq, Eq) => BinOp(EqEq),
			(BinOp(Gt), Eq) => BinOp(Ge),
			(BinOp(Lt), Eq) => BinOp(Le),

			(BinOp(Minus), BinOp(Lt)) => Arrow,
			(Not, Eq) => BinOp(Ne),

			(_, _) => return None,
		};

		Some(Self::new(glued_kind, self.span.to(next.span)))
	}

	#[must_use]
	pub const fn as_ident(self) -> Option<Ident> {
		match self.kind {
			Ident(sym) => Some(Ident::new(sym, self.span)),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
	Ident(Symbol),
	Keyword(Keyword),
	Literal(LiteralKind, Symbol),

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
	Integer,
	Float,
	Str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
	// Lexemes are `Lt` and `Gt`
	Angled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
	Plus,
	Minus,
	Mul,
	Div,
	#[doc(alias = "Rem")]
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
	#[must_use]
	pub const fn precedence(self) -> u32 {
		match self {
			Self::Mul | Self::Div | Self::Mod => 4,
			Self::Minus | Self::Plus => 3,
			Self::Gt | Self::Ge | Self::Lt | Self::Le => 2,
			Self::Ne | Self::EqEq => 1,
		}
	}
}

const EOF_CHAR: char = '\0';

#[derive(Debug, Clone)]
pub struct Lexer<'fcx, 'src> {
	fcx: &'fcx SessionCtx,

	source: &'src str,
	chars: Chars<'src>,
	token: Option<char>,
	offset: usize,

	next_glued: Option<Token>,
}

impl<'fcx, 'src> Lexer<'fcx, 'src> {
	#[must_use]
	pub fn new(fcx: &'fcx SessionCtx, source: &'src str) -> Self {
		let chars = source.chars();
		Self {
			fcx,
			source,
			chars,
			token: None,
			offset: 0,

			next_glued: None,
		}
	}

	fn bump(&mut self) -> Option<char> {
		self.token = self.chars.next();
		self.offset += self.token.map_or(0, char::len_utf8);
		self.token
	}

	fn first(&self) -> char {
		// TODO: is the clone cheap? or should we have extra logic like peekable
		self.chars.clone().next().unwrap_or(EOF_CHAR)
	}

	fn bump_while(&mut self, mut cond: impl FnMut(char) -> bool) {
		while cond(self.first()) && !self.is_eof() {
			self.bump();
		}
	}

	fn str_from_to(&self, start: usize, end: usize) -> &str {
		&self.source[start..end]
	}

	fn str_from(&self, start: usize) -> &str {
		&self.source[start..self.offset]
	}

	fn is_eof(&self) -> bool {
		self.chars.as_str().is_empty()
	}
}

impl Lexer<'_, '_> {
	pub fn next_token(&mut self) -> Option<(Token, Spacing)> {
		let mut spacing = Spacing::Joint;

		loop {
			let start = self.offset;

			let kind = match self.bump()? {
				c if is_ident_start(c) => {
					self.bump_while(is_ident_continue);
					match self.str_from(start) {
						"fn" => Keyword(Fn),
						"extern" => Keyword(Extern),
						"const" => Keyword(Const),

						"if" => Keyword(If),
						"else" => Keyword(Else),

						"loop" => Keyword(Loop),
						"while" => Keyword(While),
						"for" => Keyword(For),
						"in" => Keyword(In),

						ident => TokenKind::Ident(self.fcx.symbols.intern(ident)),
					}
				}

				// Int or Float
				c if c.is_ascii_digit() => {
					self.bump_while(|c| char::is_ascii_digit(&c));
					let kind = if self.first() == '.' {
						self.bump();
						self.bump_while(|c| char::is_ascii_digit(&c));
						Float
					} else {
						Integer
					};
					Literal(kind, self.fcx.symbols.intern(self.str_from(start)))
				}

				'"' => {
					while let Some(c) = self.bump() {
						match c {
							'\\' if self.first() == '\\' || self.first() == '"' => {
								// skip escaped character
								self.bump();
							}
							'"' => break,
							_ => {}
						}
					}

					// strip quotes
					let symbol = self.str_from_to(start + 1, self.offset - 1);
					Literal(Str, self.fcx.symbols.intern(symbol))
				}

				// Non-significative whitespace
				c if c.is_ascii_whitespace() => {
					spacing = Spacing::Joint;
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
				'/' => match self.first() {
					'/' => {
						// eat the whole line
						self.bump_while(|c| c != '\n');
						spacing = Spacing::Joint;
						continue;
					}
					'*' => {
						// eat the star
						self.bump();
						self.skip_block_comment();
						spacing = Spacing::Joint;
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

			let span = Span::new(
				u32::try_from(start).unwrap(),
				u32::try_from(self.offset).unwrap(),
			);
			let token = Token { kind, span };
			return Some((token, spacing));
		}
	}

	fn next_token_glued(&mut self) -> Option<Token> {
		let mut token = self
			.next_glued
			.take()
			.or_else(|| self.next_token().map(|(tkn, _spacing)| tkn))?;

		loop {
			// maybe glue joint token if applicable
			if let Some((next, spacing)) = self.next_token() {
				if spacing == Spacing::Joint
					&& let Some(glued_token) = token.maybe_glue_joint(&next)
				{
					token = glued_token;
				} else {
					// save token for next iteration
					self.next_glued = Some(next);
					return Some(token);
				}
			} else {
				return Some(token);
			}
		}
	}

	fn skip_block_comment(&mut self) {
		let mut count = 0;

		// handle nested block comments
		while let Some(c) = self.bump() {
			match c {
				'/' if self.first() == '*' => count += 1,
				'*' if self.first() == '/' && count == 0 => {
					// eat the trailing slash
					self.bump();
					break;
				}
				'*' if self.first() == '/' => count -= 1,
				_ => {}
			}
		}
	}
}

impl Iterator for Lexer<'_, '_> {
	type Item = Token;
	fn next(&mut self) -> Option<Self::Item> {
		self.next_token_glued()
	}
}

const fn is_ident_start(c: char) -> bool {
	c.is_ascii_alphabetic() || c == '_'
}

const fn is_ident_continue(c: char) -> bool {
	c.is_ascii_alphanumeric() || c == '_'
}
