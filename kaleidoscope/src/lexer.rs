//! Source code to tokens lexing logic

use core::fmt;
use std::str::Chars;

use crate::ast::Ident;
use crate::session::{BytePos, SessionCtx, Span, Symbol};

#[allow(clippy::enum_glob_use)]
use self::{BinaryOp::*, Delimiter::*, Keyword::*, LiteralKind::*, TokenKind::*, UnaryOp::*};

#[derive(Debug, PartialEq, Eq)]
pub enum Spacing {
	Alone,
	Joint,
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
			(Eq, Eq) => BinaryOp(EqEq),
			(BinaryOp(Gt), Eq) => BinaryOp(Ge),
			(BinaryOp(Lt), Eq) => BinaryOp(Le),

			(BinaryOp(Minus), BinaryOp(Lt)) => Arrow,
			(UnaryOp(Not), Eq) => BinaryOp(Ne),

			(BinaryOp(Lt), BinaryOp(Lt)) => BinaryOp(Shl),
			(BinaryOp(Gt), BinaryOp(Gt)) => BinaryOp(Shr),

			(Ampersand, Ampersand) => todo!("for recovery, see `and` kw"),
			(BinaryOp(Or), BinaryOp(Or)) => todo!("for recovery, see `or` kw"),

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

	UnaryOp(UnaryOp),
	BinaryOp(BinaryOp),

	Open(Delimiter),
	Close(Delimiter),

	/// `->`
	Arrow,
	Comma,
	Colon,
	Semi,
	Dot,
	/// `&`
	Ampersand,

	/// `=`
	Eq,

	Unknown,

	/// Used to reduce boilerplate with Option
	Eof,
}

impl fmt::Display for TokenKind {
	/// Should fit in the sentence "found {}"
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Ident(_) => write!(f, "an identifier"),
			Keyword(_) => write!(f, "a keyword"),
			Literal(kind, _) => write!(f, "a {kind} literal"),

			UnaryOp(kind) => write!(f, "{kind}"),
			BinaryOp(kind) => write!(f, "{kind}"),

			Open(kind) => write!(f, "an opening {kind}"),
			Close(kind) => write!(f, "a closing {kind}"),

			Arrow => write!(f, "an arrow"),
			Comma => write!(f, "a comma"),
			Colon => write!(f, "a colon"),
			Semi => write!(f, "a semicolon"),
			Dot => write!(f, "a dot"),
			Ampersand => write!(f, "an ampersand"),

			Eq => write!(f, "an assign sign"),

			Unknown => write!(f, "an unknown token"),
			Eof => write!(f, "the end of the file"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
	Integer,
	Float,
	Str,
}

impl fmt::Display for LiteralKind {
	/// Should fit in the sentence "a {} literal"
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Integer => write!(f, "integer"),
			Float => write!(f, "float"),
			Str => write!(f, "string"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	Fn,
	Extern,

	Var,
	Cst,

	If,
	Else,
	Is,

	Loop,
	While,
	In,
	For,

	Return,
	Break,
	Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
	Paren,
	Bracket,
	Brace,
	// Lexemes are `Lt` and `Gt`
	Angled,
}

impl fmt::Display for Delimiter {
	/// Should fit in the sentence "an opening {}" / "a closing {}"
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Paren => write!(f, "parenthesis"),
			Bracket => write!(f, "bracket"),
			Brace => write!(f, "brace"),
			Angled => write!(f, "angle bracket"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
	/// `!`
	Not,
}

impl fmt::Display for UnaryOp {
	/// Should fit in the sentence "found {}"
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Not => write!(f, "a negate sign"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
	// Arithmetic
	/// `+`
	Plus,
	/// `-`
	Minus,
	/// `*`
	Mul,
	/// `/`
	Div,
	/// `%`
	///
	/// Also commonly known as `Rem`
	#[doc(alias = "Rem")]
	Mod,

	// Bitwise
	/// `&`
	And,
	/// `|`
	Or,
	/// `^`
	Xor,

	/// `<<`
	Shl,
	/// `>>`
	Shr,

	// Compairaison
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

impl fmt::Display for BinaryOp {
	/// Should fit in the sentence "found {}"
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Plus => write!(f, "a plus operator"),
			Minus => write!(f, "a minus operator"),
			Mul => write!(f, "a multiplication operator"),
			Div => write!(f, "a division operator"),
			Mod => write!(f, "a modulo operator"),

			And => write!(f, "an and operator"),
			Or => write!(f, "an or operator"),
			Xor => write!(f, "a xor operator"),

			Shl => write!(f, "a shift left operator"),
			Shr => write!(f, "a shift right operator"),

			Gt => write!(f, "a greater than comparator"),
			Ge => write!(f, "a greater or equal comparator"),
			Lt => write!(f, "a lesser than comparator"),
			Le => write!(f, "a lesser or equal comparator"),

			EqEq => write!(f, "a equal comparator"),
			Ne => write!(f, "a different comparator"),
		}
	}
}

impl BinaryOp {
	#[must_use]
	pub const fn precedence(self) -> u32 {
		match self {
			Self::Mul | Self::Div | Self::Mod => 48,
			Self::Minus | Self::Plus => 40,
			Self::Shl | Self::Shr => 32,
			Self::And | Self::Or | Self::Xor => 24,
			Self::Gt | Self::Ge | Self::Lt | Self::Le => 16,
			Self::Ne | Self::EqEq => 8,
		}
	}
}

const EOF_CHAR: char = '\0';

#[derive(Debug, Clone)]
pub struct Lexer<'scx, 'src> {
	scx: &'scx SessionCtx,

	source: &'src str,
	chars: Chars<'src>,
	token: Option<char>,
	offset: BytePos,

	next_glued: Option<Token>,
}

impl<'scx, 'src> Lexer<'scx, 'src> {
	#[must_use]
	pub fn new(scx: &'scx SessionCtx, source: &'src str, offset: BytePos) -> Self {
		let chars = source.chars();
		Self {
			scx,
			source,
			chars,
			token: None,
			offset,

			next_glued: None,
		}
	}

	fn bump(&mut self) -> Option<char> {
		self.token = self.chars.next();
		self.offset = self.offset + BytePos::from_usize(self.token.map_or(0, char::len_utf8));
		self.token
	}

	fn first(&self) -> char {
		// TODO: is the clone cheap? or should we have extra logic like peekable
		self.chars.clone().next().unwrap_or(EOF_CHAR)
	}

	fn second(&self) -> char {
		self.chars.clone().nth(1).unwrap_or(EOF_CHAR)
	}

	fn bump_while(&mut self, mut cond: impl FnMut(char) -> bool) {
		while cond(self.first()) && !self.is_eof() {
			self.bump();
		}
	}

	fn str_from_to(&self, start: BytePos, end: BytePos) -> &str {
		&self.source[start.to_usize()..end.to_usize()]
	}

	fn str_from(&self, start: BytePos) -> &str {
		&self.source[start.to_usize()..self.offset.to_usize()]
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
					// TODO: make kw an symbol wrapper with preinterned value
					match self.str_from(start) {
						"fn" => Keyword(Fn),
						"extern" => Keyword(Extern),

						"var" => Keyword(Var),
						"cst" => Keyword(Cst),

						"if" => Keyword(If),
						"else" => Keyword(Else),
						"is" => Keyword(Is),

						"loop" => Keyword(Loop),
						"while" => Keyword(While),
						"in" => Keyword(In),
						"for" => Keyword(For),

						"return" => Keyword(Return),
						"break" => Keyword(Break),
						"continue" => Keyword(Continue),

						ident => TokenKind::Ident(self.scx.symbols.intern(ident)),
					}
				}

				// Int or Float
				c if c.is_ascii_digit() => {
					self.bump_while(|c| char::is_ascii_digit(&c));
					// avoid to eat the dot if this is a mac call after
					let kind = if self.first() == '.' && !is_ident_start(self.second()) {
						self.bump();
						// TODO: ensure that the float indeed has a digit after the dot
						assert!(self.token.is_some_and(|c| char::is_ascii_digit(&c)));
						self.bump_while(|c| char::is_ascii_digit(&c));
						Float
					} else {
						Integer
					};
					Literal(kind, self.scx.symbols.intern(self.str_from(start)))
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
					let symbol = self.str_from_to(
						start + BytePos::from_u32(1),
						self.offset - BytePos::from_u32(1),
					);
					Literal(Str, self.scx.symbols.intern(symbol))
				}

				// Non-significative whitespace
				c if c.is_ascii_whitespace() => {
					spacing = Spacing::Alone;
					continue;
				}

				// Delimiters
				'(' => Open(Paren),
				')' => Close(Paren),
				'[' => Open(Bracket),
				']' => Close(Bracket),
				'{' => Open(Brace),
				'}' => Close(Brace),

				'+' => BinaryOp(Plus),
				'-' => BinaryOp(Minus),
				'*' => BinaryOp(Mul),
				'/' => match self.first() {
					'/' => {
						// eat the whole line
						self.bump_while(|c| c != '\n');
						spacing = Spacing::Alone;
						continue;
					}
					'*' => {
						// eat the star
						self.bump();
						self.skip_block_comment();
						spacing = Spacing::Alone;
						continue;
					}
					_ => BinaryOp(Div),
				},
				'%' => BinaryOp(Mod),

				'>' => BinaryOp(Gt),
				'<' => BinaryOp(Lt),
				'=' => Eq,

				'!' => UnaryOp(Not),

				',' => Comma,
				'.' => Dot,
				':' => Colon,
				';' => Semi,

				'&' => Ampersand,

				_ => Unknown,
			};

			let span = Span::new(start, self.offset);
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
