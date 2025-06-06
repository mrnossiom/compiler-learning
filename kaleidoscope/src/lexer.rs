use std::{iter::Peekable, str::Chars};

use crate::Ident;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
	Ident(Ident),

	Number(i64),

	Operator(Operator),

	Delimiter(char),
	Comma,

	// Keywords
	Def,
	Extern,

	If,
	Then,
	Else,

	For,
	In,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
	Plus,
	Minus,
	Mul,
	Div,

	Gt,
	Lt,

	Eq,
}

impl Operator {
	pub const fn precedence(self) -> u32 {
		match self {
			Self::Mul | Self::Div => 40,
			Self::Minus | Self::Plus => 20,
			Self::Gt | Self::Lt => 10,
			Self::Eq => 5,
		}
	}
}

pub struct Lexer<'a> {
	chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
	pub fn new(code: &'a str) -> Self {
		Self {
			chars: code.chars().peekable(),
		}
	}

	pub fn next_token(&mut self) -> Option<Token> {
		loop {
			let chr = self.chars.next()?;

			let token = match chr {
				'#' => {
					// Eat the whole line
					while self.chars.next_if(|c| *c != '\n').is_some() {}
					continue;
				}

				c @ ('(' | ')') => Token::Delimiter(c),

				c if c.is_alphabetic() => {
					let mut ident = c.to_string();
					while let Some(c) = self.chars.next_if(char::is_ascii_alphanumeric) {
						ident.push(c);
					}
					match ident.as_str() {
						"def" => Token::Def,
						"extern" => Token::Extern,

						"if" => Token::If,
						"then" => Token::Then,
						"else" => Token::Else,

						"for" => Token::For,
						"in" => Token::In,
						_ => Token::Ident(Ident(ident)),
					}
				}
				c if c.is_ascii_digit() => {
					let mut num = c.to_string();
					while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
						num.push(c);
					}
					Token::Number(num.parse().unwrap())
				}
				c if c.is_ascii_whitespace() => {
					continue;
				}

				'+' => Token::Operator(Operator::Plus),
				'-' => Token::Operator(Operator::Minus),
				'*' => Token::Operator(Operator::Mul),
				'/' => Token::Operator(Operator::Div),

				'>' => Token::Operator(Operator::Gt),
				'<' => Token::Operator(Operator::Lt),
				'=' => Token::Operator(Operator::Eq),

				',' => Token::Comma,

				_ => todo!(),
			};

			return Some(token);
		}
	}
}

impl Iterator for Lexer<'_> {
	type Item = Token;
	fn next(&mut self) -> Option<Self::Item> {
		self.next_token()
	}
}
