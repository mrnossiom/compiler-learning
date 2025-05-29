use std::{iter::Peekable, str::Chars};

use crate::Ident;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
	Ident(Ident),

	Number(u64),

	Operator(char),

	Delimiter(char),

	// Keywords
	Def,
	Extern,
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

				// else make count as a custom op
				c => Token::Operator(c),
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
