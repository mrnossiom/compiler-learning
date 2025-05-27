use crate::Ident;

#[derive(Debug)]
pub enum Token {
	Ident(Ident),

	Number(u64),

	LBracket,
	RBracket,

	GreaterThan,
	LowerThan,

	Minus,
	Plus,
}

pub fn lex(code: &str) -> Vec<Token> {
	let mut tokens = Vec::new();

	let mut chars = code.chars().peekable();

	loop {
		let Some(chr) = chars.next() else {
			break;
		};

		let token = match chr {
			'#' => {
				while chars.next_if(|c| *c != '\n').is_some() {}
				continue;
			}

			'(' => Token::LBracket,
			')' => Token::RBracket,
			'<' => Token::LowerThan,
			'>' => Token::GreaterThan,
			'-' => Token::Minus,
			'+' => Token::Plus,
			c if c.is_alphabetic() => {
				let mut ident = c.to_string();
				while let Some(c) = chars.next_if(|c| c.is_ascii_alphanumeric()) {
					ident.push(c);
				}
				Token::Ident(Ident(ident))
			}
			c if c.is_ascii_digit() => {
				let mut num = c.to_string();
				while let Some(c) = chars.next_if(|c| c.is_ascii_digit()) {
					num.push(c);
				}
				Token::Number(num.parse().unwrap())
			}
			c if c.is_ascii_whitespace() => {
				continue;
			}
			c => todo!("{c}"),
		};

		tokens.push(token);
	}

	tokens
}
