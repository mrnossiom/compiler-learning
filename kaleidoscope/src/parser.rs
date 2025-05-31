use std::{collections::HashMap, iter::Peekable};

use crate::{Ident, lexer::Token};

pub struct Parser<'a, I: Iterator<Item = Token>> {
	tokens: Peekable<I>,
	binop_precedence: &'a mut HashMap<char, u32>,
}

impl<'a, I: Iterator<Item = Token>> Parser<'a, I> {
	pub fn new(tokens: I, binop_precedence: &'a mut HashMap<char, u32>) -> Self {
		Self {
			tokens: tokens.peekable(),
			binop_precedence,
		}
	}
}

impl<I: Iterator<Item = Token>> Parser<'_, I> {
	/// *num-literal-expr* ::= *number*
	fn parse_num_literal_expr(&mut self) -> Result<Expr, &'static str> {
		match self.tokens.next() {
			Some(Token::Number(value)) => Ok(Expr::Literal(NumberExpr { value })),
			_ => todo!(),
		}
	}

	/// *paren-expr* ::= `(` *expression* `)`
	fn parse_paren_expr(&mut self) -> Result<Expr, &'static str> {
		assert_eq!(self.tokens.next(), Some(Token::Delimiter('(')));
		let expr = self.parse_expr();
		assert_eq!(self.tokens.next(), Some(Token::Delimiter(')')));
		expr
	}

	/// *identifier-expr*
	///     ::= *identifier*
	///     ::= *identifier* `(` *expression* * `)`
	fn parse_identifier_expr(&mut self) -> Result<Expr, &'static str> {
		let ident = match self.tokens.next() {
			Some(Token::Ident(ident)) => Ok(ident),
			_ => todo!(),
		}?;

		if self
			.tokens
			.next_if(|c| c == &Token::Delimiter('('))
			.is_some()
		{
			let mut args = Vec::new();
			while self.tokens.peek() != Some(&Token::Delimiter(')')) {
				args.push(self.parse_expr()?);
			}
			Ok(Expr::FnCall { ident, args })
		} else {
			Ok(Expr::Ident(ident))
		}
	}

	/// *primary*
	///     ::= *identifier-expr*
	///     ::= *number-expr*
	///     ::= *paren-expr*
	fn parse_primary(&mut self) -> Result<Expr, &'static str> {
		match self.tokens.peek() {
			Some(Token::Delimiter('(')) => self.parse_paren_expr(),
			Some(Token::Ident(_)) => self.parse_identifier_expr(),
			Some(Token::Number(_)) => self.parse_num_literal_expr(),
			_ => todo!(),
		}
	}

	/// *expression* ::= *primary* *binop-rhs*
	fn parse_expr(&mut self) -> Result<Expr, &'static str> {
		let lhs = self.parse_primary()?;
		self.parse_binop_rhs(0, lhs)
	}

	/// *binop-rhs* ::= (*binop* *primary*)*
	fn parse_binop_rhs(&mut self, precedence: u32, mut lhs: Expr) -> Result<Expr, &'static str> {
		loop {
			let Some(Token::Operator(binop)) = self.tokens.next_if(|token| {
				matches!(token, Token::Operator(binop) if self
					.binop_precedence
					.get(binop)
					.is_some_and(|p| *p > precedence))
			}) else {
				break;
			};

			let rhs = self.parse_expr()?;
			lhs = Expr::Bin {
				op: binop,
				left: Box::new(lhs),
				right: Box::new(rhs),
			}
		}

		Ok(lhs)
	}

	/// *prototype* ::= *identifier* `(` *identifier** `)`
	fn parse_prototype(&mut self) -> Result<Prototype, &'static str> {
		let Some(Token::Ident(fn_name)) = self.tokens.next() else {
			return Err("proto has no name ident");
		};

		assert_eq!(self.tokens.next(), Some(Token::Delimiter('(')));

		let mut args = Vec::new();
		while let Some(Token::Ident(ident)) = self
			.tokens
			.next_if(|token| matches!(token, Token::Ident(_)))
		{
			args.push(ident);
		}

		assert_eq!(self.tokens.next(), Some(Token::Delimiter(')')));

		Ok(Prototype {
			name: fn_name,
			args,
		})
	}

	/// *definition* ::= `def` *prototype* *expression*
	fn parse_definition(&mut self) -> Result<Function, &'static str> {
		let Some(Token::Def) = self.tokens.next() else {
			todo!()
		};

		let proto = self.parse_prototype()?;
		let body = self.parse_expr()?;

		Ok(Function { proto, body })
	}

	/// *extern* ::= `extern` *prototype*
	fn parse_extern(&mut self) -> Result<Prototype, &'static str> {
		let Some(Token::Extern) = self.tokens.next() else {
			todo!()
		};
		self.parse_prototype()
	}

	/// *repl* ::= *expression* | *definition* | *extern*
	pub fn parse_repl(&mut self) -> Result<ReplItem, &'static str> {
		let item = match self.tokens.peek() {
			Some(Token::Def) => ReplItem::Definition(self.parse_definition()?),
			Some(Token::Extern) => ReplItem::Extern(self.parse_extern()?),
			Some(_) => ReplItem::Expr(self.parse_expr()?),
			None => return Err("no expression entered"),
		};

		Ok(item)
	}
}

#[derive(Debug)]
pub struct NumberExpr {
	pub value: u64,
}

#[derive(Debug)]
pub enum Expr {
	Literal(NumberExpr),
	Bin {
		op: char,
		left: Box<Expr>,
		right: Box<Expr>,
	},
	Ident(Ident),
	FnCall {
		ident: Ident,
		args: Vec<Expr>,
	},
}

#[derive(Debug)]
pub struct Prototype {
	pub name: Ident,
	pub args: Vec<Ident>,
}

#[derive(Debug)]
pub struct Function {
	pub proto: Prototype,
	pub body: Expr,
}

#[derive(Debug)]
pub enum ReplItem {
	Expr(Expr),
	Definition(Function),
	Extern(Prototype),
}
