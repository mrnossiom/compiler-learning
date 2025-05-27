use std::iter::Peekable;

use crate::{Ident, lexer::Token};

pub fn parse(tokens: Vec<Token>) {
	let tokens = tokens.into_iter().peekable();
}

/// NumLit ::= number
fn parse_num(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
	let Some(Token::Number(value)) = lexer.next() else {
		todo!()
	};

	Expr::NumLit(NumberExpr { value })
}

struct NumberExpr {
	value: u64,
}

struct VariableExpr {
	name: Ident,
}

enum BinOp {
	Addition,
	Soustraction,
}

enum Expr {
	NumLit(NumberExpr),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	FnCall(Ident, Vec<Expr>),
}

struct Prototype {
	name: Ident,
	args: Vec<Ident>,
}

struct Function {
	proto: Prototype,
	body: Expr,
}
