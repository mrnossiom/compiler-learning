use std::{borrow::Cow, iter::Peekable};

#[allow(clippy::enum_glob_use)]
use crate::lexer::{Delimiter::*, Keyword::*, Literal::*, Operator::*, TokenKind::*};
use crate::{
	Ident,
	lexer::{Delimiter, Lexer, Literal, Operator, TokenKind},
};

type PResult<T> = Result<T, Cow<'static, str>>;

pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	pub fn new(lexer: Lexer<'a>) -> Self {
		let lexer = lexer.peekable();
		Self { lexer }
	}
}

impl Parser<'_> {
	fn eat(&mut self, token: &TokenKind) -> Option<TokenKind> {
		self.lexer.next_if_eq(token)
	}

	fn expect(&mut self, token: &TokenKind) -> PResult<TokenKind> {
		self.eat(token)
			.ok_or_else(|| format!("expected {token:?}, got {:?}", self.lexer.peek()).into())
	}

	fn check(&mut self, token: &TokenKind) -> bool {
		self.lexer.peek() == Some(token)
	}

	fn eat_ident(&mut self) -> Option<Ident> {
		match self.lexer.next() {
			Some(TokenKind::Ident(ident)) => Some(ident),
			_ => None,
		}
	}

	fn expect_ident(&mut self) -> PResult<Ident> {
		self.eat_ident()
			.ok_or_else(|| format!("expected an ident, got {:?}", self.lexer.peek()).into())
	}

	fn parse_seq<T>(
		&mut self,
		delim: Delimiter,
		separator: &TokenKind,
		mut p_elem: impl FnMut(&mut Self) -> PResult<T>,
	) -> PResult<Vec<T>> {
		let mut finished = false;

		let mut seq = Vec::new();

		self.expect(&TokenKind::Open(delim))?;
		while self.eat(&TokenKind::Close(delim)).is_none() && !finished {
			seq.push(p_elem(self)?);

			// no comma means no item left
			finished = self.eat(separator).is_none();
		}
		Ok(seq)
	}
}

// Expressions
impl Parser<'_> {
	fn parse_num_literal_expr(&mut self) -> PResult<ExprKind> {
		match self.lexer.next() {
			Some(Literal(value)) => Ok(ExprKind::Literal(value)),
			_ => Err("expected a literal".into()),
		}
	}

	fn parse_paren_expr(&mut self) -> PResult<ExprKind> {
		self.expect(&Open(Paren))?;
		let expr = self.parse_expr()?;
		self.expect(&Close(Paren))?;
		Ok(expr)
	}

	fn parse_identifier_expr(&mut self) -> PResult<ExprKind> {
		let ident = self.expect_ident()?;

		if self.check(&TokenKind::Open(Paren)) {
			let args = self.parse_seq(Paren, &Comma, Parser::parse_expr)?;
			Ok(ExprKind::FnCall { ident, args })
		} else {
			Ok(ExprKind::Ident(ident))
		}
	}

	fn parse_if_expr(&mut self) -> PResult<ExprKind> {
		self.expect(&TokenKind::Keyword(If))?;
		let condition = Box::new(self.parse_expr()?);

		let consequence = Box::new(self.parse_block()?);
		self.expect(&TokenKind::Keyword(Else))?;
		let alternative = Box::new(self.parse_block()?);

		Ok(ExprKind::If {
			condition,
			consequence,
			alternative,
		})
	}

	fn parse_while_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(&Keyword(While))?;

		let check = Box::new(self.parse_expr()?);

		let body = Box::new(self.parse_block()?);

		Ok(StmtKind::WhileLoop { check, body })
	}

	fn parse_expr_(&mut self) -> PResult<ExprKind> {
		match self.lexer.peek() {
			Some(Open(Paren)) => self.parse_paren_expr(),
			Some(TokenKind::Ident(_)) => self.parse_identifier_expr(),
			Some(TokenKind::Literal(Integer(_))) => self.parse_num_literal_expr(),
			Some(TokenKind::Keyword(If)) => self.parse_if_expr(),
			// Some(TokenKind::Keyword(While)) => self.parse_while_stmt(),
			_ => todo!(),
		}
	}

	fn parse_expr(&mut self) -> PResult<ExprKind> {
		let lhs = self.parse_expr_()?;
		self.parse_binop_rhs(0, lhs)
	}

	fn parse_binop_rhs(&mut self, precedence: u32, mut lhs: ExprKind) -> PResult<ExprKind> {
		loop {
			let Some(TokenKind::Operator(binop)) = self.lexer.next_if(
				// TODO
				|token| matches!(token, TokenKind::Operator(binop) if binop.precedence().unwrap_or(0) >= precedence),
			) else {
				break;
			};

			let rhs = self.parse_expr()?;
			lhs = ExprKind::Binary {
				op: binop,
				left: Box::new(lhs),
				right: Box::new(rhs),
			}
		}
		Ok(lhs)
	}

	fn parse_argument(&mut self) -> PResult<(Ident, TyKind)> {
		let name = self.expect_ident()?;
		self.expect(&Colon)?;
		let ty = self.parse_ty()?;
		Ok((name, ty))
	}

	fn parse_ty(&mut self) -> PResult<TyKind> {
		Ok(TyKind::Path(self.expect_ident()?))
	}

	fn parse_prototype(&mut self) -> PResult<Prototype> {
		let name = self.expect_ident()?;
		let args = self.parse_seq(Paren, &Comma, Parser::parse_argument)?;
		let ty = if !self.check(&Open(Brace)) && !self.check(&Semi) {
			self.parse_ty()?
		} else {
			TyKind::Unit
		};
		Ok(Prototype {
			name,
			args,
			ret: ty,
		})
	}

	fn parse_fn(&mut self) -> PResult<Function> {
		self.expect(&Keyword(Fn))?;
		let proto = self.parse_prototype()?;
		let body = self.parse_block()?;
		Ok(Function { proto, body })
	}

	fn parse_extern_fn(&mut self) -> PResult<Prototype> {
		self.expect(&Keyword(Extern))?;
		self.expect(&Keyword(Fn))?;
		let prototype = self.parse_prototype()?;
		self.expect(&Semi)?;
		Ok(prototype)
	}

	fn parse_item(&mut self) -> PResult<ItemKind> {
		let item = match self.lexer.peek() {
			Some(TokenKind::Keyword(Fn)) => ItemKind::Definition(self.parse_fn()?),
			Some(TokenKind::Keyword(Extern)) => ItemKind::Extern(self.parse_extern_fn()?),
			Some(_) => return Err("could not parse item".into()),
			None => return Err("no expression entered".into()),
		};
		Ok(item)
	}

	pub fn parse_file(&mut self) -> PResult<Vec<ItemKind>> {
		let mut items = Vec::new();
		while self.lexer.peek().is_some() {
			items.push(self.parse_item()?);
		}
		Ok(items)
	}
}

impl Parser<'_> {
	fn try_parse_var_stmt(&mut self) -> PResult<Result<StmtKind, Ident>> {
		let ident = self.expect_ident()?;

		match self.lexer.peek() {
			// definition with optional ty
			Some(Colon) => {
				self.expect(&Colon)?;

				let ty = if self.check(&Eq) {
					TyKind::Infer
				} else {
					self.parse_ty()?
				};
				self.expect(&Eq)?;

				let value = Box::new(self.parse_expr()?);
				self.expect(&Semi)?;

				Ok(Ok(StmtKind::Var {
					name: ident,
					ty,
					value,
				}))
			}
			// reassignment
			Some(Eq) => {
				self.expect(&Eq)?;
				let value = Box::new(self.parse_expr()?);
				self.expect(&Semi)?;
				Ok(Ok(StmtKind::Assign {
					target: ident,
					value,
				}))
			}
			// this is not a var decl
			_ => Ok(Err(ident)),
		}
	}

	fn parse_stmt(&mut self) -> PResult<StmtKind> {
		match self.lexer.peek() {
			Some(Keyword(While)) => Ok(self.parse_while_stmt()?),
			// Some(Keyword(For)) => Ok(self.parse_for_stmt()?),
			Some(Semi) => Ok(StmtKind::Empty),
			Some(TokenKind::Ident(_)) => match self.try_parse_var_stmt()? {
				Ok(stmt) => Ok(stmt),
				Err(ident) => {
					// TODO: this logic should not be here, dup with arm below
					let expr = Box::new(self.parse_binop_rhs(0, ExprKind::Ident(ident))?);
					if self.eat(&Semi).is_some() {
						Ok(StmtKind::Expr(expr))
					} else {
						Ok(StmtKind::ExprRet(expr))
					}
				}
			},
			Some(_) => {
				let expr = Box::new(self.parse_expr()?);
				if self.eat(&Semi).is_some() {
					Ok(StmtKind::Expr(expr))
				} else {
					Ok(StmtKind::ExprRet(expr))
				}
			}
			None => Err("expected more input".into()),
		}
	}

	fn parse_block(&mut self) -> PResult<Block> {
		self.expect(&Open(Brace))?;
		let mut stmts = Vec::new();
		while self.eat(&Close(Brace)).is_none() {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Block { stmts })
	}
}

#[derive(Debug)]
pub struct NumberExpr {
	pub value: i64,
}

#[derive(Debug)]
pub enum ExprKind {
	Ident(Ident),
	Literal(Literal),

	Binary {
		op: Operator,
		left: Box<ExprKind>,
		right: Box<ExprKind>,
	},
	FnCall {
		ident: Ident,
		args: Vec<ExprKind>,
	},

	If {
		condition: Box<ExprKind>,
		consequence: Box<Block>,
		alternative: Box<Block>,
	},
}

#[derive(Debug)]
pub struct Block {
	stmts: Vec<StmtKind>,
}

#[derive(Debug)]
pub struct Prototype {
	pub name: Ident,
	pub args: Vec<(Ident, TyKind)>,
	pub ret: TyKind,
}

#[derive(Debug)]
pub struct Function {
	pub proto: Prototype,
	pub body: Block,
}

#[derive(Debug)]
pub enum TyKind {
	Path(Ident),

	Unit,

	Infer,
}

#[derive(Debug)]
pub enum ItemKind {
	Definition(Function),
	Extern(Prototype),
}

#[derive(Debug)]
pub enum StmtKind {
	WhileLoop {
		check: Box<ExprKind>,
		body: Box<Block>,
	},

	ForLoop {
		init: (Ident, Box<ExprKind>),
		check: Box<ExprKind>,
		increment: Option<Box<ExprKind>>,
		body: Box<Block>,
	},

	Var {
		name: Ident,
		ty: TyKind,
		value: Box<ExprKind>,
	},
	Assign {
		target: Ident,
		value: Box<ExprKind>,
	},

	Expr(Box<ExprKind>),

	/// Expression without a semi to return a value at the end of a block
	ExprRet(Box<ExprKind>),

	/// A single lonely `;`
	Empty,
}
