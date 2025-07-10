use std::{borrow::Cow, iter::Peekable};

#[allow(clippy::enum_glob_use)]
use crate::lexer::{Delimiter::*, Keyword::*, Literal::*, TokenKind::*};
use crate::{
	Ident,
	ast::{Block, ExprKind, FnDecl, ItemKind, StmtKind, TyKind},
	lexer::{Delimiter, Lexer, TokenKind},
};

pub type PResult<T> = Result<T, Cow<'static, str>>;

pub struct Parser<'a> {
	lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
	pub fn new(content: &'a str) -> Self {
		let lexer = Lexer::new(content).peekable();
		Self { lexer }
	}
}

/// Helper methods
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

/// Expressions
impl Parser<'_> {
	fn parse_expr(&mut self) -> PResult<Box<ExprKind>> {
		let lhs = self.parse_expr_()?;
		self.parse_binop_rhs(0, lhs)
	}

	fn parse_binop_rhs(
		&mut self,
		precedence: u32,
		mut lhs: Box<ExprKind>,
	) -> PResult<Box<ExprKind>> {
		loop {
			let Some(TokenKind::Operator(binop)) = self.lexer.next_if(
				// TODO
				|token| matches!(token, TokenKind::Operator(binop) if binop.precedence().unwrap_or(0) >= precedence),
			) else {
				break;
			};

			let rhs = self.parse_expr()?;
			lhs = Box::new(ExprKind::Binary {
				op: binop,
				left: lhs,
				right: rhs,
			})
		}
		Ok(lhs)
	}

	fn parse_expr_(&mut self) -> PResult<Box<ExprKind>> {
		let mut expr = match self.lexer.peek() {
			Some(Open(Paren)) => self.parse_paren_expr()?,
			Some(TokenKind::Ident(_)) => self.parse_identifier_expr()?,
			Some(TokenKind::Literal(Integer(_))) => self.parse_num_literal_expr()?,
			Some(TokenKind::Keyword(If)) => self.parse_if_expr()?,
			// Some(TokenKind::Keyword(While)) => self.parse_while_stmt()?,
			_ => todo!(),
		};

		// check for postfix things like `f()`
		expr = match self.lexer.peek() {
			Some(Open(Paren)) => self.parse_fn_call(expr)?,
			_ => expr,
		};

		Ok(expr)
	}

	fn parse_paren_expr(&mut self) -> PResult<Box<ExprKind>> {
		self.expect(&Open(Paren))?;
		let expr = self.parse_expr()?;
		self.expect(&Close(Paren))?;
		Ok(expr)
	}

	fn parse_identifier_expr(&mut self) -> PResult<Box<ExprKind>> {
		let ident = self.expect_ident()?;
		Ok(Box::new(ExprKind::Variable(ident)))
	}

	fn parse_num_literal_expr(&mut self) -> PResult<Box<ExprKind>> {
		match self.lexer.next() {
			Some(Literal(value)) => Ok(Box::new(ExprKind::Literal(value))),
			_ => Err("expected a literal".into()),
		}
	}

	fn parse_if_expr(&mut self) -> PResult<Box<ExprKind>> {
		self.expect(&TokenKind::Keyword(If))?;
		let condition = self.parse_expr()?;

		let consequence = self.parse_block()?;
		self.expect(&TokenKind::Keyword(Else))?;
		let alternative = self.parse_block()?;

		Ok(Box::new(ExprKind::If {
			condition,
			consequence,
			alternative,
		}))
	}
}

/// Items
impl Parser<'_> {
	pub fn parse_file(&mut self) -> PResult<Vec<ItemKind>> {
		let mut items = Vec::new();
		while self.lexer.peek().is_some() {
			items.push(self.parse_item()?);
		}
		Ok(items)
	}

	fn parse_item(&mut self) -> PResult<ItemKind> {
		let item = match self.lexer.peek() {
			Some(TokenKind::Keyword(Fn)) => self.parse_fn()?,
			Some(TokenKind::Keyword(Extern)) => self.parse_extern_fn()?,
			Some(_) => return Err("could not parse item".into()),
			None => return Err("no expression entered".into()),
		};
		Ok(item)
	}

	fn parse_fn(&mut self) -> PResult<ItemKind> {
		self.expect(&Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		let body = self.parse_block()?;
		Ok(ItemKind::Function { ident, decl, body })
	}

	fn parse_extern_fn(&mut self) -> PResult<ItemKind> {
		self.expect(&Keyword(Extern))?;
		self.expect(&Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		self.expect(&Semi)?;
		Ok(ItemKind::Extern { ident, decl })
	}

	fn parse_fn_decl(&mut self) -> PResult<(Ident, FnDecl)> {
		let name = self.expect_ident()?;
		let args = self.parse_seq(Paren, &Comma, Parser::parse_argument)?;
		let ty = if !self.check(&Open(Brace)) && !self.check(&Semi) {
			self.parse_ty()?
		} else {
			TyKind::Unit
		};
		Ok((name, FnDecl { args, ret: ty }))
	}

	fn parse_argument(&mut self) -> PResult<(Ident, TyKind)> {
		let name = self.expect_ident()?;
		self.expect(&Colon)?;
		let ty = self.parse_ty()?;
		Ok((name, ty))
	}

	fn parse_fn_call(&mut self, expr: Box<ExprKind>) -> PResult<Box<ExprKind>> {
		let args = self.parse_seq(Paren, &Comma, Parser::parse_expr)?;
		Ok(Box::new(ExprKind::FnCall { expr, args }))
	}
}

/// Types
impl Parser<'_> {
	fn parse_ty(&mut self) -> PResult<TyKind> {
		Ok(TyKind::Path(self.expect_ident()?))
	}
}

/// Statements
impl Parser<'_> {
	fn parse_stmt(&mut self) -> PResult<Box<StmtKind>> {
		let stmt = match self.lexer.peek() {
			Some(Keyword(Loop)) => self.parse_loop_stmt()?,
			Some(Keyword(While)) => self.parse_while_stmt()?,
			Some(Keyword(For)) => self.parse_for_stmt()?,
			Some(Semi) => Box::new(StmtKind::Empty),
			Some(TokenKind::Ident(_)) => match self.try_parse_var_stmt()? {
				Ok(stmt) => Box::new(stmt),
				Err(ident) => {
					// TODO: this logic should not be here, dup with arm below
					let expr = self.parse_binop_rhs(0, Box::new(ExprKind::Variable(ident)))?;
					if self.eat(&Semi).is_some() {
						Box::new(StmtKind::Expr(expr))
					} else {
						Box::new(StmtKind::ExprRet(expr))
					}
				}
			},
			Some(_) => {
				let expr = self.parse_expr()?;
				if self.eat(&Semi).is_some() {
					Box::new(StmtKind::Expr(expr))
				} else {
					Box::new(StmtKind::ExprRet(expr))
				}
			}
			None => return Err("expected more input".into()),
		};
		Ok(stmt)
	}

	fn parse_loop_stmt(&mut self) -> PResult<Box<StmtKind>> {
		self.expect(&Keyword(Loop))?;
		let body = self.parse_block()?;
		Ok(Box::new(StmtKind::Loop { body }))
	}

	fn parse_while_stmt(&mut self) -> PResult<Box<StmtKind>> {
		self.expect(&Keyword(While))?;
		let check = self.parse_expr()?;
		let body = self.parse_block()?;
		Ok(Box::new(StmtKind::WhileLoop { check, body }))
	}

	fn parse_for_stmt(&mut self) -> PResult<Box<StmtKind>> {
		self.expect(&Keyword(For))?;
		let pat = self.expect_ident()?;
		self.expect(&Keyword(In))?;
		let iter = self.parse_expr()?;
		let body = self.parse_block()?;
		Ok(Box::new(StmtKind::ForLoop { pat, iter, body }))
	}

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

				let value = self.parse_expr()?;
				self.expect(&Semi)?;

				Ok(Ok(StmtKind::Let {
					name: ident,
					ty,
					value,
				}))
			}
			// reassignment
			Some(Eq) => {
				self.expect(&Eq)?;
				let value = self.parse_expr()?;
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

	fn parse_block(&mut self) -> PResult<Box<Block>> {
		self.expect(&Open(Brace))?;
		let mut stmts = Vec::new();
		while self.eat(&Close(Brace)).is_none() {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Box::new(Block { stmts }))
	}
}
