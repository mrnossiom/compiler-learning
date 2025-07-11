//! Tokens to AST parsing logic

use std::borrow::Cow;

#[allow(clippy::enum_glob_use)]
use crate::lexer::{Delimiter::*, Keyword::*, Literal::*, TokenKind::*};
use crate::{
	ast::{Block, ExprKind, FnDecl, ItemKind, StmtKind, TyKind},
	lexer::{Delimiter, Ident, Lexer, TokenKind},
};

pub type PResult<T> = Result<T, Cow<'static, str>>;

pub struct Parser<'a> {
	lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
	pub fn new(content: &'a str) -> Self {
		let lexer = Lexer::new(content);
		Self { lexer }
	}
}

/// Helper methods
impl Parser<'_> {
	fn eat(&mut self, token: &TokenKind) -> Option<TokenKind> {
		if &self.lexer.peek() == token {
			Some(self.lexer.next())
		} else {
			None
		}
	}

	fn expect(&mut self, token: &TokenKind) -> PResult<TokenKind> {
		self.eat(token)
			.ok_or_else(|| format!("expected {token:?}, got {:?}", self.lexer.peek()).into())
	}

	fn check(&mut self, token: &TokenKind) -> bool {
		&self.lexer.peek() == token
	}

	fn eat_ident(&mut self) -> Option<Ident> {
		match self.lexer.next() {
			TokenKind::Ident(ident) => Some(ident),
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

		self.expect(&Open(delim))?;
		while self.eat(&Close(delim)).is_none() && !finished {
			seq.push(p_elem(self)?);

			// no comma means no item left
			finished = self.eat(separator).is_none();
		}
		Ok(seq)
	}
}

/// Expressions
impl Parser<'_> {
	fn parse_expr(&mut self) -> PResult<ExprKind> {
		let lhs = self.parse_expr_()?;
		self.parse_binop_rhs(0, lhs)
	}

	fn parse_binop_rhs(&mut self, precedence: u32, mut lhs: ExprKind) -> PResult<ExprKind> {
		while let BinOp(binop) = self.lexer.peek()
			&& binop.precedence() >= precedence
		{
			self.lexer.bump();
			let rhs = self.parse_expr()?;
			lhs = ExprKind::Binary {
				op: binop,
				left: Box::new(lhs),
				right: Box::new(rhs),
			};
		}
		Ok(lhs)
	}

	fn parse_expr_(&mut self) -> PResult<ExprKind> {
		let mut expr = match self.lexer.peek() {
			// prefix
			Not => self.parse_not_expr()?,

			Open(Paren) => self.parse_paren_expr()?,
			TokenKind::Ident(_) => self.parse_identifier_expr()?,
			Literal(Integer(_)) => self.parse_num_literal_expr()?,
			Keyword(If) => self.parse_if_expr()?,
			// Some(Keyword(While)) => self.parse_while_stmt()?,
			ukn => todo!("{ukn:?}"),
		};

		// check for postfix things like `f()`
		expr = match self.lexer.peek() {
			Open(Paren) => self.parse_fn_call(expr)?,
			_ => expr,
		};

		Ok(expr)
	}

	fn parse_not_expr(&mut self) -> PResult<ExprKind> {
		self.expect(&Not)?;
		let expr = self.parse_expr()?;
		Ok(expr)
	}

	fn parse_paren_expr(&mut self) -> PResult<ExprKind> {
		self.expect(&Open(Paren))?;
		let expr = self.parse_expr()?;
		self.expect(&Close(Paren))?;
		Ok(expr)
	}

	fn parse_identifier_expr(&mut self) -> PResult<ExprKind> {
		let ident = self.expect_ident()?;
		Ok(ExprKind::Variable(ident))
	}

	fn parse_num_literal_expr(&mut self) -> PResult<ExprKind> {
		match self.lexer.next() {
			Literal(value) => Ok(ExprKind::Literal(value)),
			_ => Err("expected a literal".into()),
		}
	}

	fn parse_if_expr(&mut self) -> PResult<ExprKind> {
		self.expect(&Keyword(If))?;
		let cond = self.parse_expr()?;
		let conseq = self.parse_block()?;
		let altern = if self.eat(&Keyword(Else)).is_some() {
			Some(self.parse_block()?)
		} else {
			None
		};

		Ok(ExprKind::If {
			cond: Box::new(cond),
			conseq: Box::new(conseq),
			altern: altern.map(Box::new),
		})
	}
}

/// Items
impl Parser<'_> {
	pub fn parse_file(&mut self) -> PResult<Vec<ItemKind>> {
		let mut items = Vec::new();
		while self.lexer.peek() != Eof {
			items.push(self.parse_item()?);
		}
		Ok(items)
	}

	fn parse_item(&mut self) -> PResult<ItemKind> {
		let item = match self.lexer.peek() {
			Keyword(Fn) => self.parse_fn()?,
			Keyword(Extern) => self.parse_extern_fn()?,
			Eof => return Err("no expression entered".into()),
			_ => return Err("could not parse item".into()),
		};
		Ok(item)
	}

	fn parse_fn(&mut self) -> PResult<ItemKind> {
		self.expect(&Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		let body = Box::new(self.parse_block()?);
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

	fn parse_fn_call(&mut self, expr: ExprKind) -> PResult<ExprKind> {
		let args = self.parse_seq(Paren, &Comma, Parser::parse_expr)?;
		Ok(ExprKind::FnCall {
			expr: Box::new(expr),
			args,
		})
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
	fn parse_stmt(&mut self) -> PResult<StmtKind> {
		let stmt = match self.lexer.peek() {
			Keyword(Loop) => self.parse_loop_stmt()?,
			Keyword(While) => self.parse_while_stmt()?,
			Keyword(For) => self.parse_for_stmt()?,
			Semi => StmtKind::Empty,

			TokenKind::Ident(_) if self.lexer.look_ahead(1) == Colon => self.parse_let_stmt()?,
			TokenKind::Ident(_) if self.lexer.look_ahead(1) == Eq => self.parse_assign_stmt()?,

			Eof => return Err("expected more input".into()),
			_ => {
				let expr = Box::new(self.parse_expr()?);
				if self.eat(&Semi).is_some() {
					StmtKind::Expr(expr)
				} else {
					StmtKind::ExprRet(expr)
				}
			}
		};
		Ok(stmt)
	}

	fn parse_loop_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(&Keyword(Loop))?;
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::Loop { body })
	}

	fn parse_while_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(&Keyword(While))?;
		let check = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::WhileLoop { check, body })
	}

	fn parse_for_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(&Keyword(For))?;
		let pat = self.expect_ident()?;
		self.expect(&Keyword(In))?;
		let iter = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::ForLoop { pat, iter, body })
	}

	fn parse_let_stmt(&mut self) -> PResult<StmtKind> {
		let ident = self.expect_ident()?;
		self.expect(&Colon)?;

		// definition with optional ty
		let ty = if self.check(&Eq) {
			TyKind::Infer
		} else {
			self.parse_ty()?
		};
		self.expect(&Eq)?;

		let value = Box::new(self.parse_expr()?);
		self.expect(&Semi)?;

		Ok(StmtKind::Let {
			name: ident,
			ty,
			value,
		})
	}

	fn parse_assign_stmt(&mut self) -> PResult<StmtKind> {
		let ident = self.expect_ident()?;
		self.expect(&Eq)?;

		let value = Box::new(self.parse_expr()?);
		self.expect(&Semi)?;
		Ok(StmtKind::Assign {
			target: ident,
			value,
		})
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
