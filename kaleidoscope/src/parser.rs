//! Tokens to AST parsing logic

use std::{borrow::Cow, mem};

#[allow(clippy::enum_glob_use)]
use crate::lexer::{BinOp::*, Delimiter::*, Keyword::*, LiteralKind::*, TokenKind::*};
use crate::{
	ast::{
		Block, Expr, ExprKind, FnDecl, Ident, Item, ItemKind, Root, Spanned, Stmt, StmtKind, Ty,
		TyKind,
	},
	lexer::{Delimiter, Lexer, Token, TokenKind},
	session::SessionCtx,
};

pub type PResult<T> = Result<T, Cow<'static, str>>;

pub struct Parser<'fcx> {
	fcx: &'fcx SessionCtx,

	lexer: Lexer<'fcx, 'fcx>,

	token: Token,
	last_token: Token,
}

impl<'fcx> Parser<'fcx> {
	pub fn new(fcx: &'fcx SessionCtx, content: &'fcx str) -> Self {
		let mut parser = Self {
			fcx,

			lexer: Lexer::new(fcx, content),

			token: Token::DUMMY,
			last_token: Token::DUMMY,
		};

		// init the first token
		parser.bump();

		parser
	}
}

/// Helper methods
impl Parser<'_> {
	fn bump(&mut self) -> Token {
		self.last_token = mem::replace(&mut self.token, self.lexer.next().unwrap_or(Token::DUMMY));
		self.token
	}

	fn check(&self, token: TokenKind) -> bool {
		self.token.kind == token
	}

	fn eat(&mut self, token: TokenKind) -> bool {
		if self.check(token) {
			self.bump();
			true
		} else {
			false
		}
	}

	#[track_caller]
	fn expect(&mut self, token: TokenKind) -> PResult<Token> {
		if self.check(token) {
			self.bump();
			Ok(self.token)
		} else {
			Err(format!("expected kind {token:?}, got {:?}", self.token).into())
		}
	}

	fn eat_ident(&mut self) -> Option<Ident> {
		self.token.as_ident().inspect(|_| {
			self.bump();
		})
	}

	fn expect_ident(&mut self) -> PResult<Ident> {
		self.eat_ident()
			.ok_or_else(|| format!("expected an ident, got {:?}", self.token).into())
	}

	fn parse_seq<T>(
		&mut self,
		delim: Delimiter,
		separator: TokenKind,
		mut p_elem: impl FnMut(&mut Self) -> PResult<T>,
	) -> PResult<Vec<T>> {
		let mut finished = false;

		let mut seq = Vec::new();

		self.expect(Open(delim))?;
		while !self.eat(Close(delim)) && !finished {
			seq.push(p_elem(self)?);

			// no comma means no item left
			finished = !self.eat(separator);
		}
		Ok(seq)
	}

	/// Looks one token ahead
	fn look_ahead(&self) -> TokenKind {
		self.lexer.clone().next().map_or(Eof, |tkn| tkn.kind)
	}
}

/// Expressions
impl Parser<'_> {
	fn parse_expr(&mut self) -> PResult<Expr> {
		let lhs = self.parse_expr_()?;
		self.parse_binop_rhs(0, lhs)
	}

	fn parse_binop_rhs(&mut self, precedence: u32, mut lhs: Expr) -> PResult<Expr> {
		let lo = self.token.span;

		while let BinOp(binop) = self.token.kind
			&& binop.precedence() >= precedence
		{
			let binop_span = self.token.span;
			self.bump();
			let rhs = self.parse_expr()?;
			lhs = Expr {
				kind: ExprKind::Binary {
					op: Spanned::new(binop, binop_span),
					left: Box::new(lhs),
					right: Box::new(rhs),
				},
				span: lo.to(self.last_token.span),
			};
		}
		Ok(lhs)
	}

	fn parse_expr_(&mut self) -> PResult<Expr> {
		let mut expr = match self.token.kind {
			// prefix
			Not => self.parse_not_expr()?,

			Open(Paren) => self.parse_paren_expr()?,
			TokenKind::Ident(_) => self.parse_identifier_expr()?,
			Literal(Integer, symbol) => {
				self.bump();
				Expr {
					kind: ExprKind::Literal(Integer, symbol),
					span: self.last_token.span,
				}
			}
			Keyword(If) => self.parse_if_expr()?,
			// Some(Keyword(While)) => self.parse_while_stmt()?,
			ukn => todo!("{ukn:?}"),
		};

		// check for postfix things like `f()`
		expr = match self.token.kind {
			Open(Paren) => self.parse_fn_call(expr)?,
			_ => expr,
		};

		Ok(expr)
	}

	fn parse_not_expr(&mut self) -> PResult<Expr> {
		self.expect(Not)?;
		let expr = self.parse_expr()?;
		Ok(expr)
	}

	fn parse_paren_expr(&mut self) -> PResult<Expr> {
		self.expect(Open(Paren))?;
		let expr = self.parse_expr()?;
		self.expect(Close(Paren))?;
		Ok(expr)
	}

	fn parse_identifier_expr(&mut self) -> PResult<Expr> {
		let ident = self.expect_ident()?;
		Ok(Expr {
			kind: ExprKind::Variable(ident),
			span: ident.span,
		})
	}

	fn parse_if_expr(&mut self) -> PResult<Expr> {
		let lo = self.token.span;

		self.expect(Keyword(If))?;
		let cond = self.parse_expr()?;
		let conseq = self.parse_block()?;
		let altern = if self.eat(Keyword(Else)) {
			Some(self.parse_block()?)
		} else {
			None
		};

		let expr_kind = ExprKind::If {
			cond: Box::new(cond),
			conseq: Box::new(conseq),
			altern: altern.map(Box::new),
		};
		Ok(Expr {
			kind: expr_kind,
			span: lo.to(self.last_token.span),
		})
	}
}

/// Items
impl Parser<'_> {
	pub fn parse_file(&mut self) -> PResult<Root> {
		let mut items = Vec::new();
		while self.token.kind != Eof {
			items.push(self.parse_item()?);
		}
		Ok(Root { items })
	}

	fn parse_item(&mut self) -> PResult<Item> {
		let lo = self.token.span;
		let kind = match self.token.kind {
			Keyword(Fn) => self.parse_fn()?,
			Keyword(Extern) => self.parse_extern_fn()?,
			Eof => return Err("no expression entered".into()),
			_ => return Err("could not parse item".into()),
		};
		Ok(Item {
			kind,
			span: lo.to(self.last_token.span),
		})
	}

	fn parse_fn(&mut self) -> PResult<ItemKind> {
		self.expect(Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		let body = Box::new(self.parse_block()?);
		Ok(ItemKind::Function { ident, decl, body })
	}

	fn parse_extern_fn(&mut self) -> PResult<ItemKind> {
		self.expect(Keyword(Extern))?;
		self.expect(Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		self.expect(Semi)?;
		Ok(ItemKind::Extern { ident, decl })
	}

	fn parse_fn_decl(&mut self) -> PResult<(Ident, FnDecl)> {
		let name = self.expect_ident()?;
		let decl_lo = self.token.span;
		let args = self.parse_seq(Paren, Comma, Parser::parse_argument)?;
		let ret_ty = if !self.check(Open(Brace)) && !self.check(Semi) {
			Some(self.parse_ty()?)
		} else {
			None
		};
		Ok((
			name,
			FnDecl {
				args,
				ret: ret_ty,
				span: decl_lo.to(self.last_token.span),
			},
		))
	}

	fn parse_argument(&mut self) -> PResult<(Ident, Ty)> {
		let name = self.expect_ident()?;
		self.expect(Colon)?;
		let ty = self.parse_ty()?;
		Ok((name, ty))
	}

	fn parse_fn_call(&mut self, expr: Expr) -> PResult<Expr> {
		let lo = expr.span;
		let args = self.parse_seq(Paren, Comma, Parser::parse_expr)?;
		let expr_kind = ExprKind::FnCall {
			expr: Box::new(expr),
			args,
		};
		Ok(Expr {
			kind: expr_kind,
			span: lo.to(self.last_token.span),
		})
	}
}

/// Types
impl Parser<'_> {
	fn parse_ty(&mut self) -> PResult<Ty> {
		match self.token.kind {
			Ident(_) => self.parse_ty_path(),
			_ => todo!("unknown ty type"),
		}
	}

	fn parse_ty_path(&mut self) -> PResult<Ty> {
		let lo = self.token.span;

		let mut path = Vec::new();
		path.push(self.expect_ident()?);

		while self.token.kind == Dot {
			self.bump();
			path.push(self.expect_ident()?);
		}

		let generics = if self.token.kind == BinOp(Lt) {
			Some(self.parse_ty_generics()?)
		} else {
			None
		};

		Ok(Ty {
			kind: TyKind::Path(path, generics),
			span: lo.to(self.last_token.span),
		})
	}

	fn parse_ty_generics(&mut self) -> PResult<Vec<Ty>> {
		let mut finished = false;

		let mut seq = Vec::new();

		self.expect(BinOp(Lt))?;
		while !self.eat(BinOp(Gt)) && !finished {
			seq.push(self.parse_ty()?);

			// no comma means no item left
			finished = !self.eat(Comma);
		}
		Ok(seq)
	}
}

/// Statements
impl Parser<'_> {
	fn parse_stmt(&mut self) -> PResult<Stmt> {
		let lo = self.token.span;
		let kind = match self.token.kind {
			Keyword(Loop) => self.parse_loop_stmt()?,
			Keyword(While) => self.parse_while_stmt()?,
			Keyword(For) => self.parse_for_stmt()?,
			Semi => StmtKind::Empty,

			TokenKind::Ident(_) if self.look_ahead() == Colon => self.parse_let_stmt()?,
			TokenKind::Ident(_) if self.look_ahead() == Eq => self.parse_assign_stmt()?,

			Eof => return Err("expected more input".into()),
			_ => {
				let expr = Box::new(self.parse_expr()?);
				if self.eat(Semi) {
					StmtKind::Expr(expr)
				} else {
					StmtKind::ExprRet(expr)
				}
			}
		};
		Ok(Stmt {
			kind,
			span: lo.to(self.last_token.span),
		})
	}

	fn parse_loop_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(Keyword(Loop))?;
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::Loop { body })
	}

	fn parse_while_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(Keyword(While))?;
		let check = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::WhileLoop { check, body })
	}

	fn parse_for_stmt(&mut self) -> PResult<StmtKind> {
		self.expect(Keyword(For))?;
		let pat = self.expect_ident()?;
		self.expect(Keyword(In))?;
		let iter = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::ForLoop { pat, iter, body })
	}

	fn parse_let_stmt(&mut self) -> PResult<StmtKind> {
		let ident = self.expect_ident()?;
		self.expect(Colon)?;

		// definition with optional ty
		// if the equal sign is right after, there is no type
		let ty = if self.check(Eq) {
			None
		} else {
			Some(Box::new(self.parse_ty()?))
		};
		self.expect(Eq)?;

		let value = Box::new(self.parse_expr()?);
		self.expect(Semi)?;

		Ok(StmtKind::Let { ident, ty, value })
	}

	fn parse_assign_stmt(&mut self) -> PResult<StmtKind> {
		let ident = self.expect_ident()?;
		self.expect(Eq)?;

		let value = Box::new(self.parse_expr()?);
		self.expect(Semi)?;
		Ok(StmtKind::Assign {
			target: ident,
			value,
		})
	}

	fn parse_block(&mut self) -> PResult<Block> {
		let lo = self.token.span;
		self.expect(Open(Brace))?;
		let mut stmts = Vec::new();
		while !self.eat(Close(Brace)) {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Block {
			stmts,
			span: lo.to(self.last_token.span),
		})
	}
}
