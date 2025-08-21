//! Tokens to AST parsing logic

use std::mem;

use ariadne::{Label, Report, ReportKind};

#[allow(clippy::enum_glob_use)]
use crate::lexer::{BinOp::*, Delimiter::*, Keyword::*, LiteralKind::*, TokenKind::*};
use crate::{
	ast::{
		Block, Expr, ExprKind, FnDecl, Ident, Item, ItemKind, NodeId, Root, Spanned, Stmt,
		StmtKind, Ty, TyKind,
	},
	bug, errors,
	lexer::{Delimiter, Lexer, Token, TokenKind},
	session::{Diagnostic, SessionCtx, SourceFile},
};

pub type PResult<T> = Result<T, Diagnostic>;

pub struct Parser<'scx> {
	scx: &'scx SessionCtx,

	lexer: Lexer<'scx, 'scx>,

	token: Token,
	last_token: Token,

	next_node_id: u32,
}

impl<'scx> Parser<'scx> {
	pub fn new(scx: &'scx SessionCtx, file: &'scx SourceFile) -> Self {
		let mut parser = Self {
			scx,

			lexer: Lexer::new(scx, &file.content, file.offset),

			token: Token::DUMMY,
			last_token: Token::DUMMY,

			next_node_id: 0,
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
	fn expect(&mut self, expected_kind: TokenKind) -> PResult<Token> {
		if self.check(expected_kind) {
			self.bump();
			Ok(self.token)
		} else {
			let report = errors::parser::expected_token_kind(expected_kind, self.token);
			Err(Diagnostic::new(report))
		}
	}

	fn eat_ident(&mut self) -> Option<Ident> {
		self.token.as_ident().inspect(|_| {
			self.bump();
		})
	}

	fn expect_ident(&mut self) -> PResult<Ident> {
		self.eat_ident().ok_or_else(|| {
			let placeholder = self.scx.symbols.intern("_");
			let report =
				errors::parser::expected_token_kind(TokenKind::Ident(placeholder), self.token);
			Diagnostic::new(report)
		})
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

	fn make_node_id(&mut self) -> NodeId {
		let node_id = NodeId(self.next_node_id);
		self.next_node_id = self
			.next_node_id
			.checked_add(1)
			.unwrap_or_else(|| bug!("tried to construct too much `parser::NodeId`s"));
		node_id
	}
}

/// Expressions
impl Parser<'_> {
	fn parse_expr(&mut self) -> PResult<Expr> {
		tracing::trace!(kind = ?self.token.kind, "parse_expr");
		let lhs = self.parse_expr_()?;
		self.parse_binop_rhs(0, lhs)
	}

	fn parse_binop_rhs(&mut self, precedence: u32, mut lhs: Expr) -> PResult<Expr> {
		tracing::trace!(kind = ?self.token.kind, "parse_binop_rhs");
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
				id: self.make_node_id(),
			};
		}
		Ok(lhs)
	}

	fn parse_expr_(&mut self) -> PResult<Expr> {
		tracing::trace!(kind = ?self.token.kind, "parse_expr_");

		let mut expr = match self.token.kind {
			// prefix
			Not => self.parse_not_expr()?,

			Open(Paren) => self.parse_paren_expr()?,
			TokenKind::Ident(_) => self.parse_identifier_expr()?,
			Literal(kind, symbol) => {
				self.bump();
				let kind = match kind {
					Integer => ExprKind::Literal(Integer, symbol),
					Float => ExprKind::Literal(Float, symbol),
					// handle prefixed strings (e.g. c"content")
					Str => ExprKind::Literal(Str, symbol),
				};
				Expr {
					kind,
					span: self.last_token.span,
					id: self.make_node_id(),
				}
			}
			Keyword(If) => self.parse_if_expr()?,
			// TODO: make loops be expressions
			// Keyword(While) => self.parse_while_stmt()?,
			Keyword(Return) => self.parse_return()?,
			Keyword(Break) => self.parse_break()?,
			Keyword(Continue) => self.parse_continue()?,

			_ => {
				let report =
					errors::parser::expected_construct_no_match("an expression", self.token.span);
				return Err(Diagnostic::new(report));
			}
		};

		// check for postfix things like `f()`
		expr = match self.token.kind {
			Open(Paren) => self.parse_fn_call(expr)?,
			_ => expr,
		};

		Ok(expr)
	}

	fn parse_not_expr(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_not_expr");
		self.expect(Not)?;
		let expr = self.parse_expr()?;
		Ok(expr)
	}

	fn parse_paren_expr(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_paren_expr");
		self.expect(Open(Paren))?;
		let expr = self.parse_expr()?;
		self.expect(Close(Paren))?;
		Ok(expr)
	}

	fn parse_identifier_expr(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_identifier_expr");
		let ident = self.expect_ident()?;
		Ok(Expr {
			kind: ExprKind::Variable(ident),
			span: ident.span,
			id: self.make_node_id(),
		})
	}

	fn parse_if_expr(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_if_expr");
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
			id: self.make_node_id(),
		})
	}

	fn parse_return(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_return");
		let lo = self.token.span;
		self.expect(Keyword(Return))?;

		// TODO: bad for recovery
		let expr = self.parse_expr().ok();

		Ok(Expr {
			kind: ExprKind::Return(expr.map(Box::new)),
			span: lo.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}

	fn parse_break(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_break");
		let lo = self.token.span;
		self.expect(Keyword(Break))?;
		let expr = self.parse_expr().ok();
		Ok(Expr {
			kind: ExprKind::Break(expr.map(Box::new)),
			span: lo.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}

	fn parse_continue(&mut self) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_continue");
		self.expect(Keyword(Continue))?;
		Ok(Expr {
			kind: ExprKind::Continue,
			span: self.last_token.span,
			id: self.make_node_id(),
		})
	}
}

/// Items
impl Parser<'_> {
	pub fn parse_root(&mut self) -> PResult<Root> {
		tracing::trace!(cur = ?self.token.kind, "parse_file");
		let mut items = Vec::new();
		while self.token.kind != Eof {
			items.push(self.parse_item()?);
		}
		Ok(Root { items })
	}

	fn parse_item(&mut self) -> PResult<Item> {
		tracing::trace!(cur = ?self.token.kind, "parse_item");
		let lo = self.token.span;
		let kind = match self.token.kind {
			Keyword(Fn) => self.parse_fn()?,
			Keyword(Extern) => self.parse_extern_fn()?,

			_ => {
				let report =
					errors::parser::expected_construct_no_match("an item", self.token.span);
				return Err(Diagnostic::new(report));
			}
		};
		Ok(Item {
			kind,
			span: lo.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}

	fn parse_fn(&mut self) -> PResult<ItemKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_fn");
		self.expect(Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		let body = Box::new(self.parse_block()?);
		Ok(ItemKind::Function { ident, decl, body })
	}

	fn parse_extern_fn(&mut self) -> PResult<ItemKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_extern_fn");
		self.expect(Keyword(Extern))?;
		self.expect(Keyword(Fn))?;
		let (ident, decl) = self.parse_fn_decl()?;
		self.expect(Semi)?;
		Ok(ItemKind::Extern { ident, decl })
	}

	fn parse_fn_decl(&mut self) -> PResult<(Ident, FnDecl)> {
		tracing::trace!(cur = ?self.token.kind, "parse_fn_decl");
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
		tracing::trace!(cur = ?self.token.kind, "parse_argument");
		let name = self.expect_ident()?;
		self.expect(Colon)?;
		let ty = self.parse_ty()?;
		Ok((name, ty))
	}

	fn parse_fn_call(&mut self, expr: Expr) -> PResult<Expr> {
		tracing::trace!(cur = ?self.token.kind, "parse_fn_call");
		let lo = self.token.span;
		let expr_span = expr.span;
		let args = self.parse_seq(Paren, Comma, Parser::parse_expr)?;
		let expr_kind = ExprKind::FnCall {
			expr: Box::new(expr),
			args: Spanned::new(args, lo.to(self.last_token.span)),
		};
		Ok(Expr {
			kind: expr_kind,
			span: expr_span.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}
}

/// Types
impl Parser<'_> {
	fn parse_ty(&mut self) -> PResult<Ty> {
		tracing::trace!(cur = ?self.token.kind, "parse_ty");
		match self.token.kind {
			Ident(_) => self.parse_ty_path(),
			BinOp(Mul) => self.parse_ty_pointer(),
			_ => {
				let report = errors::parser::expected_construct_no_match("a type", self.token.span);
				Err(Diagnostic::new(report))
			}
		}
	}

	fn parse_ty_path(&mut self) -> PResult<Ty> {
		tracing::trace!(cur = ?self.token.kind, "parse_ty_path");
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

	fn parse_ty_pointer(&mut self) -> PResult<Ty> {
		let lo = self.token.span;
		// TODO: expect star for diagnostics
		self.expect(BinOp(Mul))?;

		let ty = Box::new(self.parse_ty()?);

		Ok(Ty {
			kind: TyKind::Pointer(ty),
			span: lo.to(self.last_token.span),
		})
	}

	fn parse_ty_generics(&mut self) -> PResult<Vec<Ty>> {
		tracing::trace!(cur = ?self.token.kind, "parse_ty_generics");
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
		tracing::trace!(cur = ?self.token.kind, "parse_stmt");
		let lo = self.token.span;
		let kind = match self.token.kind {
			Keyword(Loop) => self.parse_loop_stmt()?,
			Keyword(While) => self.parse_while_stmt()?,
			Keyword(For) => self.parse_for_stmt()?,
			Semi => {
				self.expect(Semi)?;
				StmtKind::Empty
			}

			TokenKind::Ident(_) if self.look_ahead() == Colon => self.parse_let_stmt()?,
			TokenKind::Ident(_) if self.look_ahead() == Eq => self.parse_assign_stmt()?,

			Eof => {
				let report = Report::build(ReportKind::Error, self.token.span)
					.with_message("expected more input")
					.with_label(Label::new(self.token.span).with_message("here"));
				return Err(Diagnostic::new(report));
			}
			_ => {
				let expr = Box::new(self.parse_expr()?);
				if self.eat(Semi) {
					StmtKind::Expr(expr)
				} else {
					// TODO: enforce parsing for expr ret
					StmtKind::ExprRet(expr)
				}
			}
		};
		Ok(Stmt {
			kind,
			span: lo.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}

	fn parse_loop_stmt(&mut self) -> PResult<StmtKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_loop_stmt");
		self.expect(Keyword(Loop))?;
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::Loop { body })
	}

	fn parse_while_stmt(&mut self) -> PResult<StmtKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_while_stmt");
		self.expect(Keyword(While))?;
		let check = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::WhileLoop { check, body })
	}

	fn parse_for_stmt(&mut self) -> PResult<StmtKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_for_stmt");
		self.expect(Keyword(For))?;
		let pat = self.expect_ident()?;
		self.expect(Keyword(In))?;
		let iter = Box::new(self.parse_expr()?);
		let body = Box::new(self.parse_block()?);
		Ok(StmtKind::ForLoop { pat, iter, body })
	}

	fn parse_let_stmt(&mut self) -> PResult<StmtKind> {
		tracing::trace!(cur = ?self.token.kind, "parse_let_stmt");
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
		tracing::trace!(cur = ?self.token.kind, "parse_assign_stmt");
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
		tracing::trace!(cur = ?self.token.kind, "parse_block");
		let lo = self.token.span;
		self.expect(Open(Brace))?;
		let mut stmts = Vec::new();
		while !self.eat(Close(Brace)) {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Block {
			stmts,
			span: lo.to(self.last_token.span),
			id: self.make_node_id(),
		})
	}
}
