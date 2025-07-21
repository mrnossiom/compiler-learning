//! Higher IR

use crate::{
	ast::{self, Spanned},
	lexer::{BinOp, LiteralKind, Span},
	session::Symbol,
};

#[derive(Debug)]
pub struct Root<'lcx> {
	pub items: &'lcx [Item<'lcx>],
}

#[derive(Debug)]
pub struct Item<'lcx> {
	pub kind: ItemKind<'lcx>,
	pub span: Span,
}

#[derive(Debug)]
pub enum ItemKind<'lcx> {
	Extern {
		ident: ast::Ident,
		decl: &'lcx FnDecl<'lcx>,
	},
	Function {
		ident: ast::Ident,
		decl: &'lcx FnDecl<'lcx>,
		body: &'lcx Block<'lcx>,
	},
}

#[derive(Debug, Clone)]
pub struct FnDecl<'lcx> {
	pub inputs: &'lcx [(ast::Ident, ast::Ty)],
	pub output: &'lcx ast::Ty,

	pub span: Span,
}

#[derive(Debug)]
pub struct Block<'lcx> {
	pub stmts: &'lcx [Stmt<'lcx>],
	pub ret_expr: Option<&'lcx Expr<'lcx>>,
}

#[derive(Debug)]
pub struct Stmt<'lcx> {
	pub kind: StmtKind<'lcx>,
	pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind<'lcx> {
	Expr(&'lcx Expr<'lcx>),

	Let {
		ident: ast::Ident,
		ty: &'lcx ast::Ty,
		value: &'lcx Expr<'lcx>,
	},

	// move these to expr
	Assign {
		target: ast::Ident,
		value: &'lcx Expr<'lcx>,
	},

	Loop {
		block: &'lcx Block<'lcx>,
	},
}

#[derive(Debug)]
pub struct Expr<'lcx> {
	pub kind: ExprKind<'lcx>,
	pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'lcx> {
	Variable(ast::Ident),
	Literal(LiteralKind, Symbol),

	Binary(Spanned<BinOp>, &'lcx Expr<'lcx>, &'lcx Expr<'lcx>),

	FnCall {
		expr: &'lcx Expr<'lcx>,
		args: &'lcx [Expr<'lcx>],
	},

	If {
		cond: &'lcx Expr<'lcx>,
		conseq: &'lcx Block<'lcx>,
		altern: Option<&'lcx Block<'lcx>>,
	},

	Break(Option<&'lcx Expr<'lcx>>),
	// TODO: add scope label
	Continue,
}
