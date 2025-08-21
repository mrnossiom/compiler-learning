//! Higher IR

use std::fmt;

use crate::{
	ast::{self, Spanned},
	lexer::{BinOp, LiteralKind},
	session::{Span, Symbol},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Debug for NodeId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		// hir node id -> hid
		write!(f, "hid#{}", self.0)
	}
}

#[derive(Debug)]
pub struct Root<'lcx> {
	pub items: &'lcx [Item<'lcx>],
}

#[derive(Debug)]
pub struct Item<'lcx> {
	pub kind: ItemKind<'lcx>,
	pub span: Span,
	pub id: NodeId,
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
	pub ret: Option<&'lcx Expr<'lcx>>,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub struct Stmt<'lcx> {
	pub kind: StmtKind<'lcx>,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum StmtKind<'lcx> {
	Expr(&'lcx Expr<'lcx>),

	Let {
		ident: ast::Ident,
		// Hinted ty
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
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ExprKind<'lcx> {
	Variable(ast::Ident),
	Literal(LiteralKind, Symbol),

	Binary(Spanned<BinOp>, &'lcx Expr<'lcx>, &'lcx Expr<'lcx>),

	FnCall {
		expr: &'lcx Expr<'lcx>,
		args: Spanned<&'lcx [Expr<'lcx>]>,
	},

	If {
		cond: &'lcx Expr<'lcx>,
		conseq: &'lcx Block<'lcx>,
		altern: Option<&'lcx Block<'lcx>>,
	},

	Return(Option<&'lcx Expr<'lcx>>),
	Break(Option<&'lcx Expr<'lcx>>),
	// TODO: add scope label
	Continue,
}
