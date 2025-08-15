//! Typed Body IR
//!
//! This is what is handed to codegen

use crate::{
	ast::{self, Spanned},
	hir,
	lexer::{BinOp, LiteralKind},
	session::{Span, Symbol},
	ty,
};

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Stmt>,
	pub ret: Option<Expr>,
}

#[derive(Debug)]
pub struct Stmt {
	pub kind: StmtKind,
	pub span: Span,
	pub id: hir::NodeId,
}

#[derive(Debug)]
pub enum StmtKind {
	Expr(Expr),
	Let { ident: ast::Ident, value: Expr },
	Assign { target: ast::Ident, value: Expr },
	Loop { block: Block },
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub ty: ty::TyKind,
	pub span: Span,
	pub id: hir::NodeId,
}

#[derive(Debug)]
pub enum ExprKind {
	Literal(LiteralKind, Symbol),
	Variable(ast::Ident),

	Binary(Spanned<BinOp>, Box<Expr>, Box<Expr>),

	FnCall {
		expr: Box<Expr>,
		args: Vec<Expr>,
	},

	If {
		cond: Box<Expr>,
		conseq: Box<Block>,
		altern: Option<Box<Block>>,
	},

	Break(Option<Box<Expr>>),
	// TODO: add scope label
	Continue,
}
