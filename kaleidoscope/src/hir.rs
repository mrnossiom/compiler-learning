//! Higher IR

use std::fmt;

use crate::{
	ast::{self, Spanned},
	lexer::{BinaryOp, LiteralKind, UnaryOp},
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
pub struct Root {
	pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Item {
	pub kind: ItemKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ItemKind {
	Extern {
		ident: ast::Ident,
		decl: Box<FnDecl>,
	},
	Function {
		ident: ast::Ident,
		decl: Box<FnDecl>,
		body: Box<Block>,
	},
}

#[derive(Debug, Clone)]
pub struct FnDecl {
	pub inputs: Vec<(ast::Ident, ast::Ty)>,
	pub output: Box<ast::Ty>,

	pub span: Span,
}

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Stmt>,
	pub ret: Option<Box<Expr>>,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub struct Stmt {
	pub kind: StmtKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum StmtKind {
	Expr(Box<Expr>),

	Let {
		ident: ast::Ident,
		// Hinted ty
		ty: Box<ast::Ty>,
		value: Box<Expr>,
	},

	// move these to expr
	Assign {
		target: ast::Ident,
		value: Box<Expr>,
	},

	Loop {
		block: Box<Block>,
	},
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ExprKind {
	Access(ast::Ident),
	Literal(LiteralKind, Symbol),

	Unary(Spanned<UnaryOp>, Box<Expr>),
	Binary(Spanned<BinaryOp>, Box<Expr>, Box<Expr>),

	FnCall {
		expr: Box<Expr>,
		args: Spanned<Vec<Expr>>,
	},

	If {
		cond: Box<Expr>,
		conseq: Box<Block>,
		altern: Option<Box<Block>>,
	},

	Return(Option<Box<Expr>>),
	Break(Option<Box<Expr>>),
	// TODO: add scope label
	Continue,
}
