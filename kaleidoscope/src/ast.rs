//! Abstract Syntax Tree

use std::fmt;

use crate::{
	lexer::{BinOp, LiteralKind},
	session::{Span, Symbol},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Debug for NodeId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		// ast node id -> aid
		write!(f, "aid#{}", self.0)
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Ident {
	pub name: Symbol,
	pub span: Span,
}

impl Ident {
	#[must_use]
	pub const fn new(name: Symbol, span: Span) -> Self {
		Self { name, span }
	}
}

impl fmt::Debug for Ident {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Ident({:?}, {:?})", self.name, self.span)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
	pub bit: T,
	pub span: Span,
}

impl<T> Spanned<T> {
	pub const fn new(bit: T, span: Span) -> Self {
		Self { bit, span }
	}
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ExprKind {
	// Expression atomics
	Variable(Ident),
	Literal(LiteralKind, Symbol),

	Binary {
		op: Spanned<BinOp>,
		left: Box<Expr>,
		right: Box<Expr>,
	},
	FnCall {
		expr: Box<Expr>,
		args: Vec<Expr>,
	},

	If {
		cond: Box<Expr>,
		conseq: Box<Block>,
		altern: Option<Box<Block>>,
	},

	Return(Option<Box<Expr>>),
	Break(Option<Box<Expr>>),
	Continue,
}

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Stmt>,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub struct FnDecl {
	pub args: Vec<(Ident, Ty)>,
	pub ret: Option<Ty>,

	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ty {
	pub kind: TyKind,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Path(Vec<Ident>, Option<Vec<Ty>>),

	Unit,

	/// Corresponds to the explicit `_` token
	Infer,
}

#[derive(Debug)]
pub struct Item {
	pub kind: ItemKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ItemKind {
	Function {
		ident: Ident,
		decl: FnDecl,
		body: Box<Block>,
	},
	Extern {
		ident: Ident,
		decl: FnDecl,
	},
}

#[derive(Debug)]
pub struct Stmt {
	pub kind: StmtKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum StmtKind {
	Loop {
		body: Box<Block>,
	},
	WhileLoop {
		check: Box<Expr>,
		body: Box<Block>,
	},
	ForLoop {
		pat: Ident,
		iter: Box<Expr>,
		body: Box<Block>,
	},

	Let {
		ident: Ident,
		ty: Option<Box<Ty>>,
		value: Box<Expr>,
	},
	Assign {
		target: Ident,
		value: Box<Expr>,
	},

	Expr(Box<Expr>),

	/// Expression without a semi to return a value at the end of a block
	ExprRet(Box<Expr>),

	/// A single lonely `;`
	Empty,
}

#[derive(Debug)]
pub struct Root {
	pub items: Vec<Item>,
}
