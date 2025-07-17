//! Abstract Syntax Tree

use std::fmt;

use crate::{
	front::Symbol,
	lexer::{BinOp, LiteralKind, Span},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
	pub name: Symbol,
	pub span: Span,
}

impl Ident {
	pub const fn new(name: Symbol, span: Span) -> Self {
		Self { name, span }
	}
}

impl fmt::Debug for Ident {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Ident({:?}, {:?})", self.name, self.span)
	}
}

#[derive(Debug)]
pub enum ExprKind {
	// Expression atomics
	Variable(Ident),
	Literal(LiteralKind, Symbol),

	Binary {
		op: BinOp,
		left: Box<ExprKind>,
		right: Box<ExprKind>,
	},
	FnCall {
		expr: Box<ExprKind>,
		args: Vec<ExprKind>,
	},

	If {
		cond: Box<ExprKind>,
		conseq: Box<Block>,
		altern: Option<Box<Block>>,
	},
}

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<StmtKind>,
	pub span: Span,
}

#[derive(Debug)]
pub struct FnDecl {
	pub args: Vec<(Ident, TyKind)>,
	pub ret: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Path(Vec<Ident>, Vec<TyKind>),

	Unit,

	Infer,
}

pub struct Item {
	kind: ItemKind,
	span: Span,
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
pub enum StmtKind {
	Loop {
		body: Box<Block>,
	},
	WhileLoop {
		check: Box<ExprKind>,
		body: Box<Block>,
	},
	ForLoop {
		pat: Ident,
		iter: Box<ExprKind>,
		body: Box<Block>,
	},

	Let {
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
