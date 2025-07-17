//! Higher IR

use crate::{ast, front::Symbol, lexer};

#[derive(Debug)]
pub struct Hir<'lcx> {
	/// Roots of the typed IR
	pub items: &'lcx [ItemKind<'lcx>],
	//
	// blocks: IndexVec<BlkId, Block>,
	// exprs: IndexVec<ExprId, Expr>,
	// stmts: IndexVec<StmtId, Stmt>,
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
	pub inputs: &'lcx [(ast::Ident, ast::TyKind)],
	pub output: &'lcx ast::TyKind,
}

#[derive(Debug)]
pub struct Block<'lcx> {
	pub stmts: &'lcx [Stmt<'lcx>],
	pub ret_expr: Option<&'lcx Expr<'lcx>>,
}

#[derive(Debug)]
pub struct Stmt<'lcx> {
	pub kind: StmtKind<'lcx>,
}

#[derive(Debug)]
pub enum StmtKind<'lcx> {
	Expr(&'lcx Expr<'lcx>),

	Let {
		name: ast::Ident,
		value: &'lcx Expr<'lcx>,
		ty: &'lcx ast::TyKind,
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
}

#[derive(Debug)]
pub enum ExprKind<'lcx> {
	Variable(ast::Ident),
	Literal(lexer::LiteralKind, Symbol),

	Binary(lexer::BinOp, &'lcx Expr<'lcx>, &'lcx Expr<'lcx>),

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
