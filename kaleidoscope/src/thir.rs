use crate::{Ident, ast, lexer::Literal};

#[derive(Debug)]
pub struct Thir<'lcx> {
	/// Roots of the typed IR
	pub items: Vec<&'lcx ItemKind<'lcx>>,
	//
	// blocks: IndexVec<BlkId, Block>,
	// exprs: IndexVec<ExprId, Expr>,
	// stmts: IndexVec<StmtId, Stmt>,
}

#[derive(Debug)]
pub enum ItemKind<'lcx> {
	Extern {
		ident: Ident,
		decl: &'lcx FnDecl<'lcx>,
	},
	Function {
		ident: Ident,
		decl: &'lcx FnDecl<'lcx>,
		body: &'lcx Block<'lcx>,
	},
}

#[derive(Debug)]
pub struct FnDecl<'lcx> {
	pub inputs: &'lcx [ast::TyKind],
	pub output: &'lcx ast::TyKind,
}

#[derive(Debug)]
pub struct Block<'lcx> {
	pub stmts: &'lcx [Stmt<'lcx>],
	pub ret_expr: Option<&'lcx Expr>,
}

#[derive(Debug)]
pub struct Stmt<'lcx> {
	pub kind: StmtKind<'lcx>,
}

#[derive(Debug)]
pub enum StmtKind<'lcx> {
	Expr(&'lcx Expr),
	Let {
		name: Ident,
		value: &'lcx Expr,
		ty: &'lcx ast::TyKind,
	},
	Assign {
		target: Ident,
		value: &'lcx Expr,
	},
}

#[derive(Debug)]
pub struct Expr {
	pub ty: ast::TyKind,
	pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
	Variable(Ident),
	Literal(Literal),
}
