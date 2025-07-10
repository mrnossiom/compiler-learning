use crate::{
	Ident,
	lexer::{BinOp, Literal},
};

#[derive(Debug)]
pub struct NumberExpr {
	pub value: i64,
}

#[derive(Debug)]
pub enum ExprKind {
	Variable(Ident),
	Literal(Literal),

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
}

#[derive(Debug)]
pub struct FnDecl {
	pub args: Vec<(Ident, TyKind)>,
	pub ret: TyKind,
}

#[derive(Debug)]
pub struct Function {
	pub proto: FnDecl,
	pub body: Block,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Path(Ident),

	Unit,

	Infer,
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
