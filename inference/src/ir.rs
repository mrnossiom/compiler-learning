pub struct Block {
	pub stmts: Vec<StmtKind>,
}

impl Block {
	pub fn new(stmts: Vec<StmtKind>) -> Self {
		Self { stmts }
	}
}

#[derive(Clone)]
pub struct Ident(pub String);

pub enum StmtKind {
	Let {
		ident: Ident,
		ty: Option<TyKind>,
		value: ExprKind,
	},

	Expr(ExprKind),

	Assign {
		ident: Ident,
		value: ExprKind,
	},
	While {
		cond: ExprKind,
		body: Block,
	},
	Return(ExprKind),
}

pub enum ExprKind {
	Number(String),
	Str(String),

	Ident(String),

	Bin(String, Box<ExprKind>, Box<ExprKind>),
}

pub enum TyKind {
	Ident(Ident),
}

pub struct Function {
	pub name: Ident,
	pub params: Vec<(Ident, TyKind)>,
	pub ret_type: Option<TyKind>,
	pub body: Block,
}
