use std::collections::HashMap;

use crate::{
	ast, hir, lexer,
	resolve::Environment,
	session::{SessionCtx, Symbol},
	tbir,
};

#[derive(Debug)]
pub struct TyCtx<'scx> {
	scx: &'scx SessionCtx,
}

impl<'scx> TyCtx<'scx> {
	#[must_use]
	pub const fn new(scx: &'scx SessionCtx) -> Self {
		Self { scx }
	}
}

/// Context actions
impl TyCtx<'_> {
	#[must_use]
	pub fn typeck_fn(
		&self,
		decl: &FnDecl,
		body: &hir::Block<'_>,
		env: &Environment,
	) -> tbir::Block {
		let mut inferer = Inferer::new(self, decl, body, &env.values);
		inferer.infer_fn();

		tracing::debug!(expr_type = ?inferer.expr_type);

		inferer.build_body()
	}
}

impl TyCtx<'_> {
	fn lower_ty(&self, ty: &ast::Ty) -> TyKind<Infer> {
		match &ty.kind {
			ast::TyKind::Path(path, _generics) => self.lower_path_ty(path[0]),
			ast::TyKind::Unit => TyKind::Unit,
			ast::TyKind::Infer => TyKind::Infer(Infer::Explicit),
		}
	}

	// TODO: not pub
	pub fn lower_fn_decl(&self, decl: &hir::FnDecl) -> FnDecl {
		// TODO: diag no infer ty in functions
		let inputs = decl
			.inputs
			.iter()
			.map(|(ident, ty)| Param(*ident, self.lower_ty(ty).as_no_infer().unwrap()))
			.collect();
		FnDecl {
			inputs,
			output: self.lower_ty(decl.output).as_no_infer().unwrap(),
		}
	}

	fn lower_path_ty(&self, path: ast::Ident) -> TyKind<Infer> {
		match self.scx.symbols.resolve(path.name).as_str() {
			"number" => TyKind::Integer,
			"str" => TyKind::Str,

			// TODO: remove
			"uint" => TyKind::Integer,

			_ => panic!("ty undefined {path:?}"),
		}
	}
}

#[derive(Debug)]
pub struct Inferer<'tcx> {
	tcx: &'tcx TyCtx<'tcx>,
	item_env: &'tcx HashMap<Symbol, TyKind>,

	decl: &'tcx FnDecl,
	body: &'tcx hir::Block<'tcx>,

	local_env: HashMap<Symbol, Vec<TyKind<Infer>>>,
	expr_type: HashMap<hir::NodeId, TyKind>,
}

impl<'tcx> Inferer<'tcx> {
	#[must_use]
	pub fn new(
		tcx: &'tcx TyCtx,
		decl: &'tcx FnDecl,
		body: &'tcx hir::Block<'tcx>,
		item_env: &'tcx HashMap<Symbol, TyKind>,
	) -> Self {
		Self {
			tcx,
			item_env,

			decl,
			body,

			local_env: HashMap::default(),
			expr_type: HashMap::default(),
		}
	}
}

impl Inferer<'_> {
	pub fn infer_fn(&mut self) {
		// TODO: remove
		for (fn_, decl) in self.item_env {
			self.local_env.insert(*fn_, vec![decl.clone().as_infer()]);
		}

		// init context with function arguments

		self.decl.inputs.iter().for_each(|Param(ident, ty)| {
			self.local_env
				.entry(ident.name)
				.or_default()
				.push(ty.clone().as_infer());
		});

		let ret_ty = self.infer_block(self.body);
		self.unify(&self.decl.output.clone().as_infer(), &ret_ty);
	}

	fn infer_block<'lcx>(&mut self, block: &'lcx hir::Block<'lcx>) -> TyKind<Infer> {
		for stmt in block.stmts {
			self.infer_stmt(stmt);
		}

		let expected_ret_ty = block.ret.map_or(TyKind::Unit, |expr| self.infer_expr(expr));

		#[expect(clippy::let_and_return)]
		expected_ret_ty
	}

	fn infer_stmt<'lcx>(&mut self, stmt: &'lcx hir::Stmt<'lcx>) {
		match &stmt.kind {
			hir::StmtKind::Expr(expr) => {
				self.infer_expr(expr);
			}
			hir::StmtKind::Let { ident, value, ty } => {
				let explicit_ty = self.tcx.lower_ty(ty);
				let expr_ty = self.infer_expr(value);
				self.unify(&explicit_ty, &expr_ty);

				self.local_env.entry(ident.name).or_default().push(expr_ty);
			}
			hir::StmtKind::Assign { target, value } => {
				let target_ty = self
					.local_env
					.get(&target.name)
					.unwrap()
					.last()
					.unwrap()
					.clone();
				let expr_ty = self.infer_expr(value);
				self.unify(&target_ty, &expr_ty);
			}
			hir::StmtKind::Loop { block } => {
				let block_ty = self.infer_block(block);
				self.unify(&TyKind::Unit, &block_ty);
			}
		}
	}

	fn infer_expr<'lcx>(&mut self, expr: &'lcx hir::Expr<'lcx>) -> TyKind<Infer> {
		let ty = match &expr.kind {
			hir::ExprKind::Variable(ident) => self
				.local_env
				.get(&ident.name)
				.and_then(|v| v.last())
				.cloned()
				.unwrap_or_else(|| panic!("unknown variable {:?}", ident.name)),
			hir::ExprKind::Literal(lit, _ident) => match lit {
				lexer::LiteralKind::Integer => TyKind::Infer(Infer::Integer),
				lexer::LiteralKind::Float => TyKind::Infer(Infer::Float),
				lexer::LiteralKind::Str => TyKind::Str,
			},
			hir::ExprKind::Binary(op, left, right) => {
				let left = self.infer_expr(left);
				let right = self.infer_expr(right);
				let op_ty = self.unify(&left, &right);

				// TODO: unify both with number infer

				#[allow(clippy::enum_glob_use)]
				{
					use lexer::BinOp::*;
					match op.bit {
						Plus | Minus | Mul | Div | Mod => TyKind::Integer,
						Gt | Ge | Lt | Le | EqEq | Ne => TyKind::Bool,
					}
				}
			}
			hir::ExprKind::FnCall { expr, args } => {
				let expr_ty = self.infer_expr(expr);

				let TyKind::Fn(fn_) = expr_ty else {
					todo!("you can only call functions");
				};

				if fn_.inputs.len() != args.len() {
					todo!("args count mismatch");
				}

				for (Param(_, expected), actual) in fn_.inputs.iter().zip(args.iter()) {
					let actual_ty = self.infer_expr(actual);
					self.unify(&expected.clone().as_infer(), &actual_ty);

					assert!(self.expr_type.insert(actual.id, expected.clone()).is_none());
				}

				assert!(self.expr_type.insert(expr.id, fn_.output.clone()).is_none());

				fn_.output.as_infer()
			}
			hir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => {
				let cond_ty = self.infer_expr(cond);
				self.unify(&TyKind::Bool, &cond_ty);

				let conseq_ty = self.infer_block(conseq);
				// if no `else` part, then it must return Unit
				let altern_ty = altern
					.map(|altern| self.infer_block(altern))
					.unwrap_or(TyKind::Unit);

				self.unify(&conseq_ty, &altern_ty)
			}

			hir::ExprKind::Break(_) | hir::ExprKind::Continue => TyKind::Never,
		};

		// TODO: check
		if let Some(ty_noinf) = ty.clone().as_no_infer() {
			assert!(self.expr_type.insert(expr.id, ty_noinf).is_none());
		}

		ty
	}
}

/// Unification
impl Inferer<'_> {
	#[tracing::instrument(skip(self), ret)]
	fn unify(&self, expected: &TyKind<Infer>, actual: &TyKind<Infer>) -> TyKind<Infer> {
		match (expected, actual) {
			(TyKind::Infer(infer), ty) | (ty, TyKind::Infer(infer)) => self.unify_infer(infer, ty),
			// infer and never have different meaning but both coerces to anything
			(TyKind::Never, ty) | (ty, TyKind::Never) => ty.clone(),

			(_, _) if expected == actual => expected.clone(),
			(_, _) => todo!("ty mismatch `{expected:?}` vs. `{actual:?}`"),
		}
	}

	fn unify_infer(&self, infer: &Infer, other: &TyKind<Infer>) -> TyKind<Infer> {
		match (infer, other) {
			(Infer::Integer, TyKind::Integer) => TyKind::Integer,
			(Infer::Float, TyKind::Float) => TyKind::Float,
			(Infer::Generic | Infer::Explicit, ty) => ty.clone(),

			(_, TyKind::Infer(actual_infer)) => {
				if infer == actual_infer {
					TyKind::Infer(infer.clone())
				} else {
					panic!(
						"infer kind mismatch: expected infer {{{infer:?}}}, recieved infer {{{actual_infer:?}}}"
					)
				}
			}
			(_, ty) => {
				panic!("infer kind mismatch: expected infer {{{infer:?}}}, recieved ty {ty:?}")
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param(pub ast::Ident, pub TyKind);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
	pub inputs: Vec<Param>,
	pub output: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind<InferKind = NoInfer> {
	Unit,
	Never,

	Bool,
	Integer,
	Float,
	Str,

	Fn(Box<FnDecl>),

	Infer(InferKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NoInfer {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Infer {
	Integer,
	Float,

	Generic,
	Explicit,
}

impl TyKind<NoInfer> {
	#[must_use]
	pub fn as_infer(self) -> TyKind<Infer> {
		match self {
			Self::Unit => TyKind::Unit,
			Self::Never => TyKind::Never,
			Self::Bool => TyKind::Bool,
			Self::Integer => TyKind::Integer,
			Self::Float => TyKind::Float,
			Self::Str => TyKind::Str,
			Self::Fn(fn_) => TyKind::Fn(fn_),
		}
	}
}

impl TyKind<Infer> {
	#[must_use]
	pub fn as_no_infer(self) -> Option<TyKind<NoInfer>> {
		match self {
			Self::Unit => Some(TyKind::Unit),
			Self::Never => Some(TyKind::Never),
			Self::Bool => Some(TyKind::Bool),
			Self::Integer => Some(TyKind::Integer),
			Self::Float => Some(TyKind::Float),
			Self::Str => Some(TyKind::Str),
			Self::Fn(fn_) => Some(TyKind::Fn(fn_)),
			Self::Infer(_) => None,
		}
	}
}

/// TBIR construction
impl Inferer<'_> {
	fn build_body(&self) -> tbir::Block {
		self.build_block(self.body)
	}
	fn build_block(&self, block: &hir::Block) -> tbir::Block {
		tbir::Block {
			stmts: block
				.stmts
				.iter()
				.map(|stmt| self.build_stmt(stmt))
				.collect(),
			ret: block.ret.map(|expr| self.build_expr(expr)),
		}
	}

	fn build_stmt(&self, stmt: &hir::Stmt<'_>) -> tbir::Stmt {
		let kind = match stmt.kind {
			hir::StmtKind::Expr(expr) => tbir::StmtKind::Expr(self.build_expr(expr)),
			hir::StmtKind::Let { ident, ty, value } => tbir::StmtKind::Let {
				ident,
				value: self.build_expr(value),
			},
			hir::StmtKind::Assign { target, value } => tbir::StmtKind::Assign {
				target,
				value: self.build_expr(value),
			},
			hir::StmtKind::Loop { block } => tbir::StmtKind::Loop {
				block: self.build_block(block),
			},
		};
		tbir::Stmt {
			kind,
			span: stmt.span,
			id: stmt.id,
		}
	}

	fn build_expr(&self, expr: &hir::Expr<'_>) -> tbir::Expr {
		let kind = match expr.kind {
			hir::ExprKind::Variable(ident) => tbir::ExprKind::Variable(ident),
			hir::ExprKind::Literal(kind, sym) => tbir::ExprKind::Literal(kind, sym),
			hir::ExprKind::Binary(op, left, right) => tbir::ExprKind::Binary(
				op,
				Box::new(self.build_expr(left)),
				Box::new(self.build_expr(right)),
			),
			hir::ExprKind::FnCall { expr, args } => tbir::ExprKind::FnCall {
				expr: Box::new(self.build_expr(expr)),
				args: args.iter().map(|arg| self.build_expr(arg)).collect(),
			},
			hir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => tbir::ExprKind::If {
				cond: Box::new(self.build_expr(cond)),
				conseq: Box::new(self.build_block(conseq)),
				altern: altern.map(|altern| Box::new(self.build_block(altern))),
			},
			hir::ExprKind::Break(expr) => {
				tbir::ExprKind::Break(expr.map(|expr| Box::new(self.build_expr(expr))))
			}
			hir::ExprKind::Continue => tbir::ExprKind::Continue,
		};

		tracing::debug!(?expr);
		tbir::Expr {
			kind,
			ty: self.expr_type.get(&expr.id).unwrap().clone(),
			span: expr.span,
			id: expr.id,
		}
	}
}
