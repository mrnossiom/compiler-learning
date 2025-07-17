use std::collections::HashMap;

use crate::{ast, front::FrontCtx, hir, lexer};

#[derive(Debug)]
pub struct TyCtx<'fcx> {
	fcx: &'fcx FrontCtx,
}

impl<'fcx> TyCtx<'fcx> {
	pub const fn new(fcx: &'fcx FrontCtx) -> Self {
		Self { fcx }
	}
}

impl TyCtx<'_> {
	fn lower_ty(&self, ty: &ast::TyKind) -> TyKind {
		match ty {
			ast::TyKind::Path(path, _generics) => self.lower_path_ty(path[0]),
			ast::TyKind::Unit => TyKind::Unit,
			ast::TyKind::Infer => TyKind::Infer,
		}
	}

	fn lower_path_ty(&self, path: ast::Ident) -> TyKind {
		match self.fcx.symbols.resolve(path.name).as_str() {
			"number" => TyKind::Integer,
			"str" => TyKind::Str,

			// TODO: remove
			"uint" => TyKind::Integer,

			_ => panic!("ty undefined {path:?}"),
		}
	}
}

pub struct Collector<'tcx> {
	tcx: &'tcx TyCtx<'tcx>,
}

impl<'tcx> Collector<'tcx> {
	pub const fn new(tcx: &'tcx TyCtx) -> Self {
		Self { tcx }
	}
}

pub struct Inferer<'tcx> {
	tcx: &'tcx TyCtx<'tcx>,
}

type FnTyEnv = HashMap<ast::Ident, Vec<TyKind>>;

impl<'tcx> Inferer<'tcx> {
	pub const fn new(tcx: &'tcx TyCtx) -> Self {
		Self { tcx }
	}
}

impl Inferer<'_> {
	pub fn infer_fn<'lcx>(&self, decl: &'lcx hir::FnDecl<'lcx>, body: &'lcx hir::Block<'lcx>) {
		let mut env: FnTyEnv = HashMap::new();

		// init context with function arguments
		decl.inputs.iter().for_each(|(ident, arg_ty)| {
			let arg_ty = self.tcx.lower_ty(arg_ty);
			env.entry(*ident).or_default().push(arg_ty);
		});

		let expected_ret_ty = self.tcx.lower_ty(decl.output);
		let ret_ty = self.infer_block(&mut env, body);
		self.unify(&expected_ret_ty, &ret_ty);
	}

	fn infer_block<'lcx>(&self, env: &mut FnTyEnv, block: &'lcx hir::Block<'lcx>) -> TyKind {
		for stmt in block.stmts {
			self.infer_stmt(env, stmt);
		}

		let expected_ret_ty = block
			.ret_expr
			.map_or(TyKind::Unit, |expr| self.infer_expr(env, expr));

		expected_ret_ty
	}

	fn infer_stmt<'lcx>(&self, env: &mut FnTyEnv, stmt: &'lcx hir::Stmt<'lcx>) {
		match &stmt.kind {
			hir::StmtKind::Expr(expr) => {
				self.infer_expr(env, expr);
			}
			hir::StmtKind::Let { name, value, ty } => {
				let explicit_ty = self.tcx.lower_ty(ty);
				let expr_ty = self.infer_expr(env, value);
				self.unify(&explicit_ty, &expr_ty);

				env.entry(*name).or_default().push(expr_ty);
			}
			hir::StmtKind::Assign { target, value } => {
				let target_ty = env.get(target).unwrap().last().unwrap().clone();
				let expr_ty = self.infer_expr(env, value);
				self.unify(&target_ty, &expr_ty);
			}
			hir::StmtKind::Loop { block } => {
				let block_ty = self.infer_block(env, block);
				self.unify(&TyKind::Unit, &block_ty);
			}
		}
	}

	fn infer_expr<'lcx>(&self, env: &mut FnTyEnv, expr: &'lcx hir::Expr<'lcx>) -> TyKind {
		match &expr.kind {
			hir::ExprKind::Variable(ident) => {
				env.get(ident).and_then(|v| v.last()).unwrap().clone()
			}
			hir::ExprKind::Literal(lit, _ident) => match lit {
				lexer::LiteralKind::Integer => TyKind::Integer,
				lexer::LiteralKind::Float => TyKind::Float,
				lexer::LiteralKind::Str => TyKind::Str,
			},
			hir::ExprKind::Binary(op, left, right) => {
				let left = self.infer_expr(env, left);
				let right = self.infer_expr(env, right);
				let op_ty = self.unify(&left, &right);

				// TODO: unify both with number infer

				#[allow(clippy::enum_glob_use)]
				{
					use lexer::BinOp::*;
					match op {
						Plus | Minus | Mul | Div | Mod => TyKind::Integer,
						Gt | Ge | Lt | Le | EqEq | Ne => TyKind::Bool,
					}
				}
			}
			hir::ExprKind::FnCall { expr, args } => {
				let expr_ty = self.infer_expr(env, expr);
				// TODO: unify with function

				match expr_ty {
					TyKind::Fn(decl) => decl.output,
					_ => todo!("tried to call not a function"),
				}
			}
			hir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => {
				let cond_ty = self.infer_expr(env, cond);
				self.unify(&TyKind::Bool, &cond_ty);

				let conseq_ty = self.infer_block(env, conseq);
				// if no `else` part, then it must return Unit
				let altern_ty = altern
					.map(|altern| self.infer_block(env, altern))
					.unwrap_or(TyKind::Unit);

				self.unify(&conseq_ty, &altern_ty)
			}

			hir::ExprKind::Break(_) | hir::ExprKind::Continue => TyKind::Never,
		}
	}
}

/// Unification
impl Inferer<'_> {
	fn unify(&self, expected: &TyKind, actual: &TyKind) -> TyKind {
		#[expect(clippy::match_same_arms)]
		match (expected, actual) {
			(TyKind::Infer, ty) | (ty, TyKind::Infer) => ty.clone(),
			// works like inference
			(TyKind::Never, ty) | (ty, TyKind::Never) => ty.clone(),

			(_, _) if expected == actual => expected.clone(),

			(_, _) => todo!("ty mismatch {expected:?} vs. {actual:?}"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
	inputs: Vec<TyKind>,
	output: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
	Unit,
	Never,

	Bool,
	Integer,
	Float,
	Str,

	Fn(Box<FnDecl>),

	// move elsewhere?
	Infer,
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum InferrableTy {
// 	Concrete(TyKind),
// 	Infer,
// }
