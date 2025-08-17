use std::{
	collections::HashMap,
	sync::atomic::{AtomicU32, Ordering},
};

use crate::{
	ast::{self, Ident},
	hir, lexer,
	resolve::Environment,
	session::{SessionCtx, Symbol},
	tbir,
};

#[derive(Debug)]
pub struct TyCtx<'scx> {
	pub scx: &'scx SessionCtx,

	infer_tag_count: AtomicU32,
}

impl<'scx> TyCtx<'scx> {
	#[must_use]
	pub fn new(scx: &'scx SessionCtx) -> Self {
		Self {
			scx,
			infer_tag_count: AtomicU32::default(),
		}
	}
}

/// Context actions
impl TyCtx<'_> {
	#[must_use]
	#[tracing::instrument(level = "trace", skip(self, decl, body, env))]
	pub fn typeck_fn(
		&self,
		name: Ident,
		decl: &FnDecl,
		body: &hir::Block<'_>,
		env: &Environment,
	) -> tbir::Block {
		let mut inferer = Inferer::new(self, decl, body, &env.values);
		inferer.infer_fn();
		inferer.build_body()
	}
}

impl TyCtx<'_> {
	fn next_infer_tag(&self) -> InferTag {
		InferTag(self.infer_tag_count.fetch_add(1, Ordering::Relaxed))
	}

	fn lower_ty(&self, ty: &ast::Ty) -> TyKind<Infer> {
		match &ty.kind {
			ast::TyKind::Path(path, _generics) => self.lower_path_ty(path[0]),
			ast::TyKind::Unit => TyKind::Unit,
			ast::TyKind::Infer => TyKind::Infer(self.next_infer_tag(), Infer::Explicit),
		}
	}

	// TODO: not pub
	pub fn lower_fn_decl(&self, decl: &hir::FnDecl) -> FnDecl {
		// TODO: diag no infer ty in functions
		let inputs = decl
			.inputs
			.iter()
			.map(|(ident, ty)| Param {
				ident: *ident,
				ty: self.lower_ty(ty).as_no_infer().unwrap(),
			})
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
	expr_type: HashMap<hir::NodeId, TyKind<Infer>>,
	infer_map: HashMap<InferTag, TyKind<Infer>>,
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

			infer_map: HashMap::default(),
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

		self.decl.inputs.iter().for_each(|Param { ident, ty }| {
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
				lexer::LiteralKind::Integer => {
					TyKind::Infer(self.tcx.next_infer_tag(), Infer::Integer)
				}
				lexer::LiteralKind::Float => TyKind::Infer(self.tcx.next_infer_tag(), Infer::Float),
				lexer::LiteralKind::Str => TyKind::Str,
			},
			hir::ExprKind::Binary(op, left, right) => {
				let left = self.infer_expr(left);
				let right = self.infer_expr(right);

				// TODO: allow with bools
				self.unify(&TyKind::Integer, &left);
				self.unify(&TyKind::Integer, &right);

				#[allow(clippy::enum_glob_use)]
				let expected = {
					use lexer::BinOp::*;
					match op.bit {
						Plus | Minus | Mul | Div | Mod => TyKind::Integer,
						Gt | Ge | Lt | Le | EqEq | Ne => TyKind::Bool,
					}
				};

				expected
			}
			hir::ExprKind::FnCall { expr, args } => {
				let expr_ty = self.infer_expr(expr);

				let TyKind::Fn(fn_) = expr_ty else {
					todo!("you can only call functions");
				};

				if fn_.inputs.len() != args.len() {
					todo!("args count mismatch");
				}

				for (Param { ty: expected, .. }, actual) in fn_.inputs.iter().zip(args.iter()) {
					let actual_ty = self.infer_expr(actual);
					self.unify(&expected.clone().as_infer(), &actual_ty);
				}

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

			hir::ExprKind::Return(_) | hir::ExprKind::Break(_) | hir::ExprKind::Continue => {
				TyKind::Never
			}
		};

		// TODO
		assert!(self.expr_type.insert(expr.id, ty.clone()).is_none());

		ty
	}
}

/// Unification
impl Inferer<'_> {
	fn unify(&mut self, expected: &TyKind<Infer>, actual: &TyKind<Infer>) -> TyKind<Infer> {
		tracing::trace!(?expected, ?actual, "unify");
		match (expected, actual) {
			(TyKind::Infer(tag, infer), ty) | (ty, TyKind::Infer(tag, infer)) => {
				self.unify_infer(*tag, *infer, ty)
			}
			// infer and never have different meaning but both coerces to anything
			(TyKind::Never, ty) | (ty, TyKind::Never) => ty.clone(),

			(_, _) if expected == actual => expected.clone(),
			(_, _) => todo!("ty mismatch `{expected:?}` vs. `{actual:?}`"),
		}
	}

	fn unify_infer(&mut self, tag: InferTag, infer: Infer, other: &TyKind<Infer>) -> TyKind<Infer> {
		tracing::trace!(?tag, ?infer, ?other, "unify_infer");
		let unified = match (infer, other) {
			(Infer::Integer, TyKind::Integer) => TyKind::Integer,
			(Infer::Float, TyKind::Float) => TyKind::Float,
			(Infer::Generic | Infer::Explicit, ty) => ty.clone(),

			(_, TyKind::Infer(tag, actual_infer)) => {
				if infer == *actual_infer {
					TyKind::Infer(*tag, *actual_infer)
				} else {
					panic!(
						"infer kind mismatch: expected infer {{{infer:?}}}, received infer {{{actual_infer:?}}}"
					)
				}
			}
			(_, ty) => {
				panic!("infer kind mismatch: expected infer {{{infer:?}}}, received ty {ty:?}")
			}
		};

		self.infer_map.insert(tag, unified.clone());

		unified
	}

	#[tracing::instrument(level = "trace", skip(self), ret)]
	fn resolve_ty(&self, id: hir::NodeId) -> TyKind {
		let mut tag = match self.expr_type.get(&id).cloned().unwrap().as_no_infer() {
			Ok(ty) => return ty,
			Err((tag, _)) => tag,
		};

		loop {
			// tracing::debug!(?tag, "{:?}", self.infer_map);
			match self.infer_map.get(&tag).cloned().unwrap().as_no_infer() {
				Ok(ty) => return ty,
				Err((new_tag, _)) => tag = new_tag,
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
	pub ident: ast::Ident,
	pub ty: TyKind,
}

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

	Infer(InferTag, InferKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferTag(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoInfer {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
	pub fn as_no_infer(self) -> Result<TyKind<NoInfer>, (InferTag, Infer)> {
		match self {
			Self::Unit => Ok(TyKind::Unit),
			Self::Never => Ok(TyKind::Never),
			Self::Bool => Ok(TyKind::Bool),
			Self::Integer => Ok(TyKind::Integer),
			Self::Float => Ok(TyKind::Float),
			Self::Str => Ok(TyKind::Str),
			Self::Fn(fn_) => Ok(TyKind::Fn(fn_)),
			Self::Infer(tag, infer) => Err((tag, infer)),
		}
	}
}

/// TBIR construction
impl Inferer<'_> {
	fn build_body(&self) -> tbir::Block {
		self.build_block(self.body)
	}
	fn build_block(&self, block: &hir::Block) -> tbir::Block {
		let ret = block.ret.map(|expr| self.build_expr(expr));
		let ty = ret.as_ref().map_or(TyKind::Unit, |expr| expr.ty.clone());

		tbir::Block {
			stmts: block
				.stmts
				.iter()
				.map(|stmt| self.build_stmt(stmt))
				.collect(),
			ret,
			ty,
			span: block.span,
			id: block.id,
		}
	}

	fn build_stmt(&self, stmt: &hir::Stmt<'_>) -> tbir::Stmt {
		let kind = match stmt.kind {
			hir::StmtKind::Expr(expr) => tbir::StmtKind::Expr(self.build_expr(expr)),
			hir::StmtKind::Let {
				ident,
				ty: _,
				value,
			} => tbir::StmtKind::Let {
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
			hir::ExprKind::Return(expr) => {
				tbir::ExprKind::Return(expr.map(|expr| Box::new(self.build_expr(expr))))
			}
			hir::ExprKind::Break(expr) => {
				tbir::ExprKind::Break(expr.map(|expr| Box::new(self.build_expr(expr))))
			}

			hir::ExprKind::Continue => tbir::ExprKind::Continue,
		};

		tbir::Expr {
			kind,
			ty: self.resolve_ty(expr.id),
			span: expr.span,
			id: expr.id,
		}
	}
}
