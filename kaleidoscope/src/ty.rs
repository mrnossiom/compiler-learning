use std::{
	cell::RefCell,
	collections::HashMap,
	fmt,
	sync::atomic::{AtomicU32, Ordering},
};

use crate::{
	ast::{self, Ident},
	bug, errors, hir,
	lexer::{self, UnaryOp},
	resolve::Environment,
	session::{SessionCtx, Span, Symbol},
	tbir,
};

#[derive(Debug)]
pub struct TyCtx<'scx> {
	pub scx: &'scx SessionCtx,

	pub environment: RefCell<Option<Environment>>,

	infer_tag_count: AtomicU32,
}

impl<'scx> TyCtx<'scx> {
	#[must_use]
	pub fn new(scx: &'scx SessionCtx) -> Self {
		Self {
			scx,
			environment: RefCell::default(),
			infer_tag_count: AtomicU32::default(),
		}
	}
}

/// Context actions
impl TyCtx<'_> {
	#[must_use]
	#[tracing::instrument(level = "trace", skip(self, decl, body,))]
	pub fn typeck_fn(&self, name: Ident, decl: &FnDecl, body: &hir::Block) -> tbir::Block {
		let env = self.environment.borrow_mut().take().unwrap();
		// defer put back

		let mut inferer = Inferer::new(self, decl, body, &env.values);
		inferer.infer_fn();

		let mut expr_tys = HashMap::default();

		for ((span, node_id), ty_infer) in inferer.expr_type {
			match ty_infer.as_no_infer() {
				Ok(ty) => {
					expr_tys.insert(node_id, ty);
				}
				Err((mut tag, infer)) => loop {
					let Some(ty) = inferer.infer_map.get(&tag) else {
						// set default types for expression that can be inferred via literals
						match infer {
							Infer::Integer => {
								expr_tys
									.insert(node_id, TyKind::Primitive(PrimitiveKind::SignedInt));
							}
							Infer::Float => {
								expr_tys.insert(node_id, TyKind::Primitive(PrimitiveKind::Float));
							}
							Infer::Generic | Infer::Explicit => {
								let report = errors::ty::report_unconstrained(span);
								self.scx.dcx().emit_build(report);
							}
						}
						break;
					};
					match ty.clone().as_no_infer() {
						Ok(ty) => {
							expr_tys.insert(node_id, ty);
							break;
						}
						Err((next_tag, _)) => tag = next_tag,
					}
				},
			}
		}

		let tbir_builder = TbirBuilder {
			body: inferer.body,
			expr_tys,
		};
		let body = tbir_builder.build_body();

		// put back
		self.environment.borrow_mut().replace(env);

		body
	}
}

impl TyCtx<'_> {
	fn next_infer_tag(&self) -> InferTag {
		InferTag(self.infer_tag_count.fetch_add(1, Ordering::Relaxed))
	}

	fn lower_ty(&self, ty: &ast::Ty) -> TyKind<Infer> {
		match &ty.kind {
			ast::TyKind::Path(path) => self.lower_path_ty(path),
			ast::TyKind::Pointer(ty) => TyKind::Pointer(Box::new(self.lower_ty(ty))),
			ast::TyKind::Unit => TyKind::Primitive(PrimitiveKind::Void),
			ast::TyKind::Infer => TyKind::Infer(self.next_infer_tag(), Infer::Explicit),
		}
	}

	// TODO: not pub
	pub fn lower_fn_decl(&self, decl: &hir::FnDecl) -> FnDecl {
		// TODO: diag no infer ty in functions
		let inputs = decl
			.inputs
			.iter()
			.map(|ast::Param { name, ty }| {
				let ty = if let Ok(ty) = self.lower_ty(ty).as_no_infer() {
					ty
				} else {
					let report = errors::ty::function_cannot_infer_signature(name.span);
					self.scx.dcx().emit_build(report);
					TyKind::Error
				};
				Param { name: *name, ty }
			})
			.collect();

		let ty = if let Ok(ty) = self.lower_ty(&decl.output).as_no_infer() {
			ty
		} else {
			let report = errors::ty::function_cannot_infer_signature(decl.output.span);
			self.scx.dcx().emit_build(report);
			TyKind::Error
		};
		FnDecl { inputs, output: ty }
	}

	fn lower_path_ty(&self, path: &ast::Path) -> TyKind<Infer> {
		// TODO: remove these constraints
		assert_eq!(path.segments.len(), 1);
		assert_eq!(path.generics.len(), 0);

		let path = path.segments[0];
		match self.scx.symbols.resolve(path.sym).as_str() {
			"_" => TyKind::Infer(self.next_infer_tag(), Infer::Explicit),

			"void" => TyKind::Primitive(PrimitiveKind::Void),
			"never" => TyKind::Primitive(PrimitiveKind::Never),

			"bool" => TyKind::Primitive(PrimitiveKind::Bool),
			"uint" => TyKind::Primitive(PrimitiveKind::UnsignedInt),
			"sint" => TyKind::Primitive(PrimitiveKind::SignedInt),
			"float" => TyKind::Primitive(PrimitiveKind::Float),

			"str" => TyKind::Primitive(PrimitiveKind::Str),

			_ => {
				let report = errors::ty::type_unknown(path.span);
				self.scx.dcx().emit_build(report);
				TyKind::Error
			}
		}
	}
}

#[derive(Debug)]
pub struct Inferer<'tcx> {
	tcx: &'tcx TyCtx<'tcx>,
	item_env: &'tcx HashMap<Symbol, TyKind>,

	decl: &'tcx FnDecl,
	body: &'tcx hir::Block,

	local_env: HashMap<Symbol, Vec<TyKind<Infer>>>,
	// get this span out of here once we have an easy NodeId -> Span way
	expr_type: HashMap<(Span, hir::NodeId), TyKind<Infer>>,
	infer_map: HashMap<InferTag, TyKind<Infer>>,
}

impl<'tcx> Inferer<'tcx> {
	#[must_use]
	pub fn new(
		tcx: &'tcx TyCtx,
		decl: &'tcx FnDecl,
		body: &'tcx hir::Block,
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

	fn resolve_var_ty(&self, var: &ast::Path) -> TyKind<Infer> {
		// TODO: resolve full path
		let var = var.segments[0];

		if let Some(ty) = self
			.local_env
			.get(&var.sym)
			.and_then(|ty_kinds| ty_kinds.last())
		{
			ty.clone()
		} else {
			let report = errors::ty::variable_not_in_scope(var.span);
			self.tcx.scx.dcx().emit_build(report);
			TyKind::Error
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

		self.decl
			.inputs
			.iter()
			.for_each(|Param { name: ident, ty }| {
				self.local_env
					.entry(ident.sym)
					.or_default()
					.push(ty.clone().as_infer());
			});

		let ret_ty = self.infer_block(self.body);
		self.unify(&self.decl.output.clone().as_infer(), &ret_ty);
	}

	fn infer_block(&mut self, block: &hir::Block) -> TyKind<Infer> {
		for stmt in &block.stmts {
			self.infer_stmt(stmt);
		}

		let expected_ret_ty = block
			.ret
			.as_ref()
			.map_or(TyKind::Primitive(PrimitiveKind::Void), |expr| {
				self.infer_expr(expr)
			});

		#[expect(clippy::let_and_return)]
		expected_ret_ty
	}

	fn infer_stmt(&mut self, stmt: &hir::Stmt) {
		match &stmt.kind {
			hir::StmtKind::Expr(expr) => {
				self.infer_expr(expr);
			}
			hir::StmtKind::Let { ident, value, ty } => {
				let explicit_ty = self.tcx.lower_ty(ty);
				let expr_ty = self.infer_expr(value);
				self.unify(&explicit_ty, &expr_ty);

				self.local_env.entry(ident.sym).or_default().push(expr_ty);
			}
			hir::StmtKind::Loop { block } => {
				let block_ty = self.infer_block(block);
				self.unify(&TyKind::Primitive(PrimitiveKind::Void), &block_ty);
			}
		}
	}

	fn infer_expr(&mut self, expr: &hir::Expr) -> TyKind<Infer> {
		let ty = match &expr.kind {
			hir::ExprKind::Access(path) => self.resolve_var_ty(path),
			hir::ExprKind::Literal(lit, _ident) => match lit {
				lexer::LiteralKind::Integer => {
					TyKind::Infer(self.tcx.next_infer_tag(), Infer::Integer)
				}
				lexer::LiteralKind::Float => TyKind::Infer(self.tcx.next_infer_tag(), Infer::Float),
				lexer::LiteralKind::Str => TyKind::Primitive(PrimitiveKind::Str),
			},

			hir::ExprKind::Unary(op, expr) => {
				let expr_ty = self.infer_expr(expr);

				match op.bit {
					UnaryOp::Not => {
						self.unify(&TyKind::Primitive(PrimitiveKind::Bool), &expr_ty);
						TyKind::Primitive(PrimitiveKind::Bool)
					}
					UnaryOp::Minus => {
						self.unify(&TyKind::Primitive(PrimitiveKind::UnsignedInt), &expr_ty);
						TyKind::Primitive(PrimitiveKind::UnsignedInt)
					}
				}
			}
			hir::ExprKind::Binary(op, left, right) => {
				let left = self.infer_expr(left);
				let right = self.infer_expr(right);

				// TODO: allow with bools, and other int types
				self.unify(&TyKind::Primitive(PrimitiveKind::UnsignedInt), &left);
				self.unify(&TyKind::Primitive(PrimitiveKind::UnsignedInt), &right);

				#[allow(clippy::enum_glob_use)]
				let expected = {
					use lexer::BinaryOp::*;
					match op.bit {
						Plus | Minus | Mul | Div | Mod | And | Or | Xor | Shl | Shr => {
							TyKind::Primitive(PrimitiveKind::UnsignedInt)
						}
						Gt | Ge | Lt | Le | EqEq | Ne => TyKind::Primitive(PrimitiveKind::Bool),
					}
				};

				expected
			}
			hir::ExprKind::FnCall { expr, args } => {
				let expr_ty = self.infer_expr(expr);

				let TyKind::Fn(func) = expr_ty else {
					let report =
						errors::ty::tried_to_call_non_function(expr.span, args.span, &expr_ty);
					self.tcx.scx.dcx().emit_build(report);
					return TyKind::Error;
				};

				if func.inputs.len() != args.bit.len() {
					let report = errors::ty::function_nb_args_mismatch(
						args.span,
						func.inputs.len(),
						args.bit.len(),
					);
					self.tcx.scx.dcx().emit_build(report);
				}

				for (Param { ty: expected, .. }, actual) in func.inputs.iter().zip(args.bit.iter())
				{
					let actual_ty = self.infer_expr(actual);
					self.unify(&expected.clone().as_infer(), &actual_ty);
				}

				func.output.as_infer()
			}
			hir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => {
				let cond_ty = self.infer_expr(cond);
				self.unify(&TyKind::Primitive(PrimitiveKind::Bool), &cond_ty);

				let conseq_ty = self.infer_block(conseq);
				// if no `else` part, then it must return Unit
				let altern_ty = altern
					.as_ref()
					.map_or(TyKind::Primitive(PrimitiveKind::Void), |altern| {
						self.infer_block(altern)
					});

				self.unify(&conseq_ty, &altern_ty)
			}

			hir::ExprKind::Method(expr, name, args) => todo!(),
			hir::ExprKind::Field(expr, name) => todo!(),
			hir::ExprKind::Deref(expr) => todo!("ensure expr ty is pointer"),

			hir::ExprKind::Assign { target, value } => {
				let target_ty = self.resolve_var_ty(todo!());
				let value_ty = self.infer_expr(value);
				self.unify(&target_ty, &value_ty);
			}

			hir::ExprKind::Return(_) | hir::ExprKind::Break(_) | hir::ExprKind::Continue => {
				TyKind::Primitive(PrimitiveKind::Never)
			}
		};

		// TODO
		assert!(
			self.expr_type
				.insert((expr.span, expr.id), ty.clone())
				.is_none()
		);

		ty
	}
}

/// Unification
#[expect(clippy::match_same_arms)]
impl Inferer<'_> {
	fn unify(&mut self, expected: &TyKind<Infer>, actual: &TyKind<Infer>) -> TyKind<Infer> {
		tracing::trace!(?expected, ?actual, "unify");
		match (expected, actual) {
			(TyKind::Infer(tag, infer), ty) | (ty, TyKind::Infer(tag, infer)) => {
				self.unify_infer(*tag, *infer, ty)
			}
			// infer and never have different meaning but both coerces to anything
			(TyKind::Primitive(PrimitiveKind::Never), ty)
			| (ty, TyKind::Primitive(PrimitiveKind::Never)) => ty.clone(),
			// we try to recover further by inferring errors
			(TyKind::Error, ty) | (ty, TyKind::Error) => ty.clone(),

			(_, _) if expected == actual => expected.clone(),

			(_, _) => {
				let report = errors::ty::unification_mismatch(expected, actual);
				self.tcx.scx.dcx().emit_build(report);
				TyKind::Error
			}
		}
	}

	fn unify_infer(&mut self, tag: InferTag, infer: Infer, other: &TyKind<Infer>) -> TyKind<Infer> {
		tracing::trace!(?tag, ?infer, ?other, "unify_infer");
		let unified = match (infer, other) {
			(
				Infer::Integer,
				ty @ TyKind::Primitive(PrimitiveKind::UnsignedInt | PrimitiveKind::SignedInt),
			) => ty.clone(),
			(Infer::Float, ty @ TyKind::Primitive(PrimitiveKind::Float)) => ty.clone(),
			(Infer::Generic | Infer::Explicit, ty) => ty.clone(),

			(_, TyKind::Infer(tag, actual_infer)) => {
				if infer == *actual_infer {
					TyKind::Infer(*tag, *actual_infer)
				} else {
					let report = errors::ty::infer_unification_mismatch(infer, *actual_infer);
					self.tcx.scx.dcx().emit_build(report);
					TyKind::Error
				}
			}
			(_, ty) => {
				let report = errors::ty::infer_ty_unification_mismatch(infer, ty);
				self.tcx.scx.dcx().emit_build(report);
				TyKind::Error
			}
		};

		self.infer_map.insert(tag, unified.clone());

		unified
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
	pub name: ast::Ident,
	pub ty: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
	pub inputs: Vec<Param>,
	pub output: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind<InferKind = NoInfer> {
	Primitive(PrimitiveKind),
	Pointer(Box<Self>),

	Fn(Box<FnDecl>),
	// TODO
	Adt(()),

	Infer(InferTag, InferKind),
	Error,
}

impl<T: fmt::Display> fmt::Display for TyKind<T> {
	// Should fit in the sentence "found {}"
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Primitive(kind) => write!(f, "primitive {kind}"),
			Self::Pointer(ty) => write!(f, "*{ty}"),
			// TODO: expand args in display
			Self::Fn(_) => write!(f, "a function"),
			Self::Adt(()) => write!(f, "an adt"),
			Self::Infer(_, infer) => infer.fmt(f),
			// TODO
			Self::Error => bug!("error ty kind should never be shown to end-user"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveKind {
	Void,
	Never,

	Bool,
	UnsignedInt,
	SignedInt,
	Float,

	Str,
}

impl fmt::Display for PrimitiveKind {
	// Should fit in the sentence "found primitive {}"
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Void => write!(f, "void"),
			Self::Never => write!(f, "never"),

			Self::Bool => write!(f, "bool"),
			Self::UnsignedInt => write!(f, "uint"),
			Self::SignedInt => write!(f, "sint"),
			Self::Float => write!(f, "float"),

			Self::Str => write!(f, "str"),
		}
	}
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

impl fmt::Display for Infer {
	// Should fit in the sentence "found {}"
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Integer => write!(f, "{{integer}}"),
			Self::Float => write!(f, "{{float}}"),
			Self::Generic | Self::Explicit => write!(f, "_"),
		}
	}
}

impl TyKind<NoInfer> {
	#[must_use]
	pub fn as_infer(self) -> TyKind<Infer> {
		match self {
			Self::Primitive(kind) => TyKind::Primitive(kind),
			Self::Pointer(ty) => TyKind::Pointer(Box::new(ty.as_infer())),
			Self::Fn(fn_) => TyKind::Fn(fn_),
			Self::Adt(()) => TyKind::Adt(()),
			Self::Error => TyKind::Error,
		}
	}
}

impl TyKind<Infer> {
	pub fn as_no_infer(self) -> Result<TyKind<NoInfer>, (InferTag, Infer)> {
		match self {
			Self::Primitive(kind) => Ok(TyKind::Primitive(kind)),
			Self::Pointer(kind) => Ok(TyKind::Pointer(Box::new(kind.as_no_infer()?))),
			Self::Fn(fn_) => Ok(TyKind::Fn(fn_)),
			Self::Adt(()) => Ok(TyKind::Adt(())),
			Self::Infer(tag, infer) => Err((tag, infer)),
			Self::Error => Ok(TyKind::Error),
		}
	}
}

struct TbirBuilder<'body> {
	body: &'body hir::Block,

	expr_tys: HashMap<hir::NodeId, TyKind>,
}

/// TBIR construction
impl TbirBuilder<'_> {
	fn build_body(&self) -> tbir::Block {
		self.build_block(self.body)
	}
	fn build_block(&self, block: &hir::Block) -> tbir::Block {
		let ret = block.ret.as_ref().map(|expr| self.build_expr(expr));
		let ty = ret
			.as_ref()
			.map_or(TyKind::Primitive(PrimitiveKind::Void), |expr| {
				expr.ty.clone()
			});

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

	fn build_stmt(&self, stmt: &hir::Stmt) -> tbir::Stmt {
		let kind = match &stmt.kind {
			hir::StmtKind::Expr(expr) => tbir::StmtKind::Expr(self.build_expr(expr)),
			hir::StmtKind::Let {
				ident,
				ty: _,
				value,
			} => tbir::StmtKind::Let {
				name: *ident,
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

	fn build_expr(&self, expr: &hir::Expr) -> tbir::Expr {
		let kind = match &expr.kind {
			hir::ExprKind::Access(path) => tbir::ExprKind::Access(path.clone()),
			hir::ExprKind::Literal(kind, sym) => tbir::ExprKind::Literal(*kind, *sym),

			hir::ExprKind::Unary(op, expr) => {
				tbir::ExprKind::Unary(*op, Box::new(self.build_expr(expr)))
			}
			hir::ExprKind::Binary(op, left, right) => tbir::ExprKind::Binary(
				*op,
				Box::new(self.build_expr(left)),
				Box::new(self.build_expr(right)),
			),
			hir::ExprKind::FnCall { expr, args } => {
				let nargs = args.bit.iter().map(|arg| self.build_expr(arg)).collect();
				tbir::ExprKind::FnCall {
					expr: Box::new(self.build_expr(expr)),
					args: args.with_bit(nargs),
				}
			}
			hir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => tbir::ExprKind::If {
				cond: Box::new(self.build_expr(cond)),
				conseq: Box::new(self.build_block(conseq)),
				altern: altern
					.as_ref()
					.map(|altern| Box::new(self.build_block(altern))),
			},

			hir::ExprKind::Method(expr, name, args) => todo!(),
			hir::ExprKind::Field(expr, name) => todo!(),
			hir::ExprKind::Deref(expr) => todo!(),

			hir::ExprKind::Assign { target, value } => todo!(),

			hir::ExprKind::Return(expr) => {
				tbir::ExprKind::Return(expr.as_ref().map(|expr| Box::new(self.build_expr(expr))))
			}
			hir::ExprKind::Break(expr) => {
				tbir::ExprKind::Break(expr.as_ref().map(|expr| Box::new(self.build_expr(expr))))
			}

			hir::ExprKind::Continue => tbir::ExprKind::Continue,
		};

		let ty = match self.expr_tys.get(&expr.id) {
			Some(ty) => ty.clone(),
			None => bug!("all expression should have a type by now"),
		};

		tbir::Expr {
			kind,
			ty,
			span: expr.span,
			id: expr.id,
		}
	}
}
