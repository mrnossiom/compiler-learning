use std::{collections::HashMap, sync::Arc};

use cranelift::prelude::{isa::TargetIsa, *};
use cranelift_control::ControlPlane;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectModule, ObjectProduct};

use crate::{
	Result, ast,
	codegen::{CodeGenBackend, JitBackend, ObjectBackend},
	hir, lexer,
	resolve::Environment,
	session::{PrintKind, SessionCtx, Symbol},
	tbir,
	ty::{self, TyCtx},
};

pub enum MaybeValue {
	Value(Value),
	/// Zero-sized value
	Zst,
	Never,
}

impl MaybeValue {
	fn with_slice(&self, func: impl FnOnce(&[Value])) {
		match self {
			Self::Value(val) => func(&[*val]),
			Self::Zst => func(&[]),
			Self::Never => {}
		}
	}
}

pub struct Generator<'tcx, M: Module> {
	scx: &'tcx SessionCtx,
	tcx: &'tcx TyCtx<'tcx>,

	module: M,
	isa: Arc<dyn TargetIsa + 'static>,
	builder_context: FunctionBuilderContext,

	functions: HashMap<Symbol, FuncId>,
}

impl<'tcx, M: Module> Generator<'tcx, M> {
	pub fn new(tcx: &'tcx TyCtx, isa: Arc<dyn TargetIsa + 'static>, module: M) -> Self {
		Self {
			scx: tcx.scx,
			tcx,
			module,
			isa,
			builder_context: FunctionBuilderContext::new(),
			functions: HashMap::new(),
		}
	}

	// Return `None` on non-concrete types (e.g. zst, never)
	fn to_cl_type(&self, output: &ty::TyKind) -> Option<Type> {
		match &output {
			ty::TyKind::Unit | ty::TyKind::Never => None,
			ty::TyKind::Bool => Some(types::I8),
			ty::TyKind::Integer => Some(types::I32),
			ty::TyKind::Float => Some(types::F32),
			ty::TyKind::Str => todo!(),
			ty::TyKind::Fn(_fn_decl) => Some(self.isa.pointer_type()),
			ty::TyKind::Infer(_, _) => unreachable!(),
		}
	}
}

impl<'tcx> Generator<'tcx, JITModule> {
	pub fn new_jit(tcx: &'tcx TyCtx) -> Self {
		use ::cranelift::prelude::{Configurable, settings};
		use cranelift_jit::{JITBuilder, JITModule};
		use cranelift_module::default_libcall_names;

		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();
		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();

		let builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
		let module = JITModule::new(builder);

		Self::new(tcx, isa, module)
	}
}

impl<'tcx> Generator<'tcx, ObjectModule> {
	pub fn new_object(tcx: &'tcx TyCtx) -> Self {
		use ::cranelift::prelude::{Configurable, settings};
		use cranelift_module::default_libcall_names;
		use cranelift_object::{ObjectBuilder, ObjectModule};

		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();

		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();

		let builder = ObjectBuilder::new(isa.clone(), "out", default_libcall_names()).unwrap();

		// builder.per_function_section(per_function_section) what is this?

		let module = ObjectModule::new(builder);

		Self::new(tcx, isa, module)
	}
}

impl<M: Module> Generator<'_, M> {
	pub fn lower_signature(&mut self, decl: &ty::FnDecl) -> Signature {
		let mut signature = self.module.make_signature();

		for ty::Param { ident: _, ty } in &decl.inputs {
			let Some(type_) = self.to_cl_type(ty) else {
				continue;
			};
			signature.params.push(AbiParam::new(type_));
		}
		if let Some(type_) = self.to_cl_type(&decl.output) {
			signature.returns.push(AbiParam::new(type_));
		}

		signature
	}

	pub fn declare_func(
		&mut self,
		name: Symbol,
		decl: &ty::FnDecl,
		linkage: Linkage,
	) -> Result<FuncId> {
		if self.functions.contains_key(&name) {
			return Err("already defined");
		}

		let signature = self.lower_signature(decl);

		let func_name = self.scx.symbols.resolve(name);
		let func_id = self
			.module
			.declare_function(&func_name, linkage, &signature)
			.unwrap();

		self.functions.insert(name, func_id);
		Ok(func_id)
	}

	#[tracing::instrument(level = "debug", skip(self, decl))]
	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()> {
		let _func_id = self.declare_func(name, decl, Linkage::Import)?;
		Ok(())
	}

	#[tracing::instrument(level = "debug", skip(self, decl))]
	fn declare_function(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<FuncId> {
		let func_id = self.declare_func(name, decl, Linkage::Export)?;
		Ok(func_id)
	}

	fn define_function(
		&mut self,
		func_id: FuncId,
		decl: &ty::FnDecl,
		body: &tbir::Block,
	) -> Result<()> {
		let mut context = self.module.make_context();

		// TODO: this computes the signature a second time after declaration
		context.func.signature = self.lower_signature(decl);

		let params = decl
			.inputs
			.iter()
			// skip zst
			.filter_map(|param| self.to_cl_type(&param.ty).map(|ty| (param.ident, ty)))
			.collect::<Vec<_>>();

		let mut builder = FunctionBuilder::new(&mut context.func, &mut self.builder_context);

		let entry_block = builder.create_block();
		builder.append_block_params_for_function_params(entry_block);
		builder.switch_to_block(entry_block);
		builder.seal_block(entry_block);

		let mut values = HashMap::new();
		for (i, (ident, ty)) in params.into_iter().enumerate() {
			let value = builder.block_params(entry_block)[i];

			let variable = builder.declare_var(ty);
			builder.def_var(variable, value);

			values.insert(ident.name, Some(variable));
		}

		let mut generator = FunctionGenerator {
			scx: self.scx,

			builder,
			functions: &self.functions,
			module: &mut self.module,
			isa: self.isa.clone(),
			values,

			loops: Vec::default(),
		};

		let return_value = generator.codegen_block(body)?;
		return_value.with_slice(|vals| {
			generator.builder.ins().return_(vals);
		});

		generator.builder.finalize();

		context
			.optimize(self.module.isa(), &mut ControlPlane::default())
			.unwrap();

		if self.scx.options.print.contains(&PrintKind::BackendIr) {
			print!("{}", context.func.display());
		}

		self.module.define_function(func_id, &mut context).unwrap();

		self.module.clear_context(&mut context);

		Ok(())
	}
}

impl<M: Module> CodeGenBackend for Generator<'_, M> {
	fn codegen_root(&mut self, hir: &hir::Root, env: &Environment) {
		let mut id_map = HashMap::new();

		for item in hir.items {
			match item.kind {
				hir::ItemKind::Extern { ident, decl } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					self.declare_extern(ident.name, &decl).unwrap();
				}
				hir::ItemKind::Function { ident, decl, .. } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					let func_id = self.declare_function(ident.name, &decl).unwrap();

					id_map.insert(ident.name, func_id);
				}
			}
		}
		for item in hir.items {
			match item.kind {
				hir::ItemKind::Extern { .. } => {}
				hir::ItemKind::Function { ident, decl, body } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);

					let body = self.tcx.typeck_fn(ident, &decl, body, env);
					if self.scx.options.print.contains(&PrintKind::TypedBodyIr) {
						println!("{body:#?}");
					}
					let func_id = id_map.get(&ident.name).unwrap();
					self.define_function(*func_id, &decl, &body).unwrap();
				}
			}
		}
	}
}

impl JitBackend for Generator<'_, JITModule> {
	fn call_main(&mut self) {
		self.module.finalize_definitions().unwrap();

		let main = self.scx.symbols.intern("main");
		let main_id = self.functions.get(&main).unwrap();
		let func = self.module.get_finalized_function(*main_id);
		// TODO: this is unsafe as some functions ask for arguments, and lot a more reasons
		#[allow(unsafe_code)]
		let main = unsafe { std::mem::transmute::<*const u8, fn()>(func) };

		main();
	}
}

impl ObjectBackend for Generator<'_, ObjectModule> {
	fn get_object(self) -> ObjectProduct {
		self.module.finish()
	}
}

pub struct FunctionGenerator<'scx, 'bld> {
	scx: &'scx SessionCtx,

	builder: FunctionBuilder<'bld>,
	functions: &'bld HashMap<Symbol, FuncId>,
	module: &'bld mut dyn Module,
	isa: Arc<dyn TargetIsa + 'static>,
	values: HashMap<Symbol, Option<Variable>>,

	loops: Vec<(Block, Block)>,
}

/// Codegen tbir structs
impl FunctionGenerator<'_, '_> {
	// TODO: remove duplicate
	fn to_cl_type(&self, output: &ty::TyKind) -> Option<Type> {
		match &output {
			ty::TyKind::Unit | ty::TyKind::Never => None,
			ty::TyKind::Bool => Some(types::I8),
			ty::TyKind::Integer => Some(types::I32),
			ty::TyKind::Float => Some(types::F32),
			ty::TyKind::Str => todo!(),
			ty::TyKind::Fn(_fn_decl) => Some(self.isa.pointer_type()),
			ty::TyKind::Infer(_, _) => unreachable!(),
		}
	}

	fn codegen_block(&mut self, block: &tbir::Block) -> Result<MaybeValue> {
		tracing::trace!(id = ?block.id, "codegen_block");

		for stmt in &block.stmts {
			let should_stop_block_codegen = self.codegen_stmt(stmt)?;
			if should_stop_block_codegen {
				return Ok(MaybeValue::Never);
			}
		}

		if let Some(expr) = &block.ret {
			self.codegen_expr(expr)
		} else {
			Ok(MaybeValue::Zst)
		}
	}

	fn codegen_stmt(&mut self, stmt: &tbir::Stmt) -> Result<bool /* should_stop_block_codegen */> {
		tracing::trace!(id = ?stmt.id, "codegen_stmt");
		match &stmt.kind {
			tbir::StmtKind::Expr(expr) => match self.codegen_expr(expr)? {
				MaybeValue::Value(_) | MaybeValue::Zst => {}
				MaybeValue::Never => return Ok(true),
			},
			tbir::StmtKind::Let { ident, value, .. } => match self.codegen_expr(value)? {
				MaybeValue::Value(expr_value) => {
					let ty = self.to_cl_type(&value.ty).unwrap();
					let variable = self.builder.declare_var(ty);
					self.builder.def_var(variable, expr_value);

					self.values.insert(ident.name, Some(variable));
				}
				MaybeValue::Zst => {
					self.values.insert(ident.name, None);
				}
				MaybeValue::Never => {}
			},
			tbir::StmtKind::Assign { target, value } => {
				let Some(variable) = *self.values.get(&target.name).unwrap() else {
					// handle zst
					return Ok(false);
				};

				match self.codegen_expr(value)? {
					MaybeValue::Value(expr_value) => {
						self.builder.def_var(variable, expr_value);
					}
					MaybeValue::Zst | MaybeValue::Never => {}
				}
			}
			tbir::StmtKind::Loop { block } => {
				let loop_ = self.builder.create_block();
				let cont = self.builder.create_block();

				self.loops.push((loop_, cont));

				self.builder.ins().jump(loop_, &[]);

				self.builder.switch_to_block(loop_);

				self.codegen_block(block)?;
				self.builder.ins().jump(loop_, &[]);

				self.builder.seal_block(loop_);

				self.builder.switch_to_block(cont);
				self.builder.seal_block(cont);

				self.loops.pop();
			}
		}
		Ok(false)
	}

	fn codegen_expr(&mut self, expr: &tbir::Expr) -> Result<MaybeValue> {
		tracing::trace!(id = ?expr.id, "codegen_expr");
		let value = match &expr.kind {
			tbir::ExprKind::Literal(lit, sym) => {
				let sym = self.scx.symbols.resolve(*sym);
				let value = match lit {
					lexer::LiteralKind::Integer => {
						let int_ty = self.to_cl_type(&expr.ty).unwrap();
						self.builder
							.ins()
							.iconst(int_ty, sym.parse::<i64>().unwrap())
					}
					lexer::LiteralKind::Float => {
						self.builder.ins().f64const(sym.parse::<f64>().unwrap())
					}
					lexer::LiteralKind::Str => todo!(),
				};
				MaybeValue::Value(value)
			}
			tbir::ExprKind::Variable(ident) => match self.values.get(&ident.name) {
				Some(Some(var)) => MaybeValue::Value(self.builder.use_var(*var)),
				Some(None) => MaybeValue::Zst,
				None => return Err("var undefined"),
			},
			tbir::ExprKind::Binary(op, left, right) => {
				MaybeValue::Value(self.codegen_bin_op(*op, left, right)?)
			}
			tbir::ExprKind::FnCall { expr, args } => {
				// TODO: allow indirect calls
				let tbir::ExprKind::Variable(ident) = expr.kind else {
					todo!("not a fn")
				};
				let Some(func_id) = self.functions.get(&ident.name) else {
					return Err("invalid fn call");
				};

				let local_func = self
					.module
					.declare_func_in_func(*func_id, self.builder.func);

				let mut argsz = Vec::new();
				for arg in args {
					match self.codegen_expr(arg)? {
						MaybeValue::Value(expr_value) => {
							argsz.push(expr_value);
						}
						MaybeValue::Zst | MaybeValue::Never => {}
					}
				}

				let call = self.builder.ins().call(local_func, &argsz);

				let inst_results = self.builder.inst_results(call);
				match inst_results.len() {
					0 => MaybeValue::Zst,
					1 => MaybeValue::Value(inst_results[0]),
					_ => panic!(),
				}
			}
			tbir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => self.codegen_if(cond, conseq, altern.as_deref())?,
			tbir::ExprKind::Return(expr) => {
				if let Some(expr) = expr {
					match self.codegen_expr(expr)? {
						MaybeValue::Value(expr_value) => {
							self.builder.ins().return_(&[expr_value]);
						}
						MaybeValue::Zst => {
							self.builder.ins().return_(&[]);
						}
						MaybeValue::Never => {}
					}
				} else {
					self.builder.ins().return_(&[]);
				}

				MaybeValue::Never
			}
			tbir::ExprKind::Break(expr) => {
				let (_, cont) = *self.loops.last().unwrap();

				if let Some(expr) = expr {
					match self.codegen_expr(expr)? {
						MaybeValue::Value(expr_value) => {
							self.builder.ins().jump(cont, &[expr_value.into()]);
						}
						MaybeValue::Zst => {
							self.builder.ins().jump(cont, &[]);
						}
						MaybeValue::Never => {}
					}
				} else {
					self.builder.ins().jump(cont, &[]);
				}

				MaybeValue::Never
			}
			tbir::ExprKind::Continue => {
				let (loop_, _) = *self.loops.last().unwrap();

				self.builder.ins().jump(loop_, &[]);

				MaybeValue::Never
			}
		};
		Ok(value)
	}
}

/// Codegen bits
impl FunctionGenerator<'_, '_> {
	fn codegen_bin_op(
		&mut self,
		op: ast::Spanned<lexer::BinOp>,
		left: &tbir::Expr,
		right: &tbir::Expr,
	) -> Result<Value> {
		tracing::trace!("codegen_bin_op");
		// cannot be zst
		let lhs = self.codegen_expr(left)?;
		let rhs = self.codegen_expr(right)?;

		let (lhs, rhs) = match (lhs, rhs) {
			(MaybeValue::Value(lhs), MaybeValue::Value(rhs)) => (lhs, rhs),
			_ => panic!(),
		};

		let value = match op.bit {
			lexer::BinOp::Plus => self.builder.ins().iadd(lhs, rhs),
			lexer::BinOp::Minus => self.builder.ins().isub(lhs, rhs),
			lexer::BinOp::Mul => self.builder.ins().imul(lhs, rhs),
			lexer::BinOp::Div => self.builder.ins().udiv(lhs, rhs),
			lexer::BinOp::Mod => self.builder.ins().urem(lhs, rhs),

			lexer::BinOp::Gt => self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
			lexer::BinOp::Ge => self
				.builder
				.ins()
				.icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
			lexer::BinOp::Lt => self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
			lexer::BinOp::Le => self
				.builder
				.ins()
				.icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
			lexer::BinOp::EqEq => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
			lexer::BinOp::Ne => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
		};

		Ok(value)
	}

	fn codegen_if(
		&mut self,
		cond: &tbir::Expr,
		conseq: &tbir::Block,
		altern: Option<&tbir::Block>,
	) -> Result<MaybeValue> {
		let then_block = self.builder.create_block();
		let else_block = altern.as_ref().map(|_| self.builder.create_block());
		let cont_block = self.builder.create_block();
		tracing::trace!(?then_block, ?else_block, ?cont_block, "codegen_if");

		let condition = match self.codegen_expr(cond)? {
			MaybeValue::Value(val) => val,
			MaybeValue::Zst | MaybeValue::Never => panic!(),
		};

		if let Some(ty) = self.to_cl_type(&conseq.ty) {
			self.builder.append_block_param(cont_block, ty);
		}

		self.builder.ins().brif(
			condition,
			then_block,
			&[],
			else_block.unwrap_or(cont_block),
			&[],
		);
		self.builder.switch_to_block(then_block);
		self.builder.seal_block(then_block);
		match self.codegen_block(conseq)? {
			MaybeValue::Value(then_ret) => {
				self.builder.ins().jump(cont_block, &[then_ret.into()]);
			}
			MaybeValue::Zst => {
				self.builder.ins().jump(cont_block, &[]);
			}
			MaybeValue::Never => {}
		}
		if let Some(altern) = altern {
			// TODO
			let else_block = else_block.unwrap();

			self.builder.switch_to_block(else_block);
			self.builder.seal_block(else_block);

			match self.codegen_block(altern)? {
				MaybeValue::Value(else_ret) => {
					self.builder.ins().jump(cont_block, &[else_ret.into()]);
				}
				MaybeValue::Zst => {
					self.builder.ins().jump(cont_block, &[]);
				}
				MaybeValue::Never => {}
			}
		}
		self.builder.switch_to_block(cont_block);
		self.builder.seal_block(cont_block);
		let block_params = self.builder.block_params(cont_block);
		Ok(match block_params.len() {
			0 => MaybeValue::Zst,
			1 => MaybeValue::Value(block_params[0]),
			_ => panic!(),
		})
	}
}
