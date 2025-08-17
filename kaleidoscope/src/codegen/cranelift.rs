use std::{collections::HashMap, mem};

use cranelift::prelude::*;
use cranelift_control::ControlPlane;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

pub enum KValue {
	Value(Value),
	Zst,
	Never,
}

impl KValue {
	// surely the worst name ever given to a function
	fn value_as_slice<T>(self, func: impl FnOnce(&[Value]) -> T) {
		match self {
			Self::Value(val) => _ = func(&[val]),
			Self::Zst => _ = func(&[]),
			Self::Never => {}
		}
	}
}

use crate::{
	Result, ast,
	codegen::CodeGen,
	lexer,
	session::{SessionCtx, Symbol},
	tbir, ty,
};

pub struct Generator<'scx> {
	scx: &'scx SessionCtx,

	builder_context: FunctionBuilderContext,
	functions: HashMap<Symbol, FuncId>,
	module: JITModule,
}

impl<'scx> Generator<'scx> {
	pub fn new(scx: &'scx SessionCtx) -> Self {
		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();

		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();

		let mut builder = JITBuilder::with_isa(isa, default_libcall_names());

		builder.symbol("putchard", crate::ffi::putchard as *const u8);

		Self {
			scx,
			builder_context: FunctionBuilderContext::new(),
			functions: HashMap::new(),
			module: JITModule::new(builder),
		}
	}
}

// Return `None` on non-concrete types (e.g. zst, never)
fn to_cl_type(output: &ty::TyKind) -> Option<Type> {
	match &output {
		ty::TyKind::Unit | ty::TyKind::Never => None,
		ty::TyKind::Bool => Some(types::I8),
		ty::TyKind::Integer => Some(types::I32),
		ty::TyKind::Float => Some(types::F32),
		ty::TyKind::Str => todo!(),
		ty::TyKind::Fn(_fn_decl) => todo!(),
		ty::TyKind::Infer(_, _) => unreachable!(),
	}
}

impl Generator<'_> {
	pub fn declare_func(
		&mut self,
		name: Symbol,
		decl: &ty::FnDecl,
		linkage: Linkage,
	) -> Result<(FuncId, Signature)> {
		match self.functions.get(&name) {
			None => {
				let mut signature = self.module.make_signature();
				for ty::Param { ident: _, ty } in &decl.inputs {
					let Some(type_) = to_cl_type(ty) else {
						continue;
					};
					signature.params.push(AbiParam::new(type_));
				}
				if let Some(type_) = to_cl_type(&decl.output) {
					signature.returns.push(AbiParam::new(type_));
				}

				let func_name = self.scx.symbols.resolve(name);
				let func_id = self
					.module
					.declare_function(&func_name, linkage, &signature)
					.unwrap();

				self.functions.insert(name, func_id);
				Ok((func_id, signature))
			}
			Some(_) => Err("already defined"),
		}
	}
}

impl CodeGen for Generator<'_> {
	// HACK: compute signature elsewhere
	type FuncId = (FuncId, Signature);

	#[tracing::instrument(level = "debug", skip(self, decl))]
	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()> {
		let (func_id, signature) = self.declare_func(name, decl, Linkage::Import)?;
		Ok(())
	}

	#[tracing::instrument(level = "debug", skip(self, decl))]
	fn declare_function(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<Self::FuncId> {
		let (func_id, signature) = self.declare_func(name, decl, Linkage::Export)?;
		Ok((func_id, signature))
	}

	fn define_function(
		&mut self,
		func_id: Self::FuncId,
		decl: &ty::FnDecl,
		body: &tbir::Block,
		print_bir: bool,
	) -> Result<()> {
		let mut context = self.module.make_context();

		context.func.signature = func_id.1;

		let mut builder = FunctionBuilder::new(&mut context.func, &mut self.builder_context);

		let entry_block = builder.create_block();
		builder.append_block_params_for_function_params(entry_block);
		builder.switch_to_block(entry_block);
		builder.seal_block(entry_block);

		let mut values = HashMap::new();
		for (i, (ident, ty)) in decl
			.inputs
			.iter()
			// skip zst
			.filter_map(|param| to_cl_type(&param.ty).map(|ty| (param.ident, ty)))
			.enumerate()
		{
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
			values,

			loops: Vec::default(),
		};

		let return_value = generator.codegen_block(body)?;
		return_value.value_as_slice(|vals| generator.builder.ins().return_(vals));

		generator.builder.finalize();

		context
			.optimize(self.module.isa(), &mut ControlPlane::default())
			.unwrap();

		if print_bir {
			print!("{}", context.func.display());
		}

		self.module
			.define_function(func_id.0, &mut context)
			.unwrap();

		self.module.clear_context(&mut context);

		Ok(())
	}

	#[allow(unsafe_code)]
	unsafe fn call_fn_as_main(&mut self, func_id: Self::FuncId) -> Result<i64> {
		self.module.finalize_definitions().unwrap();

		let fn_ = self.module.get_finalized_function(func_id.0);

		// TODO: this is unsafe as some functions ask for arguments, and lot a more reasons
		let main = unsafe { mem::transmute::<*const u8, fn() -> i64>(fn_) };

		Ok(main())
	}
}

pub struct FunctionGenerator<'scx, 'bld> {
	scx: &'scx SessionCtx,

	builder: FunctionBuilder<'bld>,
	functions: &'bld HashMap<Symbol, FuncId>,
	module: &'bld mut JITModule,
	values: HashMap<Symbol, Option<Variable>>,

	loops: Vec<(Block, Block)>,
}

/// Codegen tbir structs
impl FunctionGenerator<'_, '_> {
	fn codegen_block(&mut self, block: &tbir::Block) -> Result<KValue> {
		tracing::trace!(id = ?block.id, "codegen_block");

		for stmt in &block.stmts {
			let should_stop_block_codegen = self.codegen_stmt(stmt)?;
			if should_stop_block_codegen {
				return Ok(KValue::Never);
			}
		}

		if let Some(expr) = &block.ret {
			self.codegen_expr(expr)
		} else {
			Ok(KValue::Zst)
		}
	}

	fn codegen_stmt(&mut self, stmt: &tbir::Stmt) -> Result<bool /* should_stop_block_codegen */> {
		tracing::trace!(id = ?stmt.id, "codegen_stmt");
		match &stmt.kind {
			tbir::StmtKind::Expr(expr) => match self.codegen_expr(expr)? {
				KValue::Value(_) | KValue::Zst => {}
				KValue::Never => return Ok(true),
			},
			tbir::StmtKind::Let { ident, value, .. } => match self.codegen_expr(value)? {
				KValue::Value(expr_value) => {
					let ty = to_cl_type(&value.ty).unwrap();
					let variable = self.builder.declare_var(ty);
					self.builder.def_var(variable, expr_value);

					self.values.insert(ident.name, Some(variable));
				}
				KValue::Zst => {
					self.values.insert(ident.name, None);
				}
				KValue::Never => {}
			},
			tbir::StmtKind::Assign { target, value } => {
				let Some(variable) = *self.values.get(&target.name).unwrap() else {
					// handle zst
					return Ok(false);
				};

				match self.codegen_expr(value)? {
					KValue::Value(expr_value) => {
						self.builder.def_var(variable, expr_value);
					}
					KValue::Zst | KValue::Never => {}
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

	fn codegen_expr(&mut self, expr: &tbir::Expr) -> Result<KValue> {
		tracing::trace!(id = ?expr.id, "codegen_expr");
		let value = match &expr.kind {
			tbir::ExprKind::Literal(lit, sym) => {
				let sym = self.scx.symbols.resolve(*sym);
				let value = match lit {
					lexer::LiteralKind::Integer => self
						.builder
						.ins()
						.iconst(to_cl_type(&expr.ty).unwrap(), sym.parse::<i64>().unwrap()),
					lexer::LiteralKind::Float => {
						self.builder.ins().f64const(sym.parse::<f64>().unwrap())
					}
					lexer::LiteralKind::Str => todo!(),
				};
				KValue::Value(value)
			}
			tbir::ExprKind::Variable(ident) => match self.values.get(&ident.name) {
				Some(Some(var)) => KValue::Value(self.builder.use_var(*var)),
				Some(None) => KValue::Zst,
				None => return Err("var undefined"),
			},
			tbir::ExprKind::Binary(op, left, right) => {
				KValue::Value(self.codegen_binop(*op, left, right)?)
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
						KValue::Value(expr_value) => {
							argsz.push(expr_value);
						}
						KValue::Zst | KValue::Never => {}
					}
				}

				let call = self.builder.ins().call(local_func, &argsz);

				let inst_results = self.builder.inst_results(call);
				match inst_results.len() {
					0 => KValue::Zst,
					1 => KValue::Value(inst_results[0]),
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
						KValue::Value(expr_value) => {
							self.builder.ins().return_(&[expr_value]);
						}
						KValue::Zst => {
							self.builder.ins().return_(&[]);
						}
						KValue::Never => {}
					}
				} else {
					self.builder.ins().return_(&[]);
				}

				KValue::Never
			}
			tbir::ExprKind::Break(expr) => {
				let (_, cont) = *self.loops.last().unwrap();

				if let Some(expr) = expr {
					match self.codegen_expr(expr)? {
						KValue::Value(expr_value) => {
							self.builder.ins().jump(cont, &[expr_value.into()]);
						}
						KValue::Zst => {
							self.builder.ins().jump(cont, &[]);
						}
						KValue::Never => {}
					}
				} else {
					self.builder.ins().jump(cont, &[]);
				}

				KValue::Never
			}
			tbir::ExprKind::Continue => {
				let (loop_, _) = *self.loops.last().unwrap();

				self.builder.ins().jump(loop_, &[]);

				KValue::Never
			}
		};
		Ok(value)
	}
}

/// Codegen bits
impl FunctionGenerator<'_, '_> {
	fn codegen_binop(
		&mut self,
		op: ast::Spanned<lexer::BinOp>,
		left: &tbir::Expr,
		right: &tbir::Expr,
	) -> Result<Value> {
		tracing::trace!("codegen_binop");
		// cannot be zst
		let lhs = self.codegen_expr(left)?;
		let rhs = self.codegen_expr(right)?;

		let (lhs, rhs) = match (lhs, rhs) {
			(KValue::Value(lhs), KValue::Value(rhs)) => (lhs, rhs),
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
	) -> Result<KValue> {
		let then_block = self.builder.create_block();
		let else_block = altern.as_ref().map(|_| self.builder.create_block());
		let cont_block = self.builder.create_block();
		tracing::trace!(?then_block, ?else_block, ?cont_block, "codegen_if");

		let condition = match self.codegen_expr(cond)? {
			KValue::Value(val) => val,
			KValue::Zst | KValue::Never => panic!(),
		};

		if let Some(ty) = to_cl_type(&conseq.ty) {
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
			KValue::Value(then_ret) => {
				self.builder.ins().jump(cont_block, &[then_ret.into()]);
			}
			KValue::Zst => {
				self.builder.ins().jump(cont_block, &[]);
			}
			KValue::Never => {}
		}
		if let Some(altern) = altern {
			// TODO
			let else_block = else_block.unwrap();

			self.builder.switch_to_block(else_block);
			self.builder.seal_block(else_block);

			match self.codegen_block(altern)? {
				KValue::Value(else_ret) => {
					self.builder.ins().jump(cont_block, &[else_ret.into()]);
				}
				KValue::Zst => {
					self.builder.ins().jump(cont_block, &[]);
				}
				KValue::Never => {}
			}
		}
		self.builder.switch_to_block(cont_block);
		self.builder.seal_block(cont_block);
		let block_params = self.builder.block_params(cont_block);
		Ok(match block_params.len() {
			0 => KValue::Zst,
			1 => KValue::Value(block_params[0]),
			_ => panic!(),
		})
	}
}
