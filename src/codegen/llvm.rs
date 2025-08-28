use std::{collections::HashMap, iter};

use inkwell::{
	AddressSpace, IntPredicate, OptimizationLevel,
	builder::Builder,
	context::Context,
	execution_engine::ExecutionEngine,
	module::Module,
	passes::PassBuilderOptions,
	targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
	values::{BasicValue, BasicValueEnum, FunctionValue},
};

use crate::{
	Result, ast,
	codegen::{CodeGenBackend, JitBackend, ObjectBackend},
	hir, lexer,
	resolve::Environment,
	session::{PrintKind, Symbol},
	tbir,
	ty::{self, TyCtx},
};

#[derive(Debug, Clone)]
enum MaybeValue<'ctx> {
	Value(BasicValueEnum<'ctx>),
	Zst,
	Never,
}

impl<'ctx> MaybeValue<'ctx> {
	const fn is_never(&self) -> bool {
		matches!(self, Self::Never)
	}

	fn as_value(&self) -> Option<Option<&dyn BasicValue<'ctx>>> {
		match self {
			Self::Value(val) => Some(Some(val)),
			Self::Zst => Some(None),
			Self::Never => None,
		}
	}

	fn as_value_enum(&self) -> Option<Option<BasicValueEnum<'ctx>>> {
		match self {
			Self::Value(val) => Some(Some(*val)),
			Self::Zst => Some(None),
			Self::Never => None,
		}
	}
}

pub struct Generator<'tcx, 'ctx> {
	tcx: &'tcx TyCtx<'tcx>,

	ctx: &'ctx Context,
	builder: Builder<'ctx>,
	module: Module<'ctx>,
	jit: ExecutionEngine<'ctx>,

	variables: HashMap<Symbol, MaybeValue<'ctx>>,
}

impl<'tcx> Generator<'tcx, '_> {
	pub fn new_jit(tcx: &'tcx TyCtx) -> Self {
		// TODO
		let context = Box::leak(Box::new(inkwell::context::Context::create()));

		Self::new(tcx, context)
	}
}

impl<'tcx, 'ctx> Generator<'tcx, 'ctx> {
	pub fn new(tcx: &'tcx TyCtx, ctx: &'ctx Context) -> Self {
		let module = ctx.create_module("repl");
		let jit = module
			.create_jit_execution_engine(OptimizationLevel::None)
			.unwrap();
		Self {
			tcx,

			ctx,
			builder: ctx.create_builder(),
			module,
			jit,

			variables: HashMap::new(),
		}
	}

	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()> {
		self.define_func(name, decl)?;
		Ok(())
	}

	fn declare_function(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<FunctionValue<'ctx>> {
		if self
			.module
			.get_function(&self.tcx.scx.symbols.resolve(name))
			.is_some()
		{
			return Err("cannot redefine extern function");
		}

		let func_val = self.define_func(name, decl)?;
		Ok(func_val)
	}

	fn define_function(
		&mut self,
		func_val: FunctionValue<'ctx>,
		decl: &ty::FnDecl,
		body: &tbir::Block,
	) -> Result<()> {
		let bb = self.ctx.append_basic_block(func_val, "entry");
		self.builder.position_at_end(bb);

		self.variables.clear();
		func_val
			.get_param_iter()
			.zip(&decl.inputs)
			.for_each(|(arg, ty::Param { ident, .. })| {
				let val = arg
					.into_int_value()
					.const_to_pointer(self.ctx.ptr_type(AddressSpace::default()));
				self.variables
					.insert(ident.name, MaybeValue::Value(val.as_basic_value_enum()));
			});

		let ret_val = self.codegen_block(body)?;
		if let Some(ret_val) = ret_val.as_value() {
			self.builder.build_return(ret_val).unwrap();
		}

		if !func_val.verify(true) {
			#[allow(unsafe_code)]
			unsafe {
				func_val.delete();
			}
			return Err("function is invalid");
		}

		if self.tcx.scx.options.print.contains(&PrintKind::BackendIr) {
			func_val.print_to_stderr();
		}

		Ok(())
	}
}

impl<'ctx> CodeGenBackend for Generator<'_, 'ctx> {
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
					if self.tcx.scx.options.print.contains(&PrintKind::TypedBodyIr) {
						println!("{body:#?}");
					}
					let func_id = id_map.get(&ident.name).unwrap();
					self.define_function(*func_id, &decl, &body).unwrap();
				}
			}
		}
	}
}

impl JitBackend for Generator<'_, '_> {
	fn call_main(&mut self) {
		todo!()
		// #[allow(unsafe_code)]
		// let ret = unsafe {
		// 	self.jit
		// 		.get_function::<unsafe extern "C" fn() -> i64>(func.get_name().to_str().unwrap())
		// }
		// .unwrap();
		// #[allow(unsafe_code)]
		// Ok(unsafe { ret.call() })
	}
}

impl ObjectBackend for Generator<'_, '_> {
	fn get_object(self) -> cranelift_object::ObjectProduct {
		todo!()
	}
}

impl<'ctx> Generator<'_, 'ctx> {
	pub fn apply_passes(&self) {
		Target::initialize_all(&InitializationConfig::default());

		let target_triple = TargetMachine::get_default_triple();
		let target = Target::from_triple(&target_triple).unwrap();
		let target_machine = target
			.create_target_machine(
				&target_triple,
				"generic",
				"",
				OptimizationLevel::None,
				RelocMode::PIC,
				CodeModel::Default,
			)
			.unwrap();

		let passes: &[&str] = &[
			"instcombine",
			"reassociate",
			"gvn",
			"simplifycfg",
			// "basic-aa",
			"mem2reg",
		];

		self.module
			.run_passes(
				&passes.join(","),
				&target_machine,
				PassBuilderOptions::create(),
			)
			.unwrap();
	}

	pub fn define_func(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<FunctionValue<'ctx>> {
		let args_ty = iter::repeat_n(self.ctx.i64_type(), decl.inputs.len())
			.map(Into::into)
			.collect::<Vec<_>>();
		let fn_ty = self.ctx.i64_type().fn_type(&args_ty, false);

		let name = self.tcx.scx.symbols.resolve(name);
		let fn_val = self.module.add_function(&name, fn_ty, None);

		// set arguments name
		fn_val
			.get_param_iter()
			.zip(&decl.inputs)
			.for_each(|(arg, ty::Param { ident, ty })| {
				arg.into_int_value()
					.set_name(&self.tcx.scx.symbols.resolve(ident.name));
			});

		Ok(fn_val)
	}
}

impl<'ctx> Generator<'_, 'ctx> {
	fn codegen_block(&mut self, block: &tbir::Block) -> Result<MaybeValue<'ctx>> {
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
		match &stmt.kind {
			tbir::StmtKind::Expr(expr) => Ok(self.codegen_expr(expr)?.is_never()),
			tbir::StmtKind::Let { ident, value, .. } => {
				let expr_value = self.codegen_expr(value)?;
				self.variables.insert(ident.name, expr_value.clone());
				Ok(expr_value.is_never())
			}
			tbir::StmtKind::Assign { target, value } => {
				let variable = self.variables.get(&target.name).unwrap();
				let expr_value = self.codegen_expr(value)?;
				// self.builder.def_var(variable, expr_value);

				Ok(expr_value.is_never())
			}
			tbir::StmtKind::Loop { block } => {
				let func = self
					.builder
					.get_insert_block()
					.unwrap()
					.get_parent()
					.unwrap();
				let loop_header = self.ctx.append_basic_block(func, "loop");
				self.builder.position_at_end(loop_header);
				self.codegen_block(block)?;
				self.builder
					.build_unconditional_branch(loop_header)
					.unwrap();
				Ok(false)
			}
		}
	}

	fn codegen_expr(&mut self, expr: &tbir::Expr) -> Result<MaybeValue<'ctx>> {
		let value = match &expr.kind {
			tbir::ExprKind::Literal(lit, sym) => {
				let sym = self.tcx.scx.symbols.resolve(*sym);
				let val = match lit {
					lexer::LiteralKind::Integer => self
						.ctx
						.i64_type()
						.const_int(sym.parse::<u64>().unwrap(), true)
						.as_basic_value_enum(),
					lexer::LiteralKind::Float => todo!(),
					lexer::LiteralKind::Str => todo!(),
				};
				MaybeValue::Value(val)
			}
			tbir::ExprKind::Variable(ident) => {
				let var = match self.variables.get(&ident.name).unwrap() {
					MaybeValue::Value(val) => val.into_pointer_value(),
					MaybeValue::Zst | MaybeValue::Never => panic!(),
				};

				let value = self
					.builder
					.build_load(
						// TODO
						self.ctx.i64_type(),
						var,
						&self.tcx.scx.symbols.resolve(ident.name),
					)
					.unwrap()
					.as_basic_value_enum();

				MaybeValue::Value(value)
			}
			tbir::ExprKind::Binary(op, left, right) => self.codegen_bin_op(*op, left, right)?,
			tbir::ExprKind::FnCall { expr, args } => {
				let tbir::ExprKind::Variable(ident) = expr.kind else {
					todo!("not a fn")
				};
				let func = self
					.module
					.get_function(&self.tcx.scx.symbols.resolve(ident.name))
					.unwrap();
				if args.len() != func.count_params() as usize {
					return Err("fn call args count mismatch");
				}

				let mut argsz = Vec::new();
				for arg in args {
					let val = self.codegen_expr(arg)?;
					if let Some(Some(val)) = val.as_value_enum() {
						argsz.push(val.into());
					}
				}

				let call = self.builder.build_call(func, &argsz, "call").unwrap();
				let value = call.try_as_basic_value().left();
				match value {
					Some(val) => MaybeValue::Value(val),
					None => MaybeValue::Zst,
				}
			}
			tbir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => self.codegen_if(cond, conseq, altern.as_deref())?,
			tbir::ExprKind::Return(_) => todo!(),
			tbir::ExprKind::Break(_) => todo!(),
			tbir::ExprKind::Continue => todo!(),
		};
		Ok(value)
	}

	fn codegen_if(
		&mut self,
		cond: &tbir::Expr,
		conseq: &tbir::Block,
		altern: Option<&tbir::Block>,
	) -> Result<MaybeValue<'ctx>> {
		let condition = match self.codegen_expr(cond)? {
			MaybeValue::Value(val) => val.into_int_value(),
			MaybeValue::Zst => panic!(),
			MaybeValue::Never => return Ok(MaybeValue::Never),
		};
		let func = self
			.builder
			.get_insert_block()
			.unwrap()
			.get_parent()
			.unwrap();
		let then_bb = self.ctx.append_basic_block(func, "then");
		let else_bb = self.ctx.append_basic_block(func, "else");
		let merge_bb = self.ctx.append_basic_block(func, "merge");
		self.builder
			.build_conditional_branch(condition, then_bb, else_bb)
			.unwrap();
		self.builder.position_at_end(then_bb);
		let then_val = self.codegen_block(conseq)?;
		self.builder.build_unconditional_branch(merge_bb).unwrap();
		let then_bb = self.builder.get_insert_block().unwrap();
		self.builder.position_at_end(else_bb);
		let else_val = self.codegen_block(altern.as_ref().unwrap())?;
		self.builder.build_unconditional_branch(merge_bb).unwrap();
		let else_bb = self.builder.get_insert_block().unwrap();
		self.builder.position_at_end(merge_bb);
		let phi = self
			.builder
			.build_phi(self.ctx.i64_type(), "if_ret")
			.unwrap();
		if let Some(Some(then_val)) = then_val.as_value() {
			phi.add_incoming(&[(then_val, then_bb)]);
		}
		if let Some(Some(else_val)) = else_val.as_value() {
			phi.add_incoming(&[(else_val, else_bb)]);
		}
		Ok(MaybeValue::Value(phi.as_basic_value()))
	}

	fn codegen_bin_op(
		&mut self,
		op: ast::Spanned<lexer::BinOp>,
		left: &tbir::Expr,
		right: &tbir::Expr,
	) -> Result<MaybeValue<'ctx>> {
		tracing::trace!("codegen_bin_op");

		let lhs = self.codegen_expr(left)?;
		let rhs = self.codegen_expr(right)?;

		// cannot be zst
		let (lhs, rhs) = match (lhs, rhs) {
			(MaybeValue::Value(lhs), MaybeValue::Value(rhs)) => (lhs, rhs),
			(MaybeValue::Never, _) | (_, MaybeValue::Never) => return Ok(MaybeValue::Never),
			_ => panic!(),
		};

		let lhs = lhs.into_int_value();
		let rhs = rhs.into_int_value();

		let ins = match op.bit {
			lexer::BinOp::Plus => self.builder.build_int_add(lhs, rhs, "").unwrap(),
			lexer::BinOp::Minus => self.builder.build_int_sub(lhs, rhs, "").unwrap(),
			lexer::BinOp::Mul => self.builder.build_int_mul(lhs, rhs, "").unwrap(),
			lexer::BinOp::Div => self.builder.build_int_unsigned_div(lhs, rhs, "").unwrap(),
			lexer::BinOp::Mod => self.builder.build_int_unsigned_rem(lhs, rhs, "").unwrap(),

			lexer::BinOp::Gt => self
				.builder
				.build_int_compare(IntPredicate::SGT, lhs, rhs, "")
				.unwrap(),
			lexer::BinOp::Ge => self
				.builder
				.build_int_compare(IntPredicate::SGE, lhs, rhs, "")
				.unwrap(),
			lexer::BinOp::Lt => self
				.builder
				.build_int_compare(IntPredicate::SLT, lhs, rhs, "")
				.unwrap(),
			lexer::BinOp::Le => self
				.builder
				.build_int_compare(IntPredicate::SLE, lhs, rhs, "")
				.unwrap(),
			lexer::BinOp::EqEq => self
				.builder
				.build_int_compare(IntPredicate::EQ, lhs, rhs, "")
				.unwrap(),
			lexer::BinOp::Ne => self
				.builder
				.build_int_compare(IntPredicate::NE, lhs, rhs, "")
				.unwrap(),
		};

		Ok(MaybeValue::Value(ins.as_basic_value_enum()))
	}
}
