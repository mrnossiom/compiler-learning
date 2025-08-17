use std::{collections::HashMap, iter};

use inkwell::{
	AddressSpace, IntPredicate, OptimizationLevel,
	builder::Builder,
	context::Context,
	execution_engine::ExecutionEngine,
	module::Module,
	passes::PassBuilderOptions,
	targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
	values::{FunctionValue, IntValue, PointerValue},
};

use crate::{
	Result,
	ast::Spanned,
	codegen::Backend,
	hir,
	lexer::{BinOp, LiteralKind},
	resolve::Environment,
	session::{SessionCtx, Symbol},
	tbir::{self, Expr, ExprKind},
	ty::{self, FnDecl},
};

pub struct Generator<'scx, 'ctx> {
	scx: &'scx SessionCtx,

	ctx: &'ctx Context,
	builder: Builder<'ctx>,
	module: Module<'ctx>,
	jit: ExecutionEngine<'ctx>,

	variables: HashMap<Symbol, PointerValue<'ctx>>,
}

impl<'scx, 'ctx> Generator<'scx, 'ctx> {
	pub fn new(scx: &'scx SessionCtx, ctx: &'ctx Context) -> Self {
		let module = ctx.create_module("repl");
		let jit = module
			.create_jit_execution_engine(OptimizationLevel::None)
			.unwrap();
		Self {
			scx,

			ctx,
			builder: ctx.create_builder(),
			module,
			jit,

			variables: HashMap::new(),
		}
	}
}

impl<'ctx> Backend for Generator<'_, 'ctx> {
	type FuncId = FunctionValue<'ctx>;

	fn codegen_root(&mut self, hir: &hir::Root, env: &Environment) {
		todo!()
	}

	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()> {
		self.define_func(name, decl)?;
		Ok(())
	}

	fn declare_function(
		&mut self,
		name: Symbol,
		decl: &FnDecl,
		// body: &tbir::Block,
	) -> Result<Self::FuncId> {
		if self
			.module
			.get_function(&self.scx.symbols.resolve(name))
			.is_some()
		{
			return Err("cannot redefine extern function");
		}

		let func_val = self.define_func(name, decl)?;
		Ok(func_val)
	}

	fn define_function(
		&mut self,
		func_val: Self::FuncId,
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
				self.variables.insert(
					ident.name,
					arg.into_int_value()
						.const_to_pointer(self.ctx.ptr_type(AddressSpace::default())),
				);
			});

		let ret_val = self.codegen_block(body)?;

		self.builder.build_return(Some(&ret_val)).unwrap();

		if !func_val.verify(true) {
			#[allow(unsafe_code)]
			unsafe {
				func_val.delete();
			}
			return Err("function is invalid");
		}

		if self.scx.options.print.contains("bir") {
			func_val.print_to_stderr();
		}

		Ok(())
	}

	// fn call_fn(&mut self, func: Self::Fn) -> Result<i64> {
	// 	#[allow(unsafe_code)]
	// 	let ret = unsafe {
	// 		self.jit
	// 			.get_function::<unsafe extern "C" fn() -> i64>(func.get_name().to_str().unwrap())
	// 	}
	// 	.unwrap();
	// 	#[allow(unsafe_code)]
	// 	Ok(unsafe { ret.call() })
	// }
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

	pub fn define_func(&mut self, name: Symbol, decl: &FnDecl) -> Result<FunctionValue<'ctx>> {
		let args_ty = iter::repeat_n(self.ctx.i64_type(), decl.inputs.len())
			.map(Into::into)
			.collect::<Vec<_>>();
		let fn_ty = self.ctx.i64_type().fn_type(&args_ty, false);

		let name = self.scx.symbols.resolve(name);
		let fn_val = self.module.add_function(&name, fn_ty, None);

		// set arguments name
		fn_val
			.get_param_iter()
			.zip(&decl.inputs)
			.for_each(|(arg, ty::Param { ident, ty })| {
				arg.into_int_value()
					.set_name(&self.scx.symbols.resolve(ident.name));
			});

		Ok(fn_val)
	}
}

impl<'ctx> Generator<'_, 'ctx> {
	fn codegen_block(&mut self, block: &tbir::Block) -> Result<IntValue<'ctx>> {
		for stmt in &block.stmts {
			self.codegen_stmt(stmt)?;
		}

		if let Some(expr) = &block.ret {
			self.codegen_expr(expr)
		} else {
			// TODO: should return a zst
			Ok(self.ctx.i64_type().const_int(0, false))
		}
	}

	fn codegen_stmt(&mut self, stmt: &tbir::Stmt) -> Result<()> {
		match &stmt.kind {
			tbir::StmtKind::Expr(expr) => {
				self.codegen_expr(expr)?;
				Ok(())
			}
			tbir::StmtKind::Let { ident, value, .. } => {
				// 	let expr_value = self.codegen_expr(value)?;
				// 	let value = self.variable_generator.create_var(
				// 		&mut self.builder,
				// 		expr_value,
				// 		to_cl_type(&value.ty),
				// 	);
				// 	self.values.insert(ident.name, value);
				// 	Ok(())
				todo!()
			}
			tbir::StmtKind::Assign { target, value } => {
				// let variable = *self.values.get(&target.name).unwrap();
				// let expr_value = self.codegen_expr(value)?;
				// self.builder.def_var(variable, expr_value);

				// Ok(())
				todo!()
			}
			tbir::StmtKind::Loop { block } => {
				// let loop_header = self.builder.create_block();
				// self.builder.switch_to_block(loop_header);
				// self.codegen_block(block)?;
				// self.builder.ins().jump(loop_header, &[]);
				// self.builder.seal_block(loop_header);
				// Ok(())
				todo!()
			}
		}
	}

	fn codegen_expr(&mut self, expr: &Expr) -> Result<IntValue<'ctx>> {
		let value = match &expr.kind {
			ExprKind::Literal(lit, sym) => {
				let sym = self.scx.symbols.resolve(*sym);
				match lit {
					LiteralKind::Integer => self
						.ctx
						.i64_type()
						.const_int(sym.parse::<u64>().unwrap(), true),
					LiteralKind::Float => todo!(),
					LiteralKind::Str => todo!(),
				}
			}
			ExprKind::Variable(ident) => {
				let var = self.variables.get(&ident.name).unwrap();

				self.builder
					.build_load(
						self.ctx.i64_type(),
						*var,
						&self.scx.symbols.resolve(ident.name),
					)
					.unwrap()
					.into_int_value()
			}
			ExprKind::Binary(op, left, right) => {
				let left = self.codegen_expr(left)?;
				let right = self.codegen_expr(right)?;

				self.codegen_bin_op(*op, left, right)?
			}
			ExprKind::FnCall { expr, args } => {
				let tbir::ExprKind::Variable(ident) = expr.kind else {
					todo!("not a fn")
				};
				let func = self
					.module
					.get_function(&self.scx.symbols.resolve(ident.name))
					.unwrap();
				if args.len() != func.count_params() as usize {
					return Err("fn call args count mismatch");
				}

				let args = args
					.iter()
					.map(|arg| self.codegen_expr(arg).map(Into::into))
					.collect::<Result<Vec<_>>>()?;

				let call = self.builder.build_call(func, &args, "call").unwrap();
				let value = call.try_as_basic_value().left().unwrap();
				value.into_int_value()
			}
			ExprKind::If {
				cond,
				conseq,
				altern,
			} => {
				let condition = self.codegen_expr(cond)?;
				let condition = self
					.builder
					.build_int_compare(
						IntPredicate::NE,
						condition,
						self.ctx.bool_type().const_int(0, false),
						"",
					)
					.unwrap();

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
				// TODO
				let else_val = self.codegen_block(altern.as_ref().unwrap())?;
				self.builder.build_unconditional_branch(merge_bb).unwrap();
				let else_bb = self.builder.get_insert_block().unwrap();

				self.builder.position_at_end(merge_bb);

				let phi = self
					.builder
					.build_phi(self.ctx.i64_type(), "if_ret")
					.unwrap();
				phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

				phi.as_basic_value().into_int_value()
			}
			ExprKind::Return(_) => todo!(),
			ExprKind::Break(_) => todo!(),
			ExprKind::Continue => todo!(),
		};

		Ok(value)
	}

	fn codegen_bin_op<'a>(
		&'a self,
		op: Spanned<BinOp>,
		left: IntValue<'ctx>,
		right: IntValue<'ctx>,
	) -> Result<IntValue<'ctx>> {
		let ins = match op.bit {
			BinOp::Plus => self.builder.build_int_add(left, right, "").unwrap(),
			BinOp::Minus => self.builder.build_int_sub(left, right, "").unwrap(),
			BinOp::Mul => self.builder.build_int_mul(left, right, "").unwrap(),
			BinOp::Div => self
				.builder
				.build_int_unsigned_div(left, right, "")
				.unwrap(),

			BinOp::Gt => self
				.builder
				.build_int_compare(IntPredicate::SGT, left, right, "")
				.unwrap(),
			BinOp::Lt => self
				.builder
				.build_int_compare(IntPredicate::SLT, left, right, "")
				.unwrap(),
			BinOp::EqEq => self
				.builder
				.build_int_compare(IntPredicate::EQ, left, right, "")
				.unwrap(),
			_ => todo!(),
		};
		Ok(ins)
	}
}
