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
	Ident, Result,
	lexer::Operator,
	parser::{Expr, Function, Prototype},
};

use super::CodeGen;

pub struct Generator<'ctx> {
	ctx: &'ctx Context,
	builder: Builder<'ctx>,
	module: Module<'ctx>,
	jit: ExecutionEngine<'ctx>,

	variables: HashMap<Ident, PointerValue<'ctx>>,
}

impl<'ctx> Generator<'ctx> {
	pub fn new(ctx: &'ctx Context) -> Self {
		let module = ctx.create_module("repl");
		let jit = module
			.create_jit_execution_engine(OptimizationLevel::None)
			.unwrap();
		Self {
			ctx,
			builder: ctx.create_builder(),
			module,
			jit,

			variables: HashMap::new(),
		}
	}
}

impl<'ctx> Generator<'ctx> {
	fn compile_bin_expr<'a>(
		&'a self,
		op: Operator,
		left: IntValue<'ctx>,
		right: IntValue<'ctx>,
	) -> Result<IntValue<'ctx>> {
		let ins = match op {
			Operator::Plus => self.builder.build_int_add(left, right, "").unwrap(),
			Operator::Minus => self.builder.build_int_sub(left, right, "").unwrap(),
			Operator::Mul => self.builder.build_int_mul(left, right, "").unwrap(),
			Operator::Div => self
				.builder
				.build_int_unsigned_div(left, right, "")
				.unwrap(),

			Operator::Gt => self
				.builder
				.build_int_compare(IntPredicate::SGT, left, right, "")
				.unwrap(),
			Operator::Lt => self
				.builder
				.build_int_compare(IntPredicate::SLT, left, right, "")
				.unwrap(),
		};
		Ok(ins)
	}

	fn expr(&mut self, expr: &Expr) -> Result<IntValue<'ctx>> {
		let value = match expr {
			Expr::Literal(num) => self.ctx.i64_type().const_int(num.value as u64, true),
			Expr::Ident(ident) => {
				let var = self.variables.get(ident).unwrap();

				self.builder
					.build_load(self.ctx.i64_type(), *var, &ident.0)
					.unwrap()
					.into_int_value()
			}
			Expr::Binary { op, left, right } => {
				let left = self.expr(left)?;
				let right = self.expr(right)?;

				self.compile_bin_expr(*op, left, right)?
			}
			Expr::FnCall { ident, args } => {
				let func = self.module.get_function(&ident.0).unwrap();
				if args.len() != func.count_params() as usize {
					return Err("fn call args count mismatch");
				}

				let args = args
					.iter()
					.map(|arg| self.expr(arg).map(Into::into))
					.collect::<Result<Vec<_>>>()?;

				let call = self.builder.build_call(func, &args, "call").unwrap();
				let value = call.try_as_basic_value().left().unwrap();
				value.into_int_value()
			}
			Expr::If {
				condition,
				consequence,
				alternative,
			} => {
				let condition = self.expr(condition)?;
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
				let then_val = self.expr(consequence)?;
				self.builder.build_unconditional_branch(merge_bb).unwrap();
				let then_bb = self.builder.get_insert_block().unwrap();

				self.builder.position_at_end(else_bb);
				let else_val = self.expr(alternative)?;
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
		};

		Ok(value)
	}

	pub fn prototype(&mut self, prototype: &Prototype) -> Result<FunctionValue<'ctx>> {
		let args_ty = iter::repeat_n(self.ctx.i64_type(), prototype.args.len())
			.map(Into::into)
			.collect::<Vec<_>>();
		let fn_ty = self.ctx.i64_type().fn_type(&args_ty, false);

		let fn_val = self.module.add_function(&prototype.name.0, fn_ty, None);

		// set arguments name
		fn_val
			.get_param_iter()
			.zip(&prototype.args)
			.for_each(|(arg, parg)| arg.into_int_value().set_name(&parg.0));

		Ok(fn_val)
	}
}

impl<'ctx> CodeGen for Generator<'ctx> {
	type Fn = FunctionValue<'ctx>;

	fn extern_(&mut self, proto: &Prototype) -> Result<()> {
		self.prototype(proto)?;
		Ok(())
	}

	fn function(&mut self, func: &Function) -> Result<Self::Fn> {
		if self.module.get_function(&func.proto.name.0).is_some() {
			return Err("cannot redefine extern function");
		}

		let fn_val = self.prototype(&func.proto)?;

		let bb = self.ctx.append_basic_block(fn_val, "entry");
		self.builder.position_at_end(bb);

		self.variables.clear();
		fn_val
			.get_param_iter()
			.zip(&func.proto.args)
			.for_each(|(arg, parg)| {
				self.variables.insert(
					parg.clone(),
					arg.into_int_value()
						.const_to_pointer(self.ctx.ptr_type(AddressSpace::default())),
				);
			});

		let ret_val = self.expr(&func.body)?;
		self.builder.build_return(Some(&ret_val)).unwrap();

		if !fn_val.verify(true) {
			#[allow(unsafe_code)]
			unsafe {
				fn_val.delete();
			}
			return Err("function is invalid");
		}

		fn_val.print_to_stderr();

		Ok(fn_val)
	}

	fn call_fn(&mut self, func: Self::Fn) -> Result<i64> {
		#[allow(unsafe_code)]
		let ret = unsafe {
			self.jit
				.get_function::<unsafe extern "C" fn() -> i64>(func.get_name().to_str().unwrap())
		}
		.unwrap();
		#[allow(unsafe_code)]
		Ok(unsafe { ret.call() })
	}
}

impl Generator<'_> {
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
}
