use std::{collections::HashMap, iter};

use inkwell::{
	AddressSpace, OptimizationLevel,
	builder::Builder,
	context::Context,
	module::Module,
	passes::PassBuilderOptions,
	targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
	values::{FunctionValue, IntValue, PointerValue},
};

use crate::{
	Ident,
	parser::{Expr, Function, Prototype},
};

pub struct CodeGen<'ctx> {
	ctx: &'ctx Context,
	builder: Builder<'ctx>,
	module: Module<'ctx>,

	variables: HashMap<Ident, PointerValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
	pub fn new(ctx: &'ctx Context) -> Self {
		Self {
			ctx,
			builder: ctx.create_builder(),
			module: ctx.create_module("repl"),

			variables: HashMap::new(),
		}
	}
}

impl<'ctx> CodeGen<'ctx> {
	fn compile_bin_expr<'a>(
		&'a self,
		op: char,
		left: IntValue<'ctx>,
		right: IntValue<'ctx>,
	) -> Result<IntValue<'ctx>, &'static str> {
		match op {
			'+' => Ok(self.builder.build_int_add(left, right, "tmpadd").unwrap()),
			'-' => Ok(self.builder.build_int_sub(left, right, "tmpsub").unwrap()),
			'*' => Ok(self.builder.build_int_mul(left, right, "tmpmul").unwrap()),
			_ => Err("non valid op"),
		}
	}

	fn compile_expr(&mut self, expr: &Expr) -> Result<IntValue<'ctx>, &'static str> {
		let value = match expr {
			Expr::NumLiteral(num) => self.ctx.i64_type().const_int(num.value, true),
			Expr::Ident(ident) => {
				let var = self.variables.get(ident).unwrap();

				self.builder
					.build_load(self.ctx.i64_type(), *var, &ident.0)
					.unwrap()
					.into_int_value()
			}
			Expr::Bin { op, left, right } => {
				let left = self.compile_expr(left)?;
				let right = self.compile_expr(right)?;

				self.compile_bin_expr(*op, left, right)?
			}
			Expr::FnCall { ident, args } => {
				let func = self.module.get_function(&ident.0).unwrap();
				if args.len() != func.count_params() as usize {
					todo!()
				}

				let args = args
					.iter()
					.map(|arg| self.compile_expr(arg).map(Into::into))
					.collect::<Result<Vec<_>, &'static str>>()?;

				let call = self.builder.build_call(func, &args, "call").unwrap();
				let value = call.try_as_basic_value().left().unwrap();
				value.into_int_value()
			}
		};

		Ok(value)
	}

	pub fn compile_prototype(
		&mut self,
		prototype: &Prototype,
	) -> Result<FunctionValue<'ctx>, &'static str> {
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

	pub fn compile_fn(&mut self, func: &Function) -> Result<FunctionValue<'ctx>, &'static str> {
		if self.module.get_function(&func.proto.name.0).is_some() {
			return Err("cannot redefine extern function");
		}

		let fn_val = self.compile_prototype(&func.proto)?;

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

		let ret_val = self.compile_expr(&func.body)?;
		self.builder.build_return(Some(&ret_val)).unwrap();

		if !fn_val.verify(true) {
			unsafe { fn_val.delete() }
			return Err("function is invalid");
		}

		Ok(fn_val)
	}
}

impl CodeGen<'_> {
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
