use std::{collections::HashMap, mem};

use cranelift::prelude::*;
use cranelift_control::ControlPlane;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

use crate::{
	Ident, Result,
	lexer::Operator,
	parser::{Expr, Function, Prototype},
};

use super::CodeGen;

pub struct Generator {
	builder_context: FunctionBuilderContext,
	functions: HashMap<Ident, CompiledFunction>,
	module: JITModule,
	variable_generator: VariableGenerator,
}

impl Generator {
	pub fn new() -> Self {
		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();
		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();
		let builder = JITBuilder::with_isa(isa, default_libcall_names());

		Self {
			builder_context: FunctionBuilderContext::new(),
			functions: HashMap::new(),
			module: JITModule::new(builder),
			variable_generator: VariableGenerator::default(),
		}
	}

	pub fn prototype(&mut self, prototype: &Prototype, linkage: Linkage) -> Result<FuncId> {
		match self.functions.get(&prototype.name) {
			None => {
				let mut signature = self.module.make_signature();
				for _ in &prototype.args {
					signature.params.push(AbiParam::new(types::I64));
				}
				signature.returns.push(AbiParam::new(types::I64));

				let func_id = self
					.module
					.declare_function(&prototype.name.0, linkage, &signature)
					.unwrap();
				self.functions.insert(
					prototype.name.clone(),
					CompiledFunction {
						defined: false,
						id: func_id,
						param_count: prototype.args.len(),
					},
				);
				Ok(func_id)
			}
			Some(_) => Err("already defined"),
		}
	}
}

impl CodeGen for Generator {
	type Fn = fn() -> i64;

	fn extern_(&mut self, prototype: &Prototype) -> Result<()> {
		self.prototype(prototype, Linkage::Import)?;
		Ok(())
	}

	fn function(&mut self, function: &Function) -> Result<Self::Fn> {
		let mut context = self.module.make_context();

		let signature = &mut context.func.signature;
		for _ in &function.proto.args {
			signature.params.push(AbiParam::new(types::I64));
		}
		signature.returns.push(AbiParam::new(types::I64));

		let func_id = self.prototype(&function.proto, Linkage::Export)?;

		let mut builder = FunctionBuilder::new(&mut context.func, &mut self.builder_context);

		let entry_block = builder.create_block();
		builder.append_block_params_for_function_params(entry_block);
		builder.switch_to_block(entry_block);
		builder.seal_block(entry_block);

		let mut values = HashMap::new();
		for (i, argument) in function.proto.args.iter().enumerate() {
			let value = builder.block_params(entry_block)[i];
			let variable = self.variable_generator.create_var(&mut builder, value);
			values.insert(argument.clone(), variable);
		}

		if let Some(ref mut func) = self.functions.get_mut(&function.proto.name) {
			func.defined = true;
		}

		let mut generator = FunctionGenerator {
			builder,
			functions: &self.functions,
			module: &mut self.module,
			values,
		};

		let return_value = match generator.expr(&function.body) {
			Ok(ret) => ret,
			Err(err) => {
				self.functions.remove(&function.proto.name);
				return Err(err);
			}
		};

		generator.builder.ins().return_(&[return_value]);
		generator.builder.finalize();

		context
			.optimize(self.module.isa(), &mut ControlPlane::default())
			.unwrap();

		print!("{}", context.func.display());

		self.module.define_function(func_id, &mut context).unwrap();
		self.module.clear_context(&mut context);
		self.module.finalize_definitions().unwrap();

		if function.proto.name.0.starts_with("__anon") {
			self.functions.remove(&function.proto.name);
		}

		let fn_ = self.module.get_finalized_function(func_id);

		#[allow(unsafe_code)]
		// TODO: this is unsafe as some functions ask for arguments
		let fn_ = unsafe { mem::transmute::<*const u8, fn() -> i64>(fn_) };

		Ok(fn_)
	}

	fn call_fn(&mut self, func: Self::Fn) -> Result<i64> {
		Ok(func())
	}
}

#[derive(Debug, Default)]
struct VariableGenerator {
	index: usize,
}

impl VariableGenerator {
	fn create_var(&mut self, builder: &mut FunctionBuilder, value: Value) -> Variable {
		let variable = Variable::new(self.index);
		self.index += 1;
		builder.declare_var(variable, types::I64);
		builder.def_var(variable, value);
		variable
	}
}

struct CompiledFunction {
	defined: bool,
	id: FuncId,
	param_count: usize,
}

pub struct FunctionGenerator<'a> {
	builder: FunctionBuilder<'a>,
	functions: &'a HashMap<Ident, CompiledFunction>,
	module: &'a mut JITModule,
	values: HashMap<Ident, Variable>,
}

impl FunctionGenerator<'_> {
	fn expr(&mut self, expr: &Expr) -> Result<Value> {
		let value = match expr {
			Expr::Literal(num) => self.builder.ins().iconst(types::I64, num.value),
			Expr::Ident(ident) => match self.values.get(ident) {
				Some(var) => self.builder.use_var(*var),
				None => return Err("var undefined"),
			},
			Expr::Binary { op, left, right } => {
				let lhs = self.expr(left)?;
				let rhs = self.expr(right)?;

				match op {
					Operator::Plus => self.builder.ins().iadd(lhs, rhs),
					Operator::Minus => self.builder.ins().isub(lhs, rhs),
					Operator::Mul => self.builder.ins().imul(lhs, rhs),
					Operator::Div => self.builder.ins().udiv(lhs, rhs),

					Operator::Gt => self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
					Operator::Lt => self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
				}
			}
			Expr::FnCall { ident, args } => match self.functions.get(ident) {
				Some(func) => {
					if func.param_count != args.len() {
						return Err("invalid fn call: args nb mismatch");
					}

					let local_func = self.module.declare_func_in_func(func.id, self.builder.func);

					let args = args
						.iter()
						.map(|arg| self.expr(arg))
						.collect::<Result<Vec<_>>>()?;

					let call = self.builder.ins().call(local_func, &args);

					self.builder.inst_results(call)[0]
				}
				None => return Err("invalid fn call"),
			},
			Expr::If {
				condition,
				consequence,
				alternative,
			} => {
				let condition = self.expr(condition)?;

				let then_block = self.builder.create_block();
				let else_block = self.builder.create_block();
				let cont_block = self.builder.create_block();

				// cranelift block params are used like phi values
				self.builder.append_block_param(cont_block, types::I64);

				self.builder
					.ins()
					.brif(condition, then_block, &[], else_block, &[]);

				self.builder.switch_to_block(then_block);
				self.builder.seal_block(then_block);
				let then_ret = self.expr(consequence)?;

				self.builder.ins().jump(cont_block, &[then_ret.into()]);

				self.builder.switch_to_block(else_block);
				self.builder.seal_block(else_block);
				let else_ret = self.expr(alternative)?;

				self.builder.ins().jump(cont_block, &[else_ret.into()]);

				self.builder.switch_to_block(cont_block);
				self.builder.seal_block(cont_block);

				self.builder.block_params(cont_block)[0]
			}
		};

		Ok(value)
	}
}
