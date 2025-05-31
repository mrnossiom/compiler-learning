use std::{collections::HashMap, mem};

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

use crate::{
	Ident,
	parser::{Expr, Function, Prototype},
};

type Result<T> = std::result::Result<T, &'static str>;

pub struct Generator {
	builder_context: FunctionBuilderContext,
	functions: HashMap<Ident, CompiledFunction>,
	module: JITModule,
	variable_generator: VariableGenerator,
}

impl Generator {
	pub fn new() -> Self {
		let builder = JITBuilder::new(default_libcall_names()).unwrap();
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

	pub fn function(&mut self, function: &Function) -> Result<fn() -> i64> {
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
				generator.builder.finalize();
				self.functions.remove(&function.proto.name);
				return Err(err);
			}
		};

		generator.builder.ins().return_(&[return_value]);
		generator.builder.finalize();

		print!("{}", context.func.display());

		self.module.define_function(func_id, &mut context).unwrap();
		self.module.clear_context(&mut context);
		self.module.finalize_definitions().unwrap();

		let fn_ = self.module.get_finalized_function(func_id);

		#[allow(unsafe_code)]
		// TODO: this is unsafe as some functions ask for arguments
		let fn_ = unsafe { mem::transmute::<*const u8, fn() -> i64>(fn_) };

		Ok(fn_)
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
			Expr::Literal(num) => self.builder.ins().iconst(types::I64, num.value as i64),
			Expr::Ident(ident) => match self.values.get(&ident) {
				Some(var) => self.builder.use_var(*var),
				None => return Err("var undefined"),
			},
			Expr::Bin { op, left, right } => {
				let lhs = self.expr(left)?;
				let rhs = self.expr(right)?;

				match op {
					'+' => self.builder.ins().iadd(lhs, rhs),
					'-' => self.builder.ins().isub(lhs, rhs),
					'*' => self.builder.ins().imul(lhs, rhs),
					'/' => self.builder.ins().udiv(lhs, rhs),
					_ => return Err("undefined binop"),
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
		};

		Ok(value)
	}
}
