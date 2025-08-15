use std::{collections::HashMap, mem};

use cranelift::prelude::*;
use cranelift_control::ControlPlane;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};

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
	functions: HashMap<Symbol, CompiledFunction>,
	module: JITModule,
	variable_generator: VariableGenerator,
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
			variable_generator: VariableGenerator::default(),
		}
	}
}

fn to_cl_type(output: &ty::TyKind) -> Type {
	match &output {
		ty::TyKind::Unit => types::I8, // TODO: unit is zst
		ty::TyKind::Never => todo!(),
		ty::TyKind::Bool => types::I8,
		ty::TyKind::Integer => types::I32,
		ty::TyKind::Float => types::F32,
		ty::TyKind::Str => todo!(),
		ty::TyKind::Fn(fn_decl) => todo!(),
		ty::TyKind::Infer(_, _) => unreachable!(),
	}
}

impl Generator<'_> {
	pub fn declare_func(
		&mut self,
		name: Symbol,
		decl: &ty::FnDecl,
		linkage: Linkage,
	) -> Result<FuncId> {
		match self.functions.get(&name) {
			None => {
				let mut signature = self.module.make_signature();
				for ty::Param(_name, ty) in &decl.inputs {
					signature.params.push(AbiParam::new(to_cl_type(ty)));
				}
				signature
					.returns
					.push(AbiParam::new(to_cl_type(&decl.output)));

				let func_name = self.scx.symbols.resolve(name);
				let func_id = self
					.module
					.declare_function(&func_name, linkage, &signature)
					.unwrap();

				self.functions.insert(
					name,
					CompiledFunction {
						defined: false,
						id: func_id,
						param_count: decl.inputs.len(),
					},
				);
				Ok(func_id)
			}
			Some(_) => Err("already defined"),
		}
	}
}

impl CodeGen for Generator<'_> {
	type Fn = fn() -> i64;

	fn extern_(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()> {
		self.declare_func(name, decl, Linkage::Import)?;
		Ok(())
	}

	fn function(
		&mut self,
		name: Symbol,
		decl: &ty::FnDecl,
		body: &tbir::Block,
	) -> Result<Self::Fn> {
		let mut context = self.module.make_context();

		let signature = &mut context.func.signature;
		for ty::Param(_name, ty) in &decl.inputs {
			signature.params.push(AbiParam::new(to_cl_type(ty)));
		}
		signature
			.returns
			.push(AbiParam::new(to_cl_type(&decl.output)));

		let func_id = self.declare_func(name, decl, Linkage::Export)?;

		let mut builder = FunctionBuilder::new(&mut context.func, &mut self.builder_context);

		let entry_block = builder.create_block();
		builder.append_block_params_for_function_params(entry_block);
		builder.switch_to_block(entry_block);
		builder.seal_block(entry_block);

		let mut values = HashMap::new();
		for (i, ty::Param(argument, ty)) in decl.inputs.iter().enumerate() {
			let value = builder.block_params(entry_block)[i];
			let variable = self
				.variable_generator
				.create_var(&mut builder, value, to_cl_type(ty));
			values.insert(argument.name, variable);
		}

		if let Some(ref mut func) = self.functions.get_mut(&name) {
			func.defined = true;
		}

		let mut generator = FunctionGenerator {
			scx: self.scx,
			builder,
			functions: &self.functions,
			module: &mut self.module,
			values,
			variable_generator: &mut self.variable_generator,
		};

		let return_value = match generator.codegen_block(body) {
			Ok(ret) => ret,
			Err(err) => {
				self.functions.remove(&name);
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
	fn create_var(&mut self, builder: &mut FunctionBuilder, value: Value, ty: Type) -> Variable {
		let variable = Variable::new(self.index);
		self.index += 1;
		builder.declare_var(variable, ty);
		builder.def_var(variable, value);
		variable
	}
}

struct CompiledFunction {
	defined: bool,
	id: FuncId,
	param_count: usize,
}

pub struct FunctionGenerator<'scx, 'bld> {
	scx: &'scx SessionCtx,

	builder: FunctionBuilder<'bld>,
	functions: &'bld HashMap<Symbol, CompiledFunction>,
	module: &'bld mut JITModule,
	values: HashMap<Symbol, Variable>,
	variable_generator: &'bld mut VariableGenerator,
}

impl FunctionGenerator<'_, '_> {
	fn codegen_block(&mut self, block: &tbir::Block) -> Result<Value> {
		for stmt in &block.stmts {
			self.codegen_stmt(stmt)?;
		}

		if let Some(expr) = &block.ret {
			self.codegen_expr(expr)
		} else {
			// TODO: should return a zst
			Ok(self.builder.ins().iconst(types::I8, 0))
		}
	}

	fn codegen_stmt(&mut self, stmt: &tbir::Stmt) -> Result<()> {
		match &stmt.kind {
			tbir::StmtKind::Expr(expr) => {
				self.codegen_expr(expr)?;
				Ok(())
			}
			tbir::StmtKind::Let { ident, value, .. } => {
				let expr_value = self.codegen_expr(value)?;
				let value = self.variable_generator.create_var(
					&mut self.builder,
					expr_value,
					to_cl_type(&value.ty),
				);
				self.values.insert(ident.name, value);
				Ok(())
			}
			tbir::StmtKind::Assign { target, value } => {
				let variable = *self.values.get(&target.name).unwrap();
				let expr_value = self.codegen_expr(value)?;
				self.builder.def_var(variable, expr_value);

				Ok(())
			}
			tbir::StmtKind::Loop { block } => {
				let loop_header = self.builder.create_block();
				self.builder.switch_to_block(loop_header);
				self.codegen_block(block)?;
				self.builder.ins().jump(loop_header, &[]);
				self.builder.seal_block(loop_header);
				Ok(())
			}
		}
	}

	fn codegen_expr(&mut self, expr: &tbir::Expr) -> Result<Value> {
		let value = match &expr.kind {
			tbir::ExprKind::Literal(lit, sym) => {
				let sym = self.scx.symbols.resolve(*sym);
				match lit {
					lexer::LiteralKind::Integer => self
						.builder
						.ins()
						.iconst(to_cl_type(&expr.ty), sym.parse::<i64>().unwrap()),
					lexer::LiteralKind::Float => {
						self.builder.ins().f64const(sym.parse::<f64>().unwrap())
					}
					lexer::LiteralKind::Str => todo!(),
				}
			}
			tbir::ExprKind::Variable(ident) => match self.values.get(&ident.name) {
				Some(var) => self.builder.use_var(*var),
				None => return Err("var undefined"),
			},
			tbir::ExprKind::Binary(op, left, right) => self.codegen_binop(*op, left, right)?,
			tbir::ExprKind::FnCall { expr, args } => {
				let tbir::ExprKind::Variable(ident) = expr.kind else {
					todo!("not a fn")
				};
				let Some(func) = self.functions.get(&ident.name) else {
					return Err("invalid fn call");
				};

				if func.param_count != args.len() {
					return Err("invalid fn call: args nb mismatch");
				}

				let local_func = self.module.declare_func_in_func(func.id, self.builder.func);

				let args = args
					.iter()
					.map(|arg| self.codegen_expr(arg))
					.collect::<Result<Vec<_>>>()?;

				let call = self.builder.ins().call(local_func, &args);

				self.builder.inst_results(call)[0]
			}
			tbir::ExprKind::If {
				cond,
				conseq,
				altern,
			} => {
				let condition = self.codegen_expr(cond)?;

				let then_block = self.builder.create_block();
				// only make a block if the if has an alternative
				let else_block = altern.as_ref().map(|_| self.builder.create_block());
				let cont_block = self.builder.create_block();

				// cranelift block params are used like phi values
				self.builder.append_block_param(cont_block, types::I64);

				self.builder.ins().brif(
					condition,
					then_block,
					&[],
					else_block.unwrap_or(cont_block),
					&[],
				);

				self.builder.switch_to_block(then_block);
				self.builder.seal_block(then_block);
				let then_ret = self.codegen_block(conseq)?;

				self.builder.ins().jump(cont_block, &[then_ret.into()]);

				if let Some(altern) = altern {
					// TODO
					let else_block = else_block.unwrap();

					self.builder.switch_to_block(else_block);
					self.builder.seal_block(else_block);

					let else_ret = self.codegen_block(altern)?;
					self.builder.ins().jump(cont_block, &[else_ret.into()]);
				}

				self.builder.switch_to_block(cont_block);
				self.builder.seal_block(cont_block);

				self.builder.block_params(cont_block)[0]
			}
			tbir::ExprKind::Break(expr) => {
				let expr_value = expr.as_ref().map(|expr| self.codegen_expr(expr));
				todo!("break expr")
			}
			tbir::ExprKind::Continue => {
				todo!("continue expr")
			}
		};

		Ok(value)
	}

	fn codegen_binop(
		&mut self,
		op: ast::Spanned<lexer::BinOp>,
		left: &tbir::Expr,
		right: &tbir::Expr,
	) -> Result<Value> {
		let lhs = self.codegen_expr(left)?;
		let rhs = self.codegen_expr(right)?;

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
}
