use crate::{Result, session::Symbol, tbir, ty};
#[cfg(feature = "cranelift")]
use crate::{hir, resolve::Environment, session::SessionCtx, ty::TyCtx};

#[cfg(feature = "cranelift")]
mod cranelift;
#[cfg(feature = "llvm")]
mod llvm;

#[cfg(feature = "cranelift")]
pub use self::cranelift::Generator;
#[cfg(feature = "llvm")]
pub use self::llvm::Generator;

pub trait CodeGen {
	type FuncId;

	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()>;
	fn declare_function(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<Self::FuncId>;

	fn define_function(
		&mut self,
		func_id: Self::FuncId,
		decl: &ty::FnDecl,
		body: &tbir::Block,
		print_bir: bool,
	) -> Result<()>;
}

#[derive(Debug)]
#[cfg(feature = "cranelift")]
pub struct CodeGenCtx<'scx, 'tcx, M> {
	scx: &'scx SessionCtx,
	tcx: &'tcx TyCtx<'scx>,

	module: M,
}

// Cranelift
#[cfg(feature = "cranelift")]
impl<'scx, 'tcx> CodeGenCtx<'scx, 'tcx, cranelift_jit::JITModule> {
	pub fn new_jit(scx: &'scx SessionCtx, tcx: &'tcx TyCtx<'scx>) -> Self {
		use ::cranelift::prelude::{Configurable, settings};
		use cranelift_jit::{JITBuilder, JITModule};
		use cranelift_module::default_libcall_names;

		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();
		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();

		let builder = JITBuilder::with_isa(isa, default_libcall_names());
		let module = JITModule::new(builder);

		Self { scx, tcx, module }
	}

	pub fn codegen_root(
		&mut self,
		hir: &hir::Root,
		env: &Environment,
		print_tbir: bool,
		print_bir: bool,
	) -> fn() -> i64 {
		use std::collections::HashMap;

		let mut generator = Generator::new(&self.scx, &mut self.module);

		let mut main_id = None;
		let mut id_map = HashMap::new();

		for item in hir.items {
			match item.kind {
				hir::ItemKind::Extern { ident, decl } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					generator.declare_extern(ident.name, &decl).unwrap();
				}
				hir::ItemKind::Function { ident, decl, body } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					let func_id = generator.declare_function(ident.name, &decl).unwrap();

					id_map.insert(ident.name, func_id.clone());
					if self.scx.symbols.resolve(ident.name) == "main" {
						main_id = Some(func_id.clone());
					}
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
					if print_tbir {
						println!("{body:#?}");
					}
					let func_id = id_map.get(&ident.name).unwrap();
					generator
						.define_function(func_id.clone(), &decl, &body, print_bir)
						.unwrap();
				}
			}
		}

		self.module.finalize_definitions().unwrap();

		let func = self.module.get_finalized_function(main_id.unwrap());
		// TODO: this is unsafe as some functions ask for arguments, and lot a more reasons
		#[allow(unsafe_code)]
		let main = unsafe { std::mem::transmute::<*const u8, fn() -> i64>(func) };

		main
	}
}

#[cfg(feature = "cranelift")]
impl<'scx, 'tcx> CodeGenCtx<'scx, 'tcx, cranelift_object::ObjectModule> {
	pub fn new_object(scx: &'scx SessionCtx, tcx: &'tcx TyCtx<'scx>) -> Self {
		use ::cranelift::prelude::{Configurable, settings};
		use cranelift_module::default_libcall_names;
		use cranelift_object::{ObjectBuilder, ObjectModule};

		let mut flag_builder = settings::builder();
		flag_builder.set("opt_level", "speed_and_size").unwrap();
		let isa = cranelift_native::builder()
			.unwrap()
			.finish(settings::Flags::new(flag_builder))
			.unwrap();
		let mut builder = ObjectBuilder::new(isa, "out", default_libcall_names()).unwrap();
		// builder.per_function_section(per_function_section) what is this?

		let module = ObjectModule::new(builder);
		Self { scx, tcx, module }
	}

	pub fn codegen_root(
		mut self,
		hir: &hir::Root,
		env: &Environment,
		print_tbir: bool,
		print_bir: bool,
	) -> cranelift_object::ObjectProduct {
		use std::collections::HashMap;

		let mut generator = Generator::new(&self.scx, &mut self.module);

		let mut main_id = None;
		let mut id_map = HashMap::new();

		for item in hir.items {
			match item.kind {
				hir::ItemKind::Extern { ident, decl } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					generator.declare_extern(ident.name, &decl).unwrap();
				}
				hir::ItemKind::Function { ident, decl, body } => {
					// TODO: do this elsewhere
					let decl = self.tcx.lower_fn_decl(decl);
					let func_id = generator.declare_function(ident.name, &decl).unwrap();

					id_map.insert(ident.name, func_id.clone());
					if self.scx.symbols.resolve(ident.name) == "main" {
						main_id = Some(func_id.clone());
					}
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
					if print_tbir {
						println!("{body:#?}");
					}
					let func_id = id_map.get(&ident.name).unwrap();
					generator
						.define_function(func_id.clone(), &decl, &body, print_bir)
						.unwrap();
				}
			}
		}

		self.module.finish()
	}
}

// LLVM
#[cfg(feature = "llvm")]
impl CodeGenCtx<_> {
	pub fn new_jit() -> Self {
		#[cfg(feature = "llvm")]
		let context = inkwell::context::Context::create();
		let mut generator = codegen::Generator::new(
			&scx,
			#[cfg(feature = "llvm")]
			&context,
			&mut module,
		);
	}
}
