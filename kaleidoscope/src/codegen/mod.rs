use crate::{Result, session::Symbol, tbir, ty};

#[cfg(feature = "cranelift")]
mod cranelift;
#[cfg(feature = "llvm")]
mod llvm;

#[cfg(feature = "cranelift")]
pub use cranelift::Generator;
#[cfg(feature = "llvm")]
pub use llvm::Generator;

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

	#[allow(unsafe_code)]
	unsafe fn call_fn_as_main(&mut self, func_id: Self::FuncId) -> Result<i64>;
}
