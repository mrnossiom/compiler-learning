use crate::{Result, session::Symbol, tbir, ty};
#[cfg(feature = "cranelift")]
use crate::{hir, resolve::Environment};

#[cfg(feature = "cranelift")]
mod cranelift;
#[cfg(feature = "llvm")]
mod llvm;

#[cfg(feature = "cranelift")]
pub use self::cranelift::Generator as CraneliftBackend;
#[cfg(feature = "llvm")]
pub use self::llvm::Generator as LlvmBackend;

pub trait Backend {
	type FuncId;

	fn codegen_root(&mut self, hir: &hir::Root, env: &Environment);

	fn declare_extern(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()>;
	fn declare_function(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<Self::FuncId>;

	fn define_function(
		&mut self,
		func_id: Self::FuncId,
		decl: &ty::FnDecl,
		body: &tbir::Block,
	) -> Result<()>;
}
