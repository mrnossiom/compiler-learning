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
	type Fn;

	fn extern_(&mut self, name: Symbol, decl: &ty::FnDecl) -> Result<()>;
	fn function(&mut self, name: Symbol, decl: &ty::FnDecl, body: &tbir::Block)
	-> Result<Self::Fn>;

	fn call_fn(&mut self, func: Self::Fn) -> Result<i64>;
}
