use crate::{hir, resolve::Environment};

#[cfg(feature = "cranelift")]
mod cranelift;
#[cfg(feature = "llvm")]
mod llvm;

#[cfg(feature = "cranelift")]
pub use self::cranelift::Generator as CraneliftBackend;
#[cfg(feature = "llvm")]
pub use self::llvm::Generator as LlvmBackend;

#[derive(Debug)]
pub enum AvailableBackend {
	#[cfg(feature = "cranelift")]
	Cranelift,
	#[cfg(feature = "llvm")]
	Llvm,
}

impl Default for AvailableBackend {
	fn default() -> Self {
		#[cfg(feature = "cranelift")]
		return Self::Cranelift;
		#[cfg(feature = "llvm")]
		return Self::Llvm;
	}
}

pub trait Backend {
	fn codegen_root(&mut self, hir: &hir::Root, env: &Environment);
}

pub trait JitBackend: Backend {
	fn call_main(&mut self);
}

pub trait ObjectBackend: Backend {
	// TODO: change to common object
	fn get_object(self) -> cranelift_object::ObjectProduct;
}
