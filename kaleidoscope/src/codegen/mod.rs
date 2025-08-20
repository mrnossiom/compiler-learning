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
pub enum Backend {
	#[cfg(feature = "cranelift")]
	Cranelift,
	#[cfg(feature = "llvm")]
	Llvm,
}

impl Default for Backend {
	fn default() -> Self {
		#[cfg(feature = "cranelift")]
		return Self::Cranelift;
		#[cfg(feature = "llvm")]
		return Self::Llvm;
	}
}

pub trait CodeGenBackend {
	fn codegen_root(&mut self, hir: &hir::Root, env: &Environment);
}

pub trait JitBackend: CodeGenBackend {
	fn call_main(&mut self);
}

pub trait ObjectBackend: CodeGenBackend {
	// TODO: change to common object
	fn get_object(self) -> cranelift_object::ObjectProduct;
}
