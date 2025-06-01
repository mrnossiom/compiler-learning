use crate::{
	Result,
	parser::{Function, Prototype},
};

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
	fn extern_(&mut self, prototype: &Prototype) -> Result<()>;
	fn function(&mut self, func: &Function) -> Result<Self::Fn>;
	fn call_fn(&mut self, func: Self::Fn) -> Result<i64>;
}
