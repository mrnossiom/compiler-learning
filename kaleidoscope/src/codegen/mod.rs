#[cfg(feature = "cranelift")]
mod cranelift;
#[cfg(feature = "llvm")]
mod llvm;

#[cfg(feature = "cranelift")]
pub use cranelift::CodeGen;
#[cfg(feature = "llvm")]
pub use llvm::CodeGen;
