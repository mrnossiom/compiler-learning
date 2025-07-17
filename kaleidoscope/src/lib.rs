//! # Kaleidoscope

#[cfg(feature = "llvm")]
use inkwell::context::Context;

// pub mod codegen;
pub mod front;
pub mod lexer;
pub mod lowerer;
pub mod parser;
pub mod ty;

// IRs
pub mod ast;
pub mod hir;
pub mod tbir;

pub mod ffi;

#[cfg(any(
	not(any(feature = "llvm", feature = "cranelift")),
	all(feature = "llvm", feature = "cranelift")
))]
compile_error!("You need to choose a single backend!");

// type Result<T> = std::result::Result<T, &'static str>;
