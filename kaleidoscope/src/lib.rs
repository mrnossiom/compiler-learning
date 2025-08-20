//! # Kaleidoscope

pub mod codegen;
pub mod driver;
pub mod lexer;
pub mod lowerer;
pub mod parser;
pub mod resolve;
pub mod session;
pub mod ty;

// IRs
pub mod ast;
pub mod hir;
pub mod tbir;

pub mod ffi;

type Result<T> = std::result::Result<T, &'static str>;

/// Used when reaching a branch that breaks an assumption made
#[macro_export]
macro_rules! bug {
	($msg:literal) => {
		panic!(concat!("bug triggered: ", $msg))
	};
}
