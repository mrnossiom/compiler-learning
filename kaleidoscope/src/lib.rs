//! # Kaleidoscope

pub mod codegen;
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
