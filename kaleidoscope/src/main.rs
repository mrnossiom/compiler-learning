//! # Kaleidoscope

use std::path::PathBuf;

use clap::Parser;
#[cfg(feature = "llvm")]
use inkwell::context::Context;

// mod codegen;
mod lexer;
mod lowerer;
mod parser;

// IRs
mod ast;
mod hir;
mod tbir;

mod ffi;

#[cfg(any(
	not(any(feature = "llvm", feature = "cranelift")),
	all(feature = "llvm", feature = "cranelift")
))]
compile_error!("You need to choose a single backend!");

// type Result<T> = std::result::Result<T, &'static str>;

#[derive(clap::Parser)]
struct Args {
	pub path: PathBuf,
}

fn main() {
	let args = Args::parse();

	let content = std::fs::read_to_string(args.path).unwrap();

	// parsing source
	let ast = parser::Parser::new(&content).parse_file().unwrap();
	dbg!(&ast);

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let hir = lowerer.lower_items(&ast);
	dbg!(&hir);

	// TODO: type collection and typeck HIR

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );
}
