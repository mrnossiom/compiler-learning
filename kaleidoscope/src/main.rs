//! Kaleidoscope

use std::path::PathBuf;

use clap::Parser;
#[cfg(feature = "llvm")]
use inkwell::context::Context;

// mod codegen;
mod lexer;
mod lowerer;
mod parser;

// type definition
mod ast;
mod thir;

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

	let ast = parser::Parser::new(&content).parse_file().unwrap();

	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let thir = lowerer.lower_items(&ast);

	dbg!(thir);

	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ident(String);
