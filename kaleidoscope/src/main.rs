//! Kaleidoscope

use std::io::{Write, stdout};
use std::path::PathBuf;

use clap::Parser;
#[cfg(feature = "llvm")]
use inkwell::context::Context;

use crate::{
	// codegen::{CodeGen, Generator},
	parser::{Function, ItemKind},
};

// mod codegen;
mod lexer;
mod parser;

#[cfg(any(
	not(any(feature = "llvm", feature = "cranelift")),
	all(feature = "llvm", feature = "cranelift")
))]
compile_error!("You need to choose a single backend!");

type Result<T> = std::result::Result<T, &'static str>;

#[derive(clap::Parser)]
struct Args {
	pub path: PathBuf,
}

fn main() {
	let args = Args::parse();

	let content = std::fs::read_to_string(args.path).unwrap();

	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );

	let lexer = lexer::Lexer::new(&content);
	let ast = parser::Parser::new(lexer).parse_file().unwrap();
	dbg!(&ast);

	// let thir = thir::Compiler::new(ast).parse_file().unwrap();

	// 	match item {
	// 		ReplItem::Expr(expr) => {
	// 			let func = Function::new_anon(anon_index, expr);
	// 			anon_index += 1;

	// 			let fn_val = generator.function(&func).unwrap();
	// 			println!("{}", generator.call_fn(fn_val).unwrap());
	// 		}
	// 		ReplItem::Definition(function) => {
	// 			generator.function(&function).unwrap();
	// 		}
	// 		ReplItem::Extern(proto) => {
	// 			generator.extern_(&proto).unwrap();
	// 		}
	// 	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ident(String);

#[allow(unsafe_code)]
#[unsafe(no_mangle)]
extern "C" fn putchard(i: i32) {
	print!("{}", u8::try_from(i).unwrap() as char);
	stdout().flush().unwrap();
}
