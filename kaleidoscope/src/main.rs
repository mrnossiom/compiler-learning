//! Kaleidoscope

use std::io::{Write, stdin, stdout};

#[cfg(feature = "llvm")]
use inkwell::context::Context;

use crate::{
	codegen::{CodeGen, Generator},
	parser::{Function, ReplItem},
};

mod codegen;
mod lexer;
mod parser;

#[cfg(not(any(feature = "llvm", feature = "cranelift")))]
compile_error!("You need to choose a backend!");

type Result<T> = std::result::Result<T, &'static str>;

fn main() {
	let mut line = String::new();

	#[cfg(feature = "llvm")]
	let context = Context::create();
	let mut generator = Generator::new(
		#[cfg(feature = "llvm")]
		&context,
	);

	let mut anon_index = 0;

	loop {
		print!("repl> ");
		stdout().flush().unwrap();

		line.clear();
		stdin().read_line(&mut line).unwrap();

		let mut lexer = lexer::Lexer::new(&line);
		let item = match parser::Parser::new(&mut lexer).parse_repl() {
			Ok(item) => item,
			Err(msg) => {
				eprintln!("error: {msg}");
				continue;
			}
		};

		match item {
			ReplItem::Expr(expr) => {
				let func = Function::new_anon(anon_index, expr);
				anon_index += 1;

				let fn_val = generator.function(&func).unwrap();
				println!("{}", generator.call_fn(fn_val).unwrap());
			}
			ReplItem::Definition(function) => {
				generator.function(&function).unwrap();
			}
			ReplItem::Extern(proto) => {
				generator.extern_(&proto).unwrap();
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ident(String);
