//! Kaleidoscope

use std::{
	collections::HashMap,
	io::{Write, stdin, stdout},
};

use parser::Prototype;

use crate::parser::{Function, ReplItem};

mod codegen;
mod lexer;
mod parser;

fn main() {
	// let path = args_os().nth(1).unwrap();
	// let content = std::fs::read_to_string(path).unwrap();

	let mut binop_precedence = HashMap::new();
	binop_precedence.insert('<', 10);
	binop_precedence.insert('+', 20);
	binop_precedence.insert('-', 20);
	binop_precedence.insert('*', 40);

	let mut line = String::new();

	// let ctx = Context::create();
	// let mut codegen = codegen::CodeGen::new(&ctx);

	loop {
		print!("repl> ");
		stdout().flush().unwrap();

		line.clear();
		stdin().read_line(&mut line).unwrap();

		let mut lexer = lexer::Lexer::new(&line);
		let item = match parser::Parser::new(&mut lexer, &mut binop_precedence).parse_repl() {
			Ok(item) => item,
			Err(msg) => {
				eprintln!("error: {msg}");
				continue;
			}
		};

		match item {
			ReplItem::Expr(expr) => {
				let fn_ = Function {
					proto: Prototype {
						name: Ident("anon".into()),
						args: Vec::new(),
					},
					body: expr,
				};
				// let fn_val = codegen.compile_fn(&fn_).unwrap();
				// fn_val.print_to_stderr();
				// unsafe { fn_val.delete() }
			}
			ReplItem::Definition(function) => {
				// let fn_val = codegen.compile_fn(&function).unwrap();
				// codegen.apply_passes();
				// fn_val.print_to_stderr();
			}
			ReplItem::Extern(proto) => {
				// let fn_val = codegen.compile_prototype(&proto).unwrap();
				// codegen.apply_passes();
				// fn_val.print_to_stderr();
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ident(String);
