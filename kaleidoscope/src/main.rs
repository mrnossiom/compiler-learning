//! Kaleidoscope

use std::{
	collections::HashMap,
	io::{Write, stdin, stdout},
};

use cranelift_module::Linkage;

use crate::{
	codegen::Generator,
	parser::{Function, Prototype, ReplItem},
};

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

	let mut generator = Generator::new();

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
				let fn_val = generator.function(&fn_).unwrap();
				println!("{}", fn_val());
			}
			ReplItem::Definition(function) => {
				generator.function(&function).unwrap();
			}
			ReplItem::Extern(proto) => {
				generator.prototype(&proto, Linkage::Import).unwrap();
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Ident(String);
