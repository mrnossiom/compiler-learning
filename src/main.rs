use std::env::args_os;

fn main() {
	println!("Hello, world!");

	let path = args_os().nth(1).unwrap();
	let content = std::fs::read_to_string(path).unwrap();

	let tokens = lexer::lex(&content);
	println!("{:?}", &tokens);

	let ast = parser::parse(tokens);
	println!("{:?}", &ast);
}

#[derive(Debug)]
struct Ident(String);

mod lexer;
mod parser;
