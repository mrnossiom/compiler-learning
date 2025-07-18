//! # Kaleidoscope

use std::path::PathBuf;

use clap::Parser;
#[cfg(feature = "llvm")]
use inkwell::context::Context;

// mod codegen;
mod lexer;
mod lowerer;
mod parser;
mod ty;

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

	#[clap(long)]
	pub p_ast: bool,
	#[clap(long)]
	pub p_hir: bool,
}

fn main() {
	let args = Args::parse();

	let source = std::fs::read_to_string(&args.path).unwrap();

	pipeline(&args, &source);
}

fn pipeline(args: &Args, source: &str) {
	// parsing source
	let ast = parser::Parser::new(source).parse_file().unwrap();
	insta::assert_debug_snapshot!(ast);
	if args.p_ast {
		println!("{ast:#?}");
	}

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let hir = lowerer.lower_items(&ast);
	insta::assert_debug_snapshot!(hir);
	if args.p_hir {
		println!("{hir:#?}");
	}

	let tcx = ty::TyCtx::new();
	for item in hir.items {
		match item {
			hir::ItemKind::Extern { ident, decl } => {}
			hir::ItemKind::Function { ident, decl, body } => {
				tcx.infer_fn(decl, body);
			}
		}
	}

	// TODO: HIR type collection
	// TODO: HIR typeck

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );
}
