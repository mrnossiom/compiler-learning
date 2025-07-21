use std::path::PathBuf;

use clap::Parser;
use kaleidoscope::{lowerer, parser, session, ty};

#[derive(clap::Parser)]
struct Args {
	pub path: PathBuf,

	#[clap(long)]
	pub print_ast: bool,
	#[clap(long)]
	pub print_hir: bool,
}

fn main() {
	let args = Args::parse();

	let source = std::fs::read_to_string(&args.path).unwrap();

	pipeline(&args, &source);
}

fn pipeline(args: &Args, source: &str) {
	let fcx = session::SessionCtx::new();

	// parsing source
	let ast = parser::Parser::new(&fcx, source).parse_file().unwrap();
	if args.print_ast {
		println!("{ast:#?}");
	}

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let hir = lcx.lower_root(&ast);
	if args.print_hir {
		println!("{hir:#?}");
	}

	// type collection, inference and analysis
	let mut tcx = ty::TyCtx::new(&fcx);
	let item_env = tcx.collect_hir(&hir);
	tcx.infer_root(&hir, &item_env);

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );

	println!("Reached pipeline end sucessfully!");
}
