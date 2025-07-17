use std::path::PathBuf;

use clap::Parser;
use kaleidoscope::{front, hir, lowerer, parser, ty};

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
	let fcx = front::FrontCtx::new();

	// parsing source
	let ast = parser::Parser::new(&fcx, source).parse_file().unwrap();
	if args.p_ast {
		println!("{ast:#?}");
	}

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let hir = lowerer.lower_items(&ast);
	if args.p_hir {
		println!("{hir:#?}");
	}

	// type collection, inference and analysis
	let tcx = ty::TyCtx::new(&fcx);

	// TODO: HIR type collection
	let ty_collector = ty::Collector::new(&tcx);

	// TODO: HIR typeck
	let inferer = ty::Inferer::new(&tcx);
	for item in hir.items {
		match item {
			hir::ItemKind::Extern { ident, decl } => {}
			hir::ItemKind::Function { ident, decl, body } => {
				inferer.infer_fn(decl, body);
			}
		}
	}

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = Context::create();
	// let mut generator = Generator::new(
	// 	#[cfg(feature = "llvm")]
	// 	&context,
	// );
}
