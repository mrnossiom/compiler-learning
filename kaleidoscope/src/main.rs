use std::path::PathBuf;

use clap::Parser;
use kaleidoscope::{
	codegen::{self, CodeGen},
	hir, lowerer, parser, session, ty,
};

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
	let scx = session::SessionCtx::new();

	// parsing source
	let ast = parser::Parser::new(&scx, source).parse_file().unwrap();
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
	let mut tcx = ty::TyCtx::new(&scx);
	let item_env = tcx.collect_hir(&hir);
	tcx.infer_root(&hir, &item_env);

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = Context::create();
	let mut generator = codegen::Generator::new(
		&scx,
		#[cfg(feature = "llvm")]
		&context,
	);

	for item in hir.items {
		match item.kind {
			hir::ItemKind::Extern { ident, decl } => {
				generator.extern_(ident.name, decl).unwrap();
			}
			hir::ItemKind::Function { ident, decl, body } => {
				let fn_ = generator.function(ident.name, decl, body).unwrap();
				dbg!(generator.call_fn(fn_).unwrap());
			}
		}
	}

	println!("Reached pipeline end sucessfully!");
}
