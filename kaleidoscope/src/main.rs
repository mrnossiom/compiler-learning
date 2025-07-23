use std::path::PathBuf;

use clap::Parser;
use kaleidoscope::{
	codegen::{self, CodeGen},
	hir, lowerer, parser, resolve, session, ty,
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
	let scx = session::SessionCtx::default();

	let source = scx
		.source_map
		.write()
		.load_source("entry", source.to_owned());

	// parsing source
	let ast = match parser::Parser::new(&scx, &source).parse_file() {
		Ok(ast) => ast,
		Err(diag) => {
			scx.emit_diagnostic(&diag);
			todo!()
		}
	};
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

	let mut cltr = resolve::Collector::new(&tcx);
	cltr.collect_hir(&hir);

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
				// TODO: do this elsewhere
				let decl = tcx.lower_fn_decl(decl);
				generator.extern_(ident.name, &decl).unwrap();
			}
			hir::ItemKind::Function { ident, decl, body } => {
				// TODO: do this elsewhere
				let decl = tcx.lower_fn_decl(decl);

				let body = tcx.typeck_fn(&decl, body, &cltr.environment);
				let fn_ = generator.function(ident.name, &decl, &body).unwrap();
				dbg!(generator.call_fn(fn_).unwrap());
			}
		}
	}

	println!("Reached pipeline end sucessfully!");
}
