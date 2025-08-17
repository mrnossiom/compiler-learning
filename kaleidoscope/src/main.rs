use std::{collections::HashMap, path::PathBuf};

use clap::Parser;
use kaleic::{
	codegen::{self, CodeGen},
	hir, lowerer, parser, resolve, session, ty,
};
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time};

#[expect(clippy::struct_excessive_bools)]
#[derive(clap::Parser)]
struct Args {
	pub path: PathBuf,

	#[clap(long)]
	pub print_ast: bool,
	#[clap(long)]
	pub print_hir: bool,
	#[clap(long)]
	pub print_tbir: bool,
	/// Print Backend IR
	#[clap(long)]
	pub print_bir: bool,
}

fn main() {
	FmtSubscriber::builder()
		.with_env_filter(EnvFilter::from_default_env())
		.with_timer(time::Uptime::default())
		.with_writer(std::io::stderr)
		.init();

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
	let mut parser = parser::Parser::new(&scx, &source);
	let ast = match parser.parse_root() {
		Ok(ast) => ast,
		Err(diag) => scx.emit_fatal_diagnostic(&diag),
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
	let tcx = ty::TyCtx::new(&scx);

	let mut cltr = resolve::Collector::new(&tcx);
	cltr.collect_items(&hir);

	// TODO: lower HIR bodies to TBIR

	// codegen TBIR bodies
	#[cfg(feature = "llvm")]
	let context = inkwell::context::Context::create();
	let mut generator = codegen::Generator::new(
		&scx,
		#[cfg(feature = "llvm")]
		&context,
	);

	let mut main_id = None;
	let mut id_map = HashMap::new();

	for item in hir.items {
		match item.kind {
			hir::ItemKind::Extern { ident, decl } => {
				// TODO: do this elsewhere
				let decl = tcx.lower_fn_decl(decl);
				generator.declare_extern(ident.name, &decl).unwrap();
			}
			hir::ItemKind::Function { ident, decl, body } => {
				// TODO: do this elsewhere
				let decl = tcx.lower_fn_decl(decl);

				let body = tcx.typeck_fn(ident, &decl, body, &cltr.environment);
				if args.print_tbir {
					println!("{body:#?}");
				}
				let func_id = generator.declare_function(ident.name, &decl).unwrap();

				id_map.insert(ident.name, func_id.clone());
				if scx.symbols.resolve(ident.name) == "main" {
					main_id = Some(func_id.clone());
				}
			}
		}
	}
	for item in hir.items {
		match item.kind {
			hir::ItemKind::Extern { .. } => {}
			hir::ItemKind::Function { ident, decl, body } => {
				// TODO: do this elsewhere
				let decl = tcx.lower_fn_decl(decl);

				let body = tcx.typeck_fn(ident, &decl, body, &cltr.environment);
				if args.print_tbir {
					println!("{body:#?}");
				}
				let func_id = id_map.get(&ident.name).unwrap();
				generator
					.define_function(func_id.clone(), &decl, &body, args.print_bir)
					.unwrap();
			}
		}
	}

	#[allow(unsafe_code)]
	let fn_ret = unsafe {
		generator
			.call_fn_as_main(main_id.expect("no main func"))
			.unwrap()
	};

	tracing::debug!(fn_ret);

	tracing::info!("Reached pipeline end successfully!");
}
