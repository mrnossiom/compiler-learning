use std::path::PathBuf;

use clap::Parser;
use kaleic::{codegen::CodeGenCtx, lowerer, parser, resolve, session, ty};
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time};

#[expect(clippy::struct_excessive_bools)]
#[derive(clap::Parser)]
struct Args {
	pub path: PathBuf,

	#[clap(long)]
	pub jit: bool,

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

	// lower HIR bodies to TBIR
	// codegen TBIR bodies
	if args.jit {
		let mut cgcx = CodeGenCtx::new_jit(&scx, &tcx);
		let main = cgcx.codegen_root(&hir, &cltr.environment, args.print_tbir, args.print_bir);
		let fn_ret = main();
		tracing::debug!(fn_ret);
	} else {
		let mut cgcx = CodeGenCtx::new_object(&scx, &tcx);
		let object = cgcx.codegen_root(&hir, &cltr.environment, args.print_tbir, args.print_bir);
		let bytes = object.emit().unwrap();
		std::fs::write("./out.o", bytes).unwrap();
	}

	tracing::info!("Reached pipeline end successfully!");
}
