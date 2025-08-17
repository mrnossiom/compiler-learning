use std::{path::PathBuf, process};

use clap::Parser;
use kaleic::{
	codegen::{Backend, CraneliftBackend},
	lowerer, parser, resolve,
	session::{self, SessionCtx},
	ty,
};
use tracing_subscriber::{EnvFilter, FmtSubscriber, fmt::time};

#[derive(clap::Parser)]
struct Args {
	pub input: Option<PathBuf>,
	#[clap(long)]
	pub output: Option<PathBuf>,

	#[clap(long)]
	pub jit: bool,

	/// Valid values are ast, hir, tbir, bir
	#[clap(long)]
	pub print: Vec<String>,
}

fn main() {
	FmtSubscriber::builder()
		.with_env_filter(EnvFilter::from_default_env())
		.with_timer(time::Uptime::default())
		.with_writer(std::io::stderr)
		.init();

	let args = Args::parse();

	let mut scx = session::SessionCtx::default();
	scx.options.input = args.input;
	scx.options.output = args.output;
	scx.options.jit = args.jit;
	scx.options.print.extend(args.print);

	pipeline(&scx);
}

fn pipeline(scx: &SessionCtx) {
	let filename = scx.options.input.as_ref().map_or_else(
		|| {
			eprintln!("You did not provide a filename!");
			process::exit(1);
		},
		|path| path,
	);

	let source = scx
		.source_map
		.write()
		.load_source_from_file(filename)
		.unwrap();

	// parsing source
	let mut parser = parser::Parser::new(scx, &source);
	let ast = match parser.parse_root() {
		Ok(ast) => ast,
		Err(diag) => scx.emit_fatal_diagnostic(&diag),
	};
	if scx.options.print.contains("ast") {
		println!("{ast:#?}");
	}

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let hir = lcx.lower_root(&ast);
	if scx.options.print.contains("hir") {
		println!("{hir:#?}");
	}

	// type collection, inference and analysis
	let tcx = ty::TyCtx::new(scx);

	let mut cltr = resolve::Collector::new(&tcx);
	cltr.collect_items(&hir);

	// lower HIR bodies to TBIR
	// codegen TBIR bodies
	if scx.options.jit {
		let mut cgcx = CraneliftBackend::new_jit(&tcx);
		cgcx.codegen_root(&hir, &cltr.environment);

		let main = cgcx.get_output();
		let fn_ret = main();
		tracing::debug!(fn_ret);
	} else {
		let mut cgcx = CraneliftBackend::new_object(&tcx);
		cgcx.codegen_root(&hir, &cltr.environment);

		let object = cgcx.get_output();
		let bytes = object.emit().unwrap();
		std::fs::write(scx.options.output.as_ref().unwrap(), bytes).unwrap();
	}

	tracing::info!("Reached pipeline end successfully!");
}
