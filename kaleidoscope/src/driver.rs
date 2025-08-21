use ariadne::ReportKind;

use crate::{
	codegen::{self, Backend, CodeGenBackend, JitBackend, ObjectBackend},
	lowerer, parser, resolve,
	session::{Diagnostic, OutputKind, PrintKind, Report, SessionCtx, Span},
	ty,
};

pub fn pipeline(scx: &SessionCtx) {
	let filename = scx.options.input.as_ref().unwrap_or_else(|| {
		let report = Report::build(ReportKind::Error, Span::DUMMY)
			.with_message("expected an input filename");
		scx.dcx().emit_fatal(&Diagnostic::new(report))
	});

	let source = scx
		.source_map
		.write()
		.load_source_from_file(filename)
		.unwrap();

	// parsing source
	let mut parser = parser::Parser::new(scx, &source);
	let ast = match parser.parse_root() {
		Ok(ast) => ast,
		Err(diag) => scx.dcx().emit_fatal(&diag),
	};
	if scx.options.print.contains(&PrintKind::Ast) {
		println!("{ast:#?}");
	}

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new(scx);
	let hir = lcx.lower_root(&ast);
	if scx.options.print.contains(&PrintKind::HigherIr) {
		println!("{hir:#?}");
	}

	// type collection, inference and analysis
	let tcx = ty::TyCtx::new(scx);

	let mut cltr = resolve::Collector::new(&tcx);
	cltr.collect_items(&hir);

	// lower HIR bodies to TBIR
	// codegen TBIR bodies
	match &scx.options.output {
		OutputKind::Jit => {
			let backend: &mut dyn JitBackend = match scx.options.backend {
				#[cfg(feature = "cranelift")]
				Backend::Cranelift => &mut codegen::CraneliftBackend::new_jit(&tcx),
				#[cfg(feature = "llvm")]
				Backend::Llvm => &mut codegen::LlvmBackend::new_jit(&tcx),
			};

			backend.codegen_root(&hir);
			backend.call_main();
		}
		OutputKind::Object(path) => {
			let mut backend = match scx.options.backend {
				#[cfg(feature = "cranelift")]
				Backend::Cranelift => codegen::CraneliftBackend::new_object(&tcx),
				#[cfg(feature = "llvm")]
				Backend::Llvm => todo!("no object backend for llvm"),
			};

			backend.codegen_root(&hir);

			let object = backend.get_object();
			let bytes = object.emit().unwrap();
			std::fs::write(path, bytes).unwrap();
		}
	}

	tracing::info!("Reached pipeline end successfully!");
}
