use kaleic::{lowerer, parser, session};

#[test]
fn fibo() {
	let source = include_str!("../samples/fibonacci.txt");

	let scx = session::SessionCtx::default();

	let source = scx.source_map.write().load_source("entry", source.into());

	let ast = parser::Parser::new(&scx, &source).parse_file().unwrap();
	insta::assert_debug_snapshot!(ast);

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let hir = lowerer.lower_items(&ast);
	insta::assert_debug_snapshot!(hir);
}
