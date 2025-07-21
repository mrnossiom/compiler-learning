use kaleidoscope::{lowerer, parser, session};

#[test]
fn fibo() {
	let source = include_str!("../samples/fibonacci.txt");

	let fcx = session::SessionCtx::new();

	let ast = parser::Parser::new(&fcx, source).parse_file().unwrap();
	insta::assert_debug_snapshot!(ast);

	// lowering to HIR
	let lcx = lowerer::LowerCtx::new();
	let lowerer = lowerer::Lowerer::new(&lcx);
	let hir = lowerer.lower_items(&ast);
	insta::assert_debug_snapshot!(hir);
}
