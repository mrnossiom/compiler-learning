mod ir;
mod program;
mod ty;

use crate::program::fibo;

fn main() {
	let func = fibo();
	infer_types(func);
}

fn infer_types(func: ir::Function) {}
