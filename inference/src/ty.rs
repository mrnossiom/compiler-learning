use std::{
	cell::Cell,
	sync::atomic::{self, AtomicU64},
};

use crate::ir;

pub enum TyKind {
	String,
	Number,

	Bool,

	Infer,

	Unit,

	Custom(ir::Ident),
}

pub enum TyOrFresh {
	Fresh(u64),
	Concrete(TyKind),
}

impl TyOrFresh {
	fn new_fresh(ident: &ir::Ident) -> Self {
		static COUNTER: AtomicU64 = AtomicU64::new(0);
		let fetch_add = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
		println!("{} is {}", fetch_add, &ident.0);
		Self::Fresh(fetch_add)
	}
}

pub struct Constraint(TyOrFresh, TyOrFresh);

impl Constraint {
	fn new(left: TyOrFresh, right: TyOrFresh) -> Self {
		Self(left, right)
	}
}

pub fn collect_constraints(func: &ir::Function) -> Vec<Constraint> {
	let mut constraints = Vec::new();

	let ret_ty = func
		.ret_type
		.as_ref()
		.map(get_ty_ty)
		.unwrap_or(TyKind::Unit);

	for (arg, ty) in &func.params {
		constraints.push(Constraint::new(
			TyOrFresh::new_fresh(arg),
			TyOrFresh::Concrete(get_ty_ty(ty)),
		));
	}

	for stmt in &func.body.stmts {
		// collect stmts constraint
	}

	constraints
}

fn get_ty_ty(ty: &ir::TyKind) -> TyKind {
	match ty {
		ir::TyKind::Ident(ident) => match &*ident.0 {
			"_" => TyKind::Infer,

			"str" => TyKind::String,
			"num" => TyKind::Number,
			"bool" => TyKind::Bool,
			"()" => TyKind::Unit,

			_ => TyKind::Custom(ident.clone()),
		},
	}
}
