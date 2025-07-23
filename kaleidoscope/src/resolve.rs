use std::collections::HashMap;

use crate::{hir, session::Symbol, ty};

#[derive(Debug)]
pub enum Namespace {
	Type,
	Value,
}

#[derive(Debug, Default)]
pub struct Environment {
	// TODO
	// types: HashMap<Symbol, ...>,
	pub values: HashMap<Symbol, ty::TyKind>,
}

#[derive(Debug)]
pub struct Collector<'tcx> {
	tcx: &'tcx ty::TyCtx<'tcx>,

	pub environment: Environment,
}

impl<'tcx> Collector<'tcx> {
	#[must_use]
	pub fn new(tcx: &'tcx ty::TyCtx) -> Self {
		Self {
			tcx,
			environment: Environment::default(),
		}
	}
}

impl Collector<'_> {
	pub fn collect_hir(&mut self, hir: &hir::Root) {
		for item in hir.items {
			self.collect_item(item);
		}
	}

	fn collect_item(&mut self, item: &hir::Item<'_>) {
		match &item.kind {
			hir::ItemKind::Function { ident, decl, .. } | hir::ItemKind::Extern { ident, decl } => {
				let decl = self.tcx.lower_fn_decl(decl);
				self.environment
					.values
					.insert(ident.name, ty::TyKind::Fn(Box::new(decl)));
			}
		}
	}
}
