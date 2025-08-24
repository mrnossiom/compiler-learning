use std::{collections::HashMap, mem};

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

	environment: Environment,
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
	pub fn collect_items(&mut self, hir: &hir::Root) {
		tracing::trace!("collect_items");
		for item in &hir.items {
			self.collect_item(item);
		}

		self.tcx
			.environment
			.replace(Some(mem::take(&mut self.environment)));
	}

	fn collect_item(&mut self, item: &hir::Item) {
		match &item.kind {
			hir::ItemKind::Function { name, decl, .. }
			| hir::ItemKind::Extern { name, decl, .. } => {
				let decl = self.tcx.lower_fn_decl(decl);
				self.environment
					.values
					.insert(name.sym, ty::TyKind::Fn(Box::new(decl)));
			}

			hir::ItemKind::Adt { .. } | hir::ItemKind::Trait { .. } => todo!(),
		}
	}
}
