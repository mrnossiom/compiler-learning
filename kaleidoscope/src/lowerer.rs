use bumpalo::Bump;

use crate::{
	ast,
	thir::{self, ItemKind, StmtKind},
};

#[derive(Debug)]
pub struct LowerCtx {
	pub arena: Bump,
}

impl LowerCtx {
	pub fn new() -> Self {
		Self { arena: Bump::new() }
	}
}

#[derive(Debug)]
pub struct Lowerer<'lcx> {
	tcx: &'lcx LowerCtx,
}

impl<'lcx> Lowerer<'lcx> {
	pub const fn new(tcx: &'lcx LowerCtx) -> Self {
		Self { tcx }
	}
}

impl<'lcx> Lowerer<'lcx> {
	pub fn lower_items(&'lcx self, ast_items: &'lcx [ast::ItemKind]) -> thir::Thir<'lcx> {
		let mut items = Vec::new();

		for item_kind in ast_items {
			items.push(self.lower_item_kind(item_kind));
		}

		thir::Thir { items }
	}

	fn lower_item_kind(&'lcx self, item: &'lcx ast::ItemKind) -> &'lcx ItemKind<'lcx> {
		let item = match item {
			ast::ItemKind::Function { ident, decl, body } => ItemKind::Function {
				ident: ident.clone(),
				decl: self.lower_fn_decl(decl),
				body: self.lower_fn_body(body),
			},
			ast::ItemKind::Extern { ident, decl } => ItemKind::Extern {
				ident: ident.clone(),
				decl: self.lower_fn_decl(decl),
			},
		};
		self.tcx.arena.alloc(item)
	}

	fn lower_fn_decl(&'lcx self, decl: &'lcx ast::FnDecl) -> &'lcx thir::FnDecl<'lcx> {
		let inputs = decl.args.iter().map(|(_ident, ty)| ty.to_owned());
		self.tcx.arena.alloc(thir::FnDecl {
			inputs: self.tcx.arena.alloc_slice_fill_iter(inputs),
			output: &decl.ret,
		})
	}

	fn lower_fn_body(&'lcx self, body: &'lcx ast::Block) -> &'lcx thir::Block<'lcx> {
		let mut stmts = Vec::new();
		let mut ret_expr = None;

		let mut ast_stmts = &body.stmts[..];
		while let [stmt, tail @ ..] = ast_stmts {
			ast_stmts = tail;

			let stmt_kind = match &**stmt {
				ast::StmtKind::Loop { body } => todo!(),
				// desugar to simple loop
				ast::StmtKind::WhileLoop { check, body } => todo!(),
				ast::StmtKind::ForLoop { pat, iter, body } => todo!(),

				ast::StmtKind::Let { name, ty, value } => StmtKind::Let {
					name: name.clone(),
					value: self.lower_expr(value),
					ty,
				},
				ast::StmtKind::Assign { target, value } => StmtKind::Assign {
					target: target.clone(),
					value: self.lower_expr(value),
				},

				ast::StmtKind::Expr(expr) => {
					let expr = self.lower_expr(expr);
					StmtKind::Expr(expr)
				}

				ast::StmtKind::ExprRet(expr) => {
					let expr = self.lower_expr(expr);
					if tail.is_empty() {
						ret_expr = Some(expr);
						continue;
					}
					todo!("accept otherwise? or error?")
				}

				ast::StmtKind::Empty => continue,
			};

			stmts.push(thir::Stmt { kind: stmt_kind });
		}

		self.tcx.arena.alloc(thir::Block {
			stmts: self.tcx.arena.alloc(stmts),
			ret_expr,
		})
	}

	fn lower_expr(&'lcx self, expr: &ast::ExprKind) -> &'lcx thir::Expr {
		let kind = match expr {
			ast::ExprKind::Variable(ident) => thir::ExprKind::Variable(ident.clone()),
			ast::ExprKind::Literal(lit) => thir::ExprKind::Literal(lit.clone()),
			ast::ExprKind::Binary { op, left, right } => todo!(),
			ast::ExprKind::FnCall { expr, args } => todo!(),
			ast::ExprKind::If {
				condition,
				consequence,
				alternative,
			} => todo!(),
		};
		self.tcx.arena.alloc(thir::Expr {
			ty: ast::TyKind::Infer,
			kind,
		})
	}
}
