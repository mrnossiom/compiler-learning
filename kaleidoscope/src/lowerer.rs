//! AST to HIR lowering logic

use bumpalo::Bump;

use crate::{
	ast,
	hir::{Block, Expr, ExprKind, FnDecl, Hir, ItemKind, Stmt, StmtKind},
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

	fn alloc<T>(&self, t: T) -> &'lcx T {
		self.tcx.arena.alloc(t)
	}

	fn alloc_iter<T, I>(&self, i: I) -> &'lcx [T]
	where
		I: IntoIterator<Item = T>,
		I::IntoIter: ExactSizeIterator,
	{
		self.tcx.arena.alloc_slice_fill_iter(i)
	}

	const fn mk_expr(&self, expr: ExprKind<'lcx>) -> Expr<'lcx> {
		Expr { kind: expr }
	}

	fn mk_expr_block(&self, expr: &'lcx Expr<'lcx>) -> Block<'lcx> {
		Block {
			stmts: self.alloc([]),
			ret_expr: Some(expr),
		}
	}
}

impl<'lcx> Lowerer<'lcx> {
	pub fn lower_items(&'lcx self, ast_items: &'lcx [ast::ItemKind]) -> Hir<'lcx> {
		let items = ast_items
			.iter()
			.map(|item_kind| self.lower_item_kind(item_kind));

		Hir {
			items: self.alloc_iter(items),
		}
	}

	fn lower_item_kind(&'lcx self, item: &'lcx ast::ItemKind) -> ItemKind<'lcx> {
		match item {
			ast::ItemKind::Function { ident, decl, body } => ItemKind::Function {
				ident: ident.clone(),
				decl: self.lower_fn_decl(decl),
				body: self.alloc(self.lower_block(body)),
			},
			ast::ItemKind::Extern { ident, decl } => ItemKind::Extern {
				ident: ident.clone(),
				decl: self.lower_fn_decl(decl),
			},
		}
	}

	fn lower_fn_decl(&'lcx self, decl: &'lcx ast::FnDecl) -> &'lcx FnDecl<'lcx> {
		let inputs = decl.args.iter().cloned().map(|(ident, ty)| (ident, ty));
		self.tcx.arena.alloc(FnDecl {
			inputs: self.tcx.arena.alloc_slice_fill_iter(inputs),
			output: &decl.ret,
		})
	}

	fn lower_block(&'lcx self, body: &'lcx ast::Block) -> Block<'lcx> {
		let mut stmts = Vec::new();
		let mut ret_expr = None;

		let mut ast_stmts = &body.stmts[..];
		while let [stmt, tail @ ..] = ast_stmts {
			ast_stmts = tail;

			let stmt_kind = match stmt {
				ast::StmtKind::Loop { body } => todo!(),
				// desugar to simple loop
				ast::StmtKind::WhileLoop { check, body } => self.lower_while_loop(check, body),
				ast::StmtKind::ForLoop { pat, iter, body } => todo!(),

				ast::StmtKind::Let { name, ty, value } => StmtKind::Let {
					name: name.clone(),
					value: self.alloc(self.lower_expr(value)),
					ty,
				},
				ast::StmtKind::Assign { target, value } => StmtKind::Assign {
					target: target.clone(),
					value: self.alloc(self.lower_expr(value)),
				},

				ast::StmtKind::Expr(expr) => {
					let expr = self.alloc(self.lower_expr(expr));
					StmtKind::Expr(expr)
				}

				ast::StmtKind::ExprRet(expr) => {
					let expr = self.alloc(self.lower_expr(expr));
					if tail.is_empty() {
						ret_expr = Some(expr);
						continue;
					}
					todo!("accept otherwise? or error?, {stmt:?} and {tail:?}")
				}

				ast::StmtKind::Empty => continue,
			};

			stmts.push(Stmt { kind: stmt_kind });
		}

		Block {
			stmts: self.tcx.arena.alloc(stmts),
			ret_expr,
		}
	}

	/// Lower an AST `while cond { body }` to an HIR `loop { if cond { body } else { break } }`
	fn lower_while_loop(
		&'lcx self,
		cond: &'lcx ast::ExprKind,
		body: &'lcx ast::Block,
	) -> StmtKind<'lcx> {
		let if_expr = ExprKind::If {
			cond: self.alloc(self.lower_expr(cond)),
			conseq: self.alloc(self.lower_block(body)),
			altern: Some(
				self.alloc(self.mk_expr_block(self.alloc(self.mk_expr(ExprKind::Break(None))))),
			),
		};
		let loop_blk = self.mk_expr_block(self.alloc(self.mk_expr(if_expr)));
		StmtKind::Loop {
			block: self.alloc(loop_blk),
		}
	}

	fn lower_expr(&'lcx self, expr: &'lcx ast::ExprKind) -> Expr<'lcx> {
		let kind = match expr {
			ast::ExprKind::Variable(ident) => ExprKind::Variable(ident.clone()),
			ast::ExprKind::Literal(lit) => ExprKind::Literal(lit.clone()),
			ast::ExprKind::Binary { op, left, right } => ExprKind::Binary(
				*op,
				self.alloc(self.lower_expr(left)),
				self.alloc(self.lower_expr(right)),
			),
			ast::ExprKind::FnCall { expr, args } => {
				let args = self.alloc_iter(args.iter().map(|e| self.lower_expr(e)));
				ExprKind::FnCall {
					expr: self.alloc(self.lower_expr(expr)),
					args,
				}
			}
			ast::ExprKind::If {
				cond,
				conseq,
				altern,
			} => ExprKind::If {
				cond: self.alloc(self.lower_expr(cond)),
				conseq: self.alloc(self.lower_block(conseq)),
				altern: altern.as_ref().map(|a| self.alloc(self.lower_block(a))),
			},
		};

		Expr { kind }
	}
}
