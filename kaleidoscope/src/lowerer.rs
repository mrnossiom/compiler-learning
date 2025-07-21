//! AST to HIR lowering logic

use bumpalo::Bump;

use crate::{
	ast,
	hir::{Block, Expr, ExprKind, FnDecl, Item, ItemKind, Root, Stmt, StmtKind},
};

#[derive(Debug)]
pub struct LowerCtx {
	pub arena: Bump,
}

impl LowerCtx {
	#[must_use]
	pub fn new() -> Self {
		Self { arena: Bump::new() }
	}
}

impl LowerCtx {
	pub fn lower_root<'a>(&'a self, ast: &'a ast::Root) -> Root<'a> {
		let lowerer = Lowerer::new(self);
		lowerer.lower_items(ast)
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

	fn mk_expr_block(&self, expr: &'lcx Expr<'lcx>) -> Block<'lcx> {
		Block {
			stmts: self.alloc([]),
			ret_expr: Some(expr),
		}
	}
}

impl<'lcx> Lowerer<'lcx> {
	#[must_use]
	pub fn lower_items(&self, file: &'lcx ast::Root) -> Root<'lcx> {
		let items = file.items.iter().map(|item| self.lower_item(item));

		Root {
			items: self.alloc_iter(items),
		}
	}

	fn lower_item(&self, item: &'lcx ast::Item) -> Item<'lcx> {
		let kind = match &item.kind {
			ast::ItemKind::Function { ident, decl, body } => ItemKind::Function {
				ident: *ident,
				decl: self.lower_fn_decl(decl),
				body: self.alloc(self.lower_block(body)),
			},
			ast::ItemKind::Extern { ident, decl } => ItemKind::Extern {
				ident: *ident,
				decl: self.lower_fn_decl(decl),
			},
		};
		Item {
			kind,
			span: item.span,
		}
	}

	fn lower_fn_decl(&self, decl: &'lcx ast::FnDecl) -> &'lcx FnDecl<'lcx> {
		let inputs = decl.args.iter().cloned();
		self.alloc(FnDecl {
			inputs: self.alloc_iter(inputs),
			output: decl.ret.as_ref().unwrap_or_else(|| {
				self.alloc(ast::Ty {
					kind: ast::TyKind::Unit,
					span: decl.span.end(),
				})
			}),
			span: decl.span,
		})
	}

	fn lower_block(&self, body: &'lcx ast::Block) -> Block<'lcx> {
		let mut stmts = Vec::new();
		let mut ret_expr = None;

		let mut ast_stmts = &body.stmts[..];
		while let [stmt, tail @ ..] = ast_stmts {
			ast_stmts = tail;

			let kind = match &stmt.kind {
				ast::StmtKind::Loop { body } => todo!(),
				// desugar to simple loop
				ast::StmtKind::WhileLoop { check, body } => self.lower_while_loop(check, body),
				ast::StmtKind::ForLoop { pat, iter, body } => todo!(),

				ast::StmtKind::Let { ident, ty, value } => StmtKind::Let {
					ident: *ident,
					ty: self.alloc(ty.as_ref().map_or_else(
						|| ast::Ty {
							kind: ast::TyKind::Infer,
							span: ident.span.end(),
						},
						|ty| ty.as_ref().clone(),
					)),
					value: self.alloc(self.lower_expr(value)),
				},
				ast::StmtKind::Assign { target, value } => StmtKind::Assign {
					target: *target,
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

			stmts.push(Stmt {
				kind,
				span: stmt.span,
			});
		}

		Block {
			stmts: self.alloc(stmts),
			ret_expr,
		}
	}

	/// Lower an AST `while cond { body }` to an HIR `loop { if cond { body } else { break } }`
	fn lower_while_loop(&self, cond: &'lcx ast::Expr, body: &'lcx ast::Block) -> StmtKind<'lcx> {
		let if_expr = ExprKind::If {
			cond: self.alloc(self.lower_expr(cond)),
			conseq: self.alloc(self.lower_block(body)),
			altern: Some(self.alloc(self.mk_expr_block(self.alloc(Expr {
				kind: ExprKind::Break(None),
				span: body.span,
			})))),
		};
		let loop_blk = self.mk_expr_block(self.alloc(Expr {
			kind: if_expr,
			span: body.span,
		}));
		StmtKind::Loop {
			block: self.alloc(loop_blk),
		}
	}

	fn lower_expr(&self, expr: &'lcx ast::Expr) -> Expr<'lcx> {
		let kind = match &expr.kind {
			ast::ExprKind::Variable(ident) => ExprKind::Variable(*ident),
			ast::ExprKind::Literal(lit, ident) => ExprKind::Literal(*lit, *ident),
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

		Expr {
			kind,
			span: expr.span,
		}
	}
}
