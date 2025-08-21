//! AST to HIR lowering logic

use std::sync::atomic::{self, AtomicU32};

use bumpalo::Bump;

use crate::{
	ast::{self, Spanned},
	errors,
	hir::{Block, Expr, ExprKind, FnDecl, Item, ItemKind, NodeId, Root, Stmt, StmtKind},
	lexer,
	session::SessionCtx,
};

#[derive(Debug)]
pub struct LowerCtx<'scx> {
	pub scx: &'scx SessionCtx,

	pub arena: Bump,
}

impl<'scx> LowerCtx<'scx> {
	#[must_use]
	pub fn new(scx: &'scx SessionCtx) -> Self {
		Self {
			scx,
			arena: Bump::default(),
		}
	}
}

impl LowerCtx<'_> {
	pub fn lower_root<'a>(&'a self, ast: &'a ast::Root) -> Root<'a> {
		tracing::trace!("lower_root");
		let lowerer = Lowerer::new(self);
		lowerer.lower_items(ast)
	}
}

#[derive(Debug)]
pub struct Lowerer<'lcx> {
	lcx: &'lcx LowerCtx<'lcx>,

	next_node_id: AtomicU32,
}

impl<'lcx> Lowerer<'lcx> {
	pub const fn new(lcx: &'lcx LowerCtx) -> Self {
		Self {
			lcx,
			next_node_id: AtomicU32::new(0),
		}
	}

	fn make_node_id(&self, aid: ast::NodeId) -> NodeId {
		// TODO: store hid provenance
		let _ = aid;

		let hid = self.next_node_id.fetch_add(1, atomic::Ordering::Relaxed);
		NodeId(hid)
	}

	fn make_new_node_id(&self) -> NodeId {
		let hid = self.next_node_id.fetch_add(1, atomic::Ordering::Relaxed);
		NodeId(hid)
	}

	fn alloc<T>(&self, t: T) -> &'lcx T {
		self.lcx.arena.alloc(t)
	}

	fn alloc_iter<T, I>(&self, i: I) -> &'lcx [T]
	where
		I: IntoIterator<Item = T>,
		I::IntoIter: ExactSizeIterator,
	{
		self.lcx.arena.alloc_slice_fill_iter(i)
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
			id: self.make_node_id(item.id),
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

	fn lower_block(&self, block: &'lcx ast::Block) -> Block<'lcx> {
		let mut stmts = Vec::new();
		let mut ret = None;

		let mut ast_stmts = &block.stmts[..];
		while let [stmt, tail @ ..] = ast_stmts {
			ast_stmts = tail;

			let kind = match &stmt.kind {
				ast::StmtKind::Loop { body } => StmtKind::Loop {
					block: self.alloc(self.lower_block(body)),
				},
				// desugar to simple loop
				ast::StmtKind::WhileLoop { check, body } => self.lower_while_loop(check, body),
				ast::StmtKind::ForLoop { .. } => todo!("for loop is not parsed"),

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

					// correct case
					if tail.is_empty() {
						ret = Some(expr);
						break;
					}

					let report = errors::lowerer::no_semicolon_mid_block(expr.span);
					self.lcx.scx.dcx().emit_build(report);

					// recover like there was a semicolon
					StmtKind::Expr(expr)
				}

				ast::StmtKind::Empty => continue,
			};

			stmts.push(Stmt {
				kind,
				span: stmt.span,
				id: self.make_node_id(stmt.id),
			});
		}

		Block {
			stmts: self.alloc(stmts),
			ret,
			span: block.span,
			id: self.make_node_id(block.id),
		}
	}

	/// Lower an AST `while cond { body }` to an HIR `loop { if cond { body } else { break } }`
	fn lower_while_loop(&self, cond: &'lcx ast::Expr, body: &'lcx ast::Block) -> StmtKind<'lcx> {
		let break_expr = Expr {
			kind: ExprKind::Break(None),
			span: body.span,
			id: self.make_new_node_id(),
		};
		let altern_blk = Block {
			stmts: self.alloc_iter([]),
			ret: Some(self.alloc(break_expr)),
			span: body.span,
			id: self.make_new_node_id(),
		};

		let if_expr = Expr {
			kind: ExprKind::If {
				cond: self.alloc(self.lower_expr(cond)),
				conseq: self.alloc(self.lower_block(body)),
				altern: Some(self.alloc(altern_blk)),
			},
			span: body.span,
			id: self.make_new_node_id(),
		};
		let loop_blk = Block {
			stmts: self.alloc_iter([]),
			ret: Some(self.alloc(if_expr)),

			span: body.span,
			id: self.make_new_node_id(),
		};

		StmtKind::Loop {
			block: self.alloc(loop_blk),
		}
	}

	fn lower_expr(&self, expr: &'lcx ast::Expr) -> Expr<'lcx> {
		let kind = match &expr.kind {
			ast::ExprKind::Variable(ident) => ExprKind::Variable(*ident),
			ast::ExprKind::Literal(lit, ident) => ExprKind::Literal(*lit, *ident),
			ast::ExprKind::Binary { op, left, right } => self.lower_binary(op, left, right),
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

			ast::ExprKind::Return(expr) => {
				ExprKind::Return(expr.as_ref().map(|expr| self.alloc(self.lower_expr(expr))))
			}
			ast::ExprKind::Break(expr) => {
				ExprKind::Break(expr.as_ref().map(|expr| self.alloc(self.lower_expr(expr))))
			}
			ast::ExprKind::Continue => ExprKind::Continue,
		};

		Expr {
			kind,
			span: expr.span,
			id: self.make_node_id(expr.id),
		}
	}

	fn lower_binary(
		&self,
		op: &'lcx Spanned<lexer::BinOp>,
		left: &'lcx ast::Expr,
		right: &'lcx ast::Expr,
	) -> ExprKind<'lcx> {
		// TODO: lower to interface call
		// `a + b` becomes `Add.add(a, b)` or `<a as Add>.add(b)`
		// e.g. ExprKind::FnCall { expr: to_core_func(op), args: vec![left, right] }

		ExprKind::Binary(
			*op,
			self.alloc(self.lower_expr(left)),
			self.alloc(self.lower_expr(right)),
		)
	}
}
