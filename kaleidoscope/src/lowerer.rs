//! AST to HIR lowering logic

use std::sync::atomic::{self, AtomicU32};

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
}

impl<'scx> LowerCtx<'scx> {
	#[must_use]
	pub fn new(scx: &'scx SessionCtx) -> Self {
		Self { scx }
	}
}

impl LowerCtx<'_> {
	pub fn lower_root(&self, ast: &ast::Root) -> Root {
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
}

impl Lowerer<'_> {
	#[must_use]
	pub fn lower_items(&self, file: &ast::Root) -> Root {
		let items = file
			.items
			.iter()
			.map(|item| self.lower_item(item))
			.collect();

		Root { items }
	}

	fn lower_item(&self, item: &ast::Item) -> Item {
		let kind = match &item.kind {
			ast::ItemKind::Function { ident, decl, body } => ItemKind::Function {
				ident: *ident,
				decl: Box::new(self.lower_fn_decl(decl)),
				body: Box::new(self.lower_block(body)),
			},
			ast::ItemKind::Extern { ident, decl } => ItemKind::Extern {
				ident: *ident,
				decl: Box::new(self.lower_fn_decl(decl)),
			},
		};
		Item {
			kind,
			span: item.span,
			id: self.make_node_id(item.id),
		}
	}

	fn lower_fn_decl(&self, decl: &ast::FnDecl) -> FnDecl {
		let output = decl.ret.clone().unwrap_or_else(|| ast::Ty {
			kind: ast::TyKind::Unit,
			span: decl.span.end(),
		});
		FnDecl {
			inputs: decl.args.clone(),
			output: Box::new(output),
			span: decl.span,
		}
	}

	fn lower_block(&self, block: &ast::Block) -> Block {
		let mut stmts = Vec::new();
		let mut ret = None;

		let mut ast_stmts = &block.stmts[..];
		while let [stmt, tail @ ..] = ast_stmts {
			ast_stmts = tail;

			let kind = match &stmt.kind {
				ast::StmtKind::Loop { body } => StmtKind::Loop {
					block: Box::new(self.lower_block(body)),
				},
				// desugar to simple loop
				ast::StmtKind::WhileLoop { check, body } => self.lower_while_loop(check, body),
				ast::StmtKind::ForLoop { .. } => todo!("for loop is not parsed"),

				ast::StmtKind::Let { ident, ty, value } => StmtKind::Let {
					ident: *ident,
					ty: Box::new(ty.as_ref().map_or_else(
						|| ast::Ty {
							kind: ast::TyKind::Infer,
							span: ident.span.end(),
						},
						|ty| ty.as_ref().clone(),
					)),
					value: Box::new(self.lower_expr(value)),
				},
				ast::StmtKind::Assign { target, value } => StmtKind::Assign {
					target: *target,
					value: Box::new(self.lower_expr(value)),
				},

				ast::StmtKind::Expr(expr) => {
					let expr = Box::new(self.lower_expr(expr));
					StmtKind::Expr(expr)
				}

				ast::StmtKind::ExprRet(expr) => {
					let expr = Box::new(self.lower_expr(expr));

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
			stmts,
			ret,
			span: block.span,
			id: self.make_node_id(block.id),
		}
	}

	/// Lower an AST `while cond { body }` to an HIR `loop { if cond { body } else { break } }`
	fn lower_while_loop(&self, cond: &ast::Expr, body: &ast::Block) -> StmtKind {
		let break_expr = Expr {
			kind: ExprKind::Break(None),
			span: body.span,
			id: self.make_new_node_id(),
		};
		let altern_blk = Block {
			stmts: Vec::new(),
			ret: Some(Box::new(break_expr)),
			span: body.span,
			id: self.make_new_node_id(),
		};

		let if_expr = Expr {
			kind: ExprKind::If {
				cond: Box::new(self.lower_expr(cond)),
				conseq: Box::new(self.lower_block(body)),
				altern: Some(Box::new(altern_blk)),
			},
			span: body.span,
			id: self.make_new_node_id(),
		};
		let loop_blk = Block {
			stmts: Vec::new(),
			ret: Some(Box::new(if_expr)),

			span: body.span,
			id: self.make_new_node_id(),
		};

		StmtKind::Loop {
			block: Box::new(loop_blk),
		}
	}

	fn lower_expr(&self, expr: &ast::Expr) -> Expr {
		let kind = match &expr.kind {
			ast::ExprKind::Access(ident) => ExprKind::Access(*ident),
			ast::ExprKind::Literal(lit, ident) => ExprKind::Literal(*lit, *ident),

			ast::ExprKind::Paren(expr) => self.lower_expr(expr).kind,
			ast::ExprKind::Unary { op, expr } => self.lower_unary(*op, expr),
			ast::ExprKind::Binary { op, left, right } => self.lower_binary(*op, left, right),
			ast::ExprKind::FnCall { expr, args } => ExprKind::FnCall {
				expr: Box::new(self.lower_expr(expr)),
				args: args.with_bit(args.bit.iter().map(|e| self.lower_expr(e)).collect()),
			},
			ast::ExprKind::If {
				cond,
				conseq,
				altern,
			} => ExprKind::If {
				cond: Box::new(self.lower_expr(cond)),
				conseq: Box::new(self.lower_block(conseq)),
				altern: altern.as_ref().map(|a| Box::new(self.lower_block(a))),
			},

			ast::ExprKind::Return(expr) => {
				ExprKind::Return(expr.as_ref().map(|expr| Box::new(self.lower_expr(expr))))
			}
			ast::ExprKind::Break(expr) => {
				ExprKind::Break(expr.as_ref().map(|expr| Box::new(self.lower_expr(expr))))
			}
			ast::ExprKind::Continue => ExprKind::Continue,
		};

		Expr {
			kind,
			span: expr.span,
			id: self.make_node_id(expr.id),
		}
	}

	fn lower_unary(&self, op: Spanned<lexer::UnaryOp>, expr: &ast::Expr) -> ExprKind {
		ExprKind::Unary(op, Box::new(self.lower_expr(expr)))
	}

	fn lower_binary(
		&self,
		op: Spanned<lexer::BinaryOp>,
		left: &ast::Expr,
		right: &ast::Expr,
	) -> ExprKind {
		// TODO: lower to interface call
		// `a + b` becomes `Add.add(a, b)` or `<a as Add>.add(b)`
		// e.g. ExprKind::FnCall { expr: to_core_func(op), args: vec![left, right] }

		ExprKind::Binary(
			op,
			Box::new(self.lower_expr(left)),
			Box::new(self.lower_expr(right)),
		)
	}
}
