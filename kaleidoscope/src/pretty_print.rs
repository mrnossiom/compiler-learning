//! Pretty print a source file.
//!
//! This essentially prints the AST taking line length into account. It also
//! reads content that not in the AST like comments to not lose any data.

use std::io::{self, Write, stdout};

use crate::{
	ast::{
		Block, Expr, ExprKind, Function, Item, ItemKind, Param, Path, Root, Stmt, StmtKind, Ty,
		TyKind, Type,
	},
	lexer::{BinaryOp, LiteralKind, UnaryOp},
	session::Symbol,
};

type Result<T> = std::result::Result<T, io::Error>;

pub struct PrettyFormatter<'fmt> {
	inner: &'fmt mut dyn Write,

	ident: u32,
}

impl PrettyFormatter<'_> {
	fn with_ident(&mut self, f: impl FnOnce(&mut PrettyFormatter) -> Result<()>) -> Result<()> {
		self.ident += 1;
		f(self)?;
		self.ident -= 1;
		Ok(())
	}

	fn newline(&mut self) -> io::Result<()> {
		self.inner.write_all(b"\n")?;
		for _ in 0..self.ident {
			self.inner.write_all(b"\t")?;
		}

		Ok(())
	}

	fn write(&mut self, s: &str) -> io::Result<()> {
		self.inner.write_all(s.as_bytes())?;
		Ok(())
	}

	fn write_seq<T>(
		&mut self,
		elements: &[T],
		mut print: impl FnMut(&mut Self, &T) -> Result<()>,
		sep: &str,
	) -> Result<()> {
		for (i, elem) in elements.iter().enumerate() {
			print(self, elem)?;
			if i != elements.len() - 1 {
				self.write(sep)?;
			}
		}
		Ok(())
	}
}

// impl<'fmt> fmt::Write for PrettyFormatter<'fmt> {}

pub fn pretty_print_root(root: &Root) -> Result<()> {
	let out = stdout();
	let mut f = PrettyFormatter {
		inner: &mut &out,
		ident: 0,
	};
	root.pprint(&mut f)?;
	Ok(())
}

trait PrettyPrint {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()>;
}

impl PrettyPrint for Root {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		for item in &self.items {
			item.pprint(f)?;
			f.newline()?;
			f.newline()?;
		}
		Ok(())
	}
}

impl PrettyPrint for Item {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match &self.kind {
			ItemKind::Function(func) => func.pprint(f)?,
			ItemKind::Type(ty) => ty.pprint(f)?,

			ItemKind::Struct { .. } => todo!(),
			ItemKind::Enum { .. } => todo!(),
			ItemKind::Trait { .. } => todo!(),
			ItemKind::TraitImpl { .. } => todo!(),
		}
		Ok(())
	}
}

impl PrettyPrint for Function {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		if let Some(abi) = &self.abi {
			f.write("extern ")?;
			abi.pprint(f)?;
			f.write(" ")?;
		}

		f.write("fn ")?;
		self.name.sym.pprint(f)?;
		f.write("(")?;
		f.write_seq(&self.decl.params, |f, param| param.pprint(f), ", ")?;
		f.write(")")?;

		if let Some(ret) = &self.decl.ret {
			f.write(" ")?;
			ret.pprint(f)?;
		}

		if let Some(body) = &self.body {
			f.write(" ")?;
			body.pprint(f)?;
		} else {
			f.write(";")?;
		}

		Ok(())
	}
}

impl PrettyPrint for Type {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		f.write("type ")?;
		self.name.sym.pprint(f)?;
		if let Some(alias) = &self.alias {
			f.write(" = ")?;
			alias.pprint(f)?;
		}
		f.write(";")?;
		Ok(())
	}
}

impl PrettyPrint for Expr {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match &self.kind {
			ExprKind::Access(path) => path.pprint(f),
			ExprKind::Literal(kind, sym) => match kind {
				LiteralKind::Integer | LiteralKind::Float => sym.pprint(f),
				LiteralKind::Str => {
					f.write("\"")?;
					sym.pprint(f)?;
					f.write("\"")?;
					Ok(())
				}
			},

			ExprKind::Paren(expr) => {
				f.write("(")?;
				expr.pprint(f)?;
				f.write(")")?;
				Ok(())
			}
			ExprKind::Unary { op, expr } => {
				op.bit.pprint(f)?;
				expr.pprint(f)?;
				Ok(())
			}
			ExprKind::Binary { op, left, right } => {
				left.pprint(f)?;
				f.write(" ")?;
				op.bit.pprint(f)?;
				f.write(" ")?;
				right.pprint(f)?;
				Ok(())
			}

			ExprKind::FnCall { expr, args } => todo!(),
			ExprKind::If {
				cond,
				conseq,
				altern,
			} => todo!(),
			ExprKind::Method(expr, name, args) => todo!(),
			ExprKind::Field(expr, name) => todo!(),
			ExprKind::Deref(expr) => todo!(),
			ExprKind::Assign { target, value } => todo!(),
			ExprKind::Return(expr) => todo!(),
			ExprKind::Break(expr) => todo!(),
			ExprKind::Continue => todo!(),
		}
	}
}

impl PrettyPrint for Param {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		self.name.sym.pprint(f)?;
		f.write(": ")?;
		self.ty.pprint(f)?;
		Ok(())
	}
}

impl PrettyPrint for Ty {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match &self.kind {
			TyKind::Path(path) => path.pprint(f)?,

			TyKind::Pointer(ty) => {
				f.write("&")?;
				ty.pprint(f)?;
			}
			TyKind::Unit => f.write("()")?,
			TyKind::Infer => f.write("_")?,
		}
		Ok(())
	}
}

impl PrettyPrint for Symbol {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		// TODO
		f.write(&format!("{self:#?}"))
	}
}

impl PrettyPrint for Path {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		f.write_seq(&self.segments, |f, segment| segment.sym.pprint(f), "::")?;
		if !self.generics.is_empty() {
			f.write("<")?;
			f.write_seq(&self.generics, |f, generic| generic.pprint(f), ", ")?;
			f.write(">")?;
		}
		Ok(())
	}
}

impl PrettyPrint for Block {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		f.write("{")?;
		f.with_ident(|f| {
			f.newline()?;

			for stmt in &self.stmts {
				stmt.pprint(f)?;
			}

			Ok(())
		})?;
		f.newline()?;
		f.write("}")?;

		Ok(())
	}
}

impl PrettyPrint for Stmt {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match &self.kind {
			StmtKind::Loop { body } => {
				f.write("loop ")?;
				body.pprint(f)?;
			}
			StmtKind::WhileLoop { check, body } => {
				f.write("while ")?;
				check.pprint(f)?;
				f.write(" ")?;
				body.pprint(f)?;
			}

			StmtKind::Let { name, ty, value } => {
				f.write("let ")?;
				name.sym.pprint(f)?;
				if let Some(ty) = &ty {
					f.write(": ")?;
					ty.pprint(f)?;
				}
				f.write(" = ")?;
				value.pprint(f)?;
				f.write(";")?;
			}

			StmtKind::Empty => f.write(";")?,

			StmtKind::Expr(expr) => {
				expr.pprint(f)?;
				f.write(";")?;
			}
			StmtKind::ExprRet(expr) => expr.pprint(f)?,
		}
		Ok(())
	}
}

impl PrettyPrint for UnaryOp {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match self {
			Self::Not => f.write("!"),
			Self::Minus => f.write("-"),
		}
	}
}

impl PrettyPrint for BinaryOp {
	fn pprint(&self, f: &mut PrettyFormatter) -> Result<()> {
		match self {
			Self::Plus => f.write("+"),
			Self::Minus => f.write("-"),
			Self::Mul => f.write("*"),
			Self::Div => f.write("/"),
			Self::Mod => f.write("%"),

			Self::And => f.write("&"),
			Self::Or => f.write("|"),
			Self::Xor => f.write("^"),

			Self::Shl => f.write("<<"),
			Self::Shr => f.write(">>"),

			Self::Gt => f.write(">"),
			Self::Ge => f.write(">="),
			Self::Lt => f.write("<"),
			Self::Le => f.write("<="),

			Self::Ne => f.write("!="),
			Self::EqEq => f.write("=="),
		}
	}
}
