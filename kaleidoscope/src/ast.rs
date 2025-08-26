//! Abstract Syntax Tree

use std::fmt;

use crate::{
	lexer::{BinaryOp, LiteralKind, UnaryOp},
	session::{Span, Symbol},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Debug for NodeId {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		// ast node id -> aid
		write!(f, "aid#{}", self.0)
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Ident {
	pub sym: Symbol,
	pub span: Span,
}

impl Ident {
	#[must_use]
	pub const fn new(name: Symbol, span: Span) -> Self {
		Self { sym: name, span }
	}
}

impl fmt::Debug for Ident {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Ident({:?}, {:?})", self.sym, self.span)
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
	/// The bit of information that is spanned
	pub bit: T,

	pub span: Span,
}

impl<T> Spanned<T> {
	pub const fn new(bit: T, span: Span) -> Self {
		Self { bit, span }
	}

	pub const fn with_bit<U>(&self, bit: U) -> Spanned<U> {
		Spanned::new(bit, self.span)
	}

	pub fn map<U>(&self, map: impl FnOnce(&T) -> U) -> Spanned<U> {
		Spanned::new(map(&self.bit), self.span)
	}
}

#[derive(Debug)]
pub struct Root {
	pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct Expr {
	pub kind: ExprKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum ExprKind {
	// Atomics
	Access(Path),
	Literal(LiteralKind, Symbol),

	// Composition
	/// `( <expr> )`
	Paren(Box<Expr>),
	/// `<op> <expr>`
	Unary {
		op: Spanned<UnaryOp>,
		expr: Box<Expr>,
	},
	/// `<left> <op> <right>`
	Binary {
		op: Spanned<BinaryOp>,
		left: Box<Expr>,
		right: Box<Expr>,
	},
	/// `<expr> ( <args>* )`
	FnCall {
		expr: Box<Expr>,
		args: Spanned<Vec<Expr>>,
	},

	/// `if <cond> <conseq> [ else <altern> ]`
	If {
		cond: Box<Expr>,
		conseq: Box<Block>,
		altern: Option<Box<Block>>,
	},

	/// `<expr> . *`
	Deref(Box<Expr>),

	/// `<target> = <value>`
	Assign {
		target: Box<Expr>,
		value: Box<Expr>,
	},

	/// `return [ <label> ] [ <expr> ]`
	Return(Option<Box<Expr>>),
	/// `break [ <label> ]`
	Break(Option<Box<Expr>>),
	/// `continue [ <label> ]`
	Continue,
}

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Stmt>,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub struct FnDecl {
	pub params: Vec<Param>,
	pub ret: Option<Ty>,

	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
	pub name: Ident,
	pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Ty {
	pub kind: TyKind,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
	Path(Path),

	Pointer(Box<Ty>),

	Unit,

	/// Corresponds to the explicit `_` token
	Infer,
}

#[derive(Debug, Clone)]

pub struct Path {
	pub segments: Vec<Ident>,
	pub generics: Vec<Ty>,
}

#[derive(Debug)]
pub struct Item {
	pub kind: ItemKind,
	pub span: Span,
	pub id: NodeId,
}

/// `type <name> (= <ty>)|;`
#[derive(Debug)]
pub struct Type {
	pub name: Ident,
	pub alias: Option<Box<Ty>>,
}

/// `[ extern <externess> ] fn <name> <decl> <body>|;`
#[derive(Debug)]
pub struct Function {
	pub name: Ident,
	pub decl: FnDecl,
	pub body: Option<Box<Block>>,

	pub externess: Option<Expr>,
}

#[derive(Debug)]
pub enum ItemKind {
	Function(Function),
	Type(Type),
	/// `struct <name> <generics> { <fields>* }`
	Struct {
		name: Ident,
		generics: Vec<Ident>,
		fields: Vec<FieldDef>,
	},
	/// `enum <name> <generics> { <variant>* }`
	Enum {
		name: Ident,
		generics: Vec<Ident>,
		variants: Vec<Variant>,
	},
	/// `trait <name> <generics> { <items>* }`
	Trait {
		name: Ident,
		generics: Vec<Ident>,
		members: Vec<TraitItem>,
	},

	/// `for <type> impl <trait> { <items>* }`
	TraitImpl {
		type_: Path,
		trait_: Path,
		members: Vec<TraitItem>,
	},
}

/// `<name> : <ty>`
#[derive(Debug)]
pub struct FieldDef {
	pub name: Ident,
	pub ty: Ty,

	pub span: Span,
}

/// `<name> <kind>`
#[derive(Debug)]
pub struct Variant {
	pub name: Ident,
	pub kind: VariantKind,
	pub span: Span,
}

#[derive(Debug)]
pub enum VariantKind {
	/// `ε`
	Bare,
	/// `( <tys>* )`
	Tuple(Vec<Ty>),
	/// `{ <fields>* }`
	Struct(Vec<FieldDef>),
}

#[derive(Debug)]
pub struct TraitItem {
	pub kind: TraitItemKind,
	pub span: Span,
}

#[derive(Debug)]
pub enum TraitItemKind {
	Type(Type),
	Function(Function),
}

#[derive(Debug)]
pub struct Stmt {
	pub kind: StmtKind,
	pub span: Span,
	pub id: NodeId,
}

#[derive(Debug)]
pub enum StmtKind {
	/// `loop <body>`
	Loop { body: Box<Block> },
	/// `while <check> <body>`
	WhileLoop { check: Box<Expr>, body: Box<Block> },

	/// `var <name> [ : <ty> ] = <expr> ;`
	Let {
		name: Ident,
		ty: Option<Box<Ty>>,
		value: Box<Expr>,
	},

	/// `<expr> ;`
	Expr(Box<Expr>),

	/// Expression without a semi to return a value at the end of a block
	ExprRet(Box<Expr>),

	/// A single lonely `;`
	Empty,
}
