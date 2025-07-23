//! Common data for front related operations

use std::{cmp, fmt, ops::Sub, process, rc::Rc};

use parking_lot::RwLock;
use string_interner::{StringInterner, Symbol as _, backend::StringBackend, symbol::SymbolU32};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
	pub start: u32,
	pub end: u32,
}

impl Span {
	pub(crate) const DUMMY: Self = Self::new(u32::MAX, u32::MAX);

	#[must_use]
	pub const fn new(start: u32, end: u32) -> Self {
		Self { start, end }
	}

	#[must_use]
	pub fn to(self, span: Self) -> Self {
		Self {
			start: cmp::min(self.start, span.start),
			end: cmp::max(self.end, span.end),
		}
	}

	#[must_use]
	pub const fn start(self) -> Self {
		Self {
			start: self.start,
			end: self.start,
		}
	}

	#[must_use]
	pub const fn end(self) -> Self {
		Self {
			start: self.end,
			end: self.end,
		}
	}
}

impl Sub<u32> for Span {
	type Output = Self;
	fn sub(self, rhs: u32) -> Self::Output {
		Self {
			start: self.start - rhs,
			end: self.end - rhs,
		}
	}
}

impl fmt::Debug for Span {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "sp#{}..{}", self.start, self.end)
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolU32);

impl fmt::Debug for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[cfg(feature = "debug")]
		let interned = INTERNER.with(|i| {
			i.get().map_or(Ok(false), |i| {
				i.read().resolve(self.0).map_or(Ok(false), |str| {
					write!(f, "`{str}`#{}", self.0.to_usize()).map(|()| true)
				})
			})
		})?;
		#[cfg(not(feature = "debug"))]
		let interned = false;

		if !interned {
			write!(f, "sym#{:?}", self.0.to_usize())?;
		}

		Ok(())
	}
}

#[cfg(feature = "debug")]
thread_local! {
	static INTERNER: std::sync::OnceLock<std::sync::Arc<RwLock<StringInterner<StringBackend>>>> = std::sync::OnceLock::default();
}

#[derive(Debug)]
pub struct SymbolInterner {
	#[cfg(feature = "debug")]
	inner: std::sync::Arc<RwLock<StringInterner<StringBackend>>>,
	#[cfg(not(feature = "debug"))]
	inner: RwLock<StringInterner<StringBackend>>,
}

impl Default for SymbolInterner {
	fn default() -> Self {
		let inner = RwLock::default();
		#[cfg(feature = "debug")]
		let inner = {
			let inner = std::sync::Arc::new(inner);
			_ = INTERNER.with(|i| i.set(inner.clone()));
			inner
		};
		Self { inner }
	}
}

impl SymbolInterner {
	#[must_use]
	pub fn intern(&self, symbol: &str) -> Symbol {
		Symbol(self.inner.write().get_or_intern(symbol))
	}

	#[must_use]
	pub fn resolve(&self, symbol: Symbol) -> String {
		self.inner.read().resolve(symbol.0).unwrap().to_owned()
	}
}

#[derive(Debug, Default)]
pub struct SessionCtx {
	pub symbols: SymbolInterner,
	pub source_map: RwLock<SourceMap>,
}

impl SessionCtx {
	pub fn emit_diagnostic(&self, diagnostic: &Diagnostic) {
		let fatal = diagnostic.level == DiagLevel::Error;

		eprintln!(
			"{}: `{}`",
			diagnostic.msg,
			self.source_map.read().fetch_span(diagnostic.span)
		);

		if fatal {
			process::exit(1);
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum DiagLevel {
	Error,
	Warning,
	Info,
}

#[derive(Debug)]
pub struct Diagnostic {
	msg: String,
	span: Span,

	level: DiagLevel,
	#[cfg(feature = "debug")]
	location: &'static std::panic::Location<'static>,
}

impl Diagnostic {
	#[must_use]
	#[track_caller]
	pub const fn new_err(msg: String, span: Span) -> Self {
		Self::new(DiagLevel::Error, msg, span)
	}

	#[must_use]
	#[track_caller]
	pub const fn new(level: DiagLevel, msg: String, span: Span) -> Self {
		Self {
			msg,
			span,
			level,
			#[cfg(feature = "debug")]
			location: std::panic::Location::caller(),
		}
	}
}

#[derive(Debug, Default)]
pub struct SourceMap {
	files: Vec<Rc<SourceFile>>,
	offset: u32,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
	pub name: String,

	pub content: String,

	pub offset: u32,
}

impl SourceMap {
	pub fn load_source(&mut self, name: &str, src: String) -> Rc<SourceFile> {
		let src_len = u32::try_from(src.len()).unwrap();

		let src_file = Rc::new(SourceFile {
			name: name.to_owned(),
			content: src,
			offset: self.offset,
		});

		self.files.push(src_file.clone());
		self.offset += src_len;

		src_file
	}

	#[must_use]
	pub fn fetch_span(&self, span: Span) -> &str {
		let file_idx = self
			.files
			.binary_search_by_key(&span.start, |f| f.offset)
			.unwrap_or_else(|p| p - 1);
		let file = &self.files[file_idx];

		let local_span = span - file.offset;

		&file.content[local_span.start as usize..local_span.end as usize]
	}
}
