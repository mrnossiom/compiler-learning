//! Common data for front related operations

use std::fmt;

use parking_lot::RwLock;
use string_interner::{StringInterner, Symbol as _, backend::StringBackend, symbol::SymbolU32};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolU32);

impl fmt::Debug for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		// TODO: change interner from fcx to a global to have better access
		write!(f, "i#{:?}", self.0.to_usize())
	}
}

#[derive(Debug, Default)]
pub struct SymbolInterner {
	inner: RwLock<StringInterner<StringBackend>>,
}

impl SymbolInterner {
	pub fn intern(&self, symbol: &str) -> Symbol {
		Symbol(self.inner.write().get_or_intern(symbol))
	}

	pub fn resolve(&self, symbol: Symbol) -> String {
		self.inner.read().resolve(symbol.0).unwrap().to_owned()
	}
}

#[derive(Debug)]
pub struct FrontCtx {
	pub symbols: SymbolInterner,
}

impl FrontCtx {
	pub fn new() -> Self {
		Self {
			symbols: SymbolInterner::default(),
		}
	}
}
