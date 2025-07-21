//! Common data for front related operations

use std::fmt;

use parking_lot::RwLock;
use string_interner::{StringInterner, Symbol as _, backend::StringBackend, symbol::SymbolU32};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(SymbolU32);

impl fmt::Debug for Symbol {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[cfg(feature = "debug")]
		let interned = INTERNER.with(|i| {
			i.get().map_or(Ok(false), |i| {
				i.read()
					.resolve(self.0)
					.map_or(Ok(false), |str| write!(f, "i`{str}`").map(|()| true))
			})
		})?;
		#[cfg(not(feature = "debug"))]
		let interned = false;

		if !interned {
			write!(f, "i#{:?}", self.0.to_usize())?;
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

impl SymbolInterner {
	#[must_use]
	pub fn new() -> Self {
		let inner = RwLock::default();
		#[cfg(feature = "debug")]
		let inner = {
			let inner = std::sync::Arc::new(inner);
			_ = INTERNER.with(|i| i.set(inner.clone()));
			inner
		};
		Self { inner }
	}

	#[must_use]
	pub fn intern(&self, symbol: &str) -> Symbol {
		Symbol(self.inner.write().get_or_intern(symbol))
	}

	#[must_use]
	pub fn resolve(&self, symbol: Symbol) -> String {
		self.inner.read().resolve(symbol.0).unwrap().to_owned()
	}
}

#[derive(Debug)]
pub struct SessionCtx {
	pub symbols: SymbolInterner,
}

impl SessionCtx {
	#[must_use]
	pub fn new() -> Self {
		Self {
			symbols: SymbolInterner::new(),
		}
	}
}
