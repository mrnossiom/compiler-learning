//! FFI for Kaleidoscope programs to interact with the outside world
//!
//! This module is temporary.

use std::io::{Write, stdout};

#[allow(unsafe_code)]
#[unsafe(no_mangle)]
pub extern "C" fn putchard(i: i32) {
	print!("{}", u8::try_from(i).unwrap() as char);
	stdout().flush().unwrap();
}
