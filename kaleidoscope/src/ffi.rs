use std::io::{Write, stdout};

#[allow(unsafe_code)]
#[unsafe(no_mangle)]
extern "C" fn putchard(i: i32) {
	print!("{}", u8::try_from(i).unwrap() as char);
	stdout().flush().unwrap();
}
