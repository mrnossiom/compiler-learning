use std::collections::HashMap;

use cranelift::prelude::FunctionBuilderContext;
use cranelift_module::Module;
use cranelift_simplejit::{SimpleJITBuilder, SimpleJITModule};

use crate::Ident;

pub struct Generator {
	builder_context: FunctionBuilderContext,
	// functions: HashMap<Ident, CompiledFunction>,
	module: SimpleJITModule,
	// variable_builder: VariableBuilder,
}
