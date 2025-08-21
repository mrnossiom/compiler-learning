use ariadne::{Label, ReportKind};

use crate::session::{Report, ReportBuilder, Span};

pub mod parser {
	use crate::lexer::{Token, TokenKind};

	use super::*;

	pub fn expected_token_kind(expected: TokenKind, actual: Token) -> ReportBuilder {
		Report::build(ReportKind::Error, actual.span)
			.with_message(format!("expected {expected}"))
			.with_label(Label::new(actual.span).with_message(format!("got {}", actual.kind)))
	}

	/// Construct should fit in the sentence "expected {}"
	pub fn expected_construct_no_match(construct: &str, token_span: Span) -> ReportBuilder {
		Report::build(ReportKind::Error, token_span)
			.with_message(format!("expected {construct}"))
			.with_label(Label::new(token_span).with_message("got an unexpected token".to_string()))
	}
}

pub mod lowerer {
	use super::*;

	pub fn no_semicolon_mid_block(expr_span: Span) -> ReportBuilder {
		Report::build(ReportKind::Error, expr_span)
			.with_message("expression is missing a semicolon but is not at the end")
			.with_message("you may need to add a semicolon at the end of the expression")
	}
}

pub mod ty {
	use super::*;

	pub fn report_unconstrained(ty_span: Span) -> ReportBuilder {
		Report::build(ReportKind::Error, ty_span)
			.with_message("expression's type is unconstrained, need type annotations")
			.with_label(Label::new(ty_span))
	}

	pub fn function_cannot_infer_signature(io_span: Span) -> ReportBuilder {
		Report::build(ReportKind::Error, io_span)
			.with_message("function cannot infer its signature")
			.with_label(Label::new(io_span).with_message("specify a concrete type"))
	}

	pub fn type_unknown(path_span: Span) -> ReportBuilder {
		Report::build(ReportKind::Error, path_span)
			.with_message("type is invalid")
			.with_label(Label::new(path_span).with_message("type is not in scope"))
	}
}
