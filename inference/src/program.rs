use crate::ir::{Block, ExprKind, Function, Ident, StmtKind, TyKind};

pub fn fibo() -> Function {
	Function {
		name: Ident("fibo".to_string()),
		params: vec![(
			Ident("n".to_string()),
			TyKind::Ident(Ident("uint".to_string())),
		)],
		ret_type: None,
		body: Block::new(vec![
			StmtKind::Let {
				ident: Ident("a".to_string()),
				ty: None,
				value: ExprKind::Number("0".to_string()),
			},
			StmtKind::Let {
				ident: Ident("b".to_string()),
				ty: None,
				value: ExprKind::Number("0".to_string()),
			},
			StmtKind::Let {
				ident: Ident("i".to_string()),
				ty: None,
				value: ExprKind::Ident("n".to_string()),
			},
			StmtKind::While {
				cond: ExprKind::Bin(
					"!=".to_string(),
					Box::new(ExprKind::Ident("n".to_string())),
					Box::new(ExprKind::Number("0".to_string())),
				),
				body: Block::new(vec![
					StmtKind::Let {
						ident: Ident("next".to_string()),
						ty: None,
						value: ExprKind::Bin(
							"+".to_string(),
							Box::new(ExprKind::Ident("a".to_string())),
							Box::new(ExprKind::Ident("b".to_string())),
						),
					},
					StmtKind::Assign {
						ident: Ident("a".to_string()),
						value: ExprKind::Ident("b".to_string()),
					},
					StmtKind::Assign {
						ident: Ident("b".to_string()),
						value: ExprKind::Ident("next".to_string()),
					},
					StmtKind::Assign {
						ident: Ident("n".to_string()),
						value: ExprKind::Bin(
							"-".to_string(),
							Box::new(ExprKind::Ident("n".to_string())),
							Box::new(ExprKind::Number("1".to_string())),
						),
					},
				]),
			},
			StmtKind::Return(ExprKind::Ident("b".to_string())),
		]),
	}
}
