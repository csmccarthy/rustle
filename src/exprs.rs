use crate::scanner::{ Token, Tokens, Literal as LiteralValue };
use std::fmt::Display;

enum RuntimeError {
	InvalidBinaryOp(Tokens),
	InvalidBinaryLiteral(LiteralValue),
	InvalidConditional(Box<dyn Expr>),
	InvalidUnaryOp(Tokens),
	InvalidUnaryLiteral(LiteralValue),
}

type RuntimeResult<T> = std::result::Result<T, RuntimeError>;

pub trait ExprVisitor<R> {
	fn visit_binary(&self, expr: &Binary) -> R;
	fn visit_grouping(&self, expr: &Grouping) -> R;
	fn visit_literal(&self, expr: &Literal) -> R;
	fn visit_unary(&self, expr: &Unary) -> R;
	fn visit_ternary(&self, expr: &Ternary) -> R;
}

struct ASTEvaluator;

impl ExprVisitor<RuntimeResult<LiteralValue>> for ASTEvaluator {
	fn visit_binary(&self, expr: &Binary) -> RuntimeResult<LiteralValue> {
		let left = expr.left.accept(self)?;
		// let a = (*expr.left);
		let right = expr.right.accept(self)?;

		match expr.operator.token_type {
			Tokens::Plus => { match left {
				LiteralValue::Num(n1) => { match right {
					LiteralValue::Num(n2) => return Ok(LiteralValue::Num(n1 + n2)),
					_ => return Err(RuntimeError::InvalidBinaryLiteral(right))
				} },
				LiteralValue::Str(s1) => { match right {
					LiteralValue::Str(s2) => return Ok(LiteralValue::Str(s1 + &s2)),
					_ => return Err(RuntimeError::InvalidBinaryLiteral(right))
				} },
				_ => return Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Minus => { match left {
				LiteralValue::Num(n1) => { match right {
					LiteralValue::Num(n2) => return Ok(LiteralValue::Num(n1 - n2)),
					_ => return Err(RuntimeError::InvalidBinaryLiteral(right))
				} },
				_ => return Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Star => { match left {
				LiteralValue::Num(n1) => { match right {
					LiteralValue::Num(n2) => return Ok(LiteralValue::Num(n1 * n2)),
					_ => return Err(RuntimeError::InvalidBinaryLiteral(right))
				} },
				_ => return Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Slash => { match left {
				LiteralValue::Num(n1) => { match right {
					LiteralValue::Num(n2) => return Ok(LiteralValue::Num(n1 / n2)),
					_ => return Err(RuntimeError::InvalidBinaryLiteral(right))
				} },
				_ => return Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },
			_ => return Err(RuntimeError::InvalidBinaryOp(expr.operator.token_type))
		}
	}

	fn visit_grouping(&self, expr: &Grouping) -> RuntimeResult<LiteralValue> {
		expr.expression.accept(self)
	}

	fn visit_literal(&self, expr: &Literal) -> RuntimeResult<LiteralValue> {
		Ok(expr.value)
	}

	fn visit_unary(&self, expr: &Unary) -> RuntimeResult<LiteralValue> {
		let right = expr.right.accept(self)?;

		match right {
			LiteralValue::Bool(b) if matches!(expr.operator.token_type, Tokens::Not) => {
				return Ok(LiteralValue::Bool(!b));
			},
			LiteralValue::Bool(_) => return Err(RuntimeError::InvalidUnaryLiteral(right)),

			LiteralValue::Num(n) if matches!(expr.operator.token_type, Tokens::Minus) => {
				return Ok(LiteralValue::Num(-n));
			},
			LiteralValue::Num(_) => return Err(RuntimeError::InvalidUnaryLiteral(right)),
			_ if matches!(expr.operator.token_type, Tokens::Minus | Tokens::Not) => {
				return Err(RuntimeError::InvalidUnaryLiteral(right))
			}
			_ => return Err(RuntimeError::InvalidUnaryOp(expr.operator.token_type))
		}
	}

	fn visit_ternary(&self, expr: &Ternary) -> RuntimeResult<LiteralValue> {
		let test = expr.accept(self)?;

		match test {
			LiteralValue::Bool(true) => return expr
		}
	}
}

pub trait Evaluable {
	fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R;
}

pub trait Expr: Display {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R
	where Self: Sized;
}



pub struct Binary {
	pub left: Box<dyn Expr>,
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Binary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_binary(self)
	}
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {} {})", self.left, self.operator.lexeme, self.right)
    }
}

// impl Evaluable for Binary {
//     fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
// 		visitor.visit_binary(self)
//     }
// }


pub struct Grouping {
	pub expression: Box<dyn Expr>,
}

impl Expr for Grouping {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_grouping(self)
	}
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({})", self.expression)
    }
}

// impl Evaluable for Grouping {
//     fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
// 		visitor.visit_grouping(self)
//     }
// }


pub struct Literal {
	pub value: LiteralValue,
}

impl Expr for Literal {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_literal(self)
	}
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value)
    }
}

// impl Evaluable for Literal {
//     fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
// 		visitor.visit_literal(self)
//     }
// }


pub struct Unary {
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Unary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_unary(self)
	}
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {})", self.operator.lexeme, self.right)
    }
}

// impl Evaluable for Unary {
//     fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
// 		visitor.visit_unary(self)
//     }
// }

pub struct Ternary {
	pub condition: Box<dyn Expr>,
	pub expr_if: Box<dyn Expr>,
	pub expr_else: Box<dyn Expr>,
}

impl Expr for Ternary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_ternary(self)
	}
}

impl Display for Ternary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} ? {} : {})", self.condition, self.expr_if, self.expr_else)
    }
}

// impl Evaluable for Ternary {
//     fn evaluate<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
// 		visitor.visit_ternary(self)
//     }
// }