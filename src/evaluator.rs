

use crate::scanner::{ Tokens, Literal as LiteralValue };
use crate::exprs::{ Expr, ExprVisitor, Binary, Grouping, Literal, Unary, Ternary, Variable, Assign };
use std::collections::HashMap;

#[derive(Debug)]
pub enum RuntimeError {
	InvalidBinaryOp(Tokens),
	InvalidBinaryLiteral(LiteralValue),
	InvalidConditional(LiteralValue),
	InvalidUnaryOp(Tokens),
	InvalidUnaryLiteral(LiteralValue),
	UndefinedVariable(String),
}

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;
pub type RuntimeValue = RuntimeResult<LiteralValue>;

pub struct ASTEvaluator<'declarator, 'parser> {
    stack: &'declarator mut HashMap<String, &'parser Box<dyn Expr>>
}

impl<'declarator, 'parser> ASTEvaluator<'declarator, 'parser> {
    pub fn new(map: &'declarator mut HashMap<String, &'parser Box<dyn Expr>>)
    -> ASTEvaluator<'declarator, 'parser>
    {
        ASTEvaluator { stack: map }
    }

    fn num_operate<F, G, H>(&'declarator self, r: LiteralValue, result: G, op_cb: F) -> RuntimeValue
    where F: Fn(f64) -> H, G: Fn(H) -> LiteralValue
    {
        return match r {
            LiteralValue::Num(b) => Ok(result(op_cb(b))),
            _ => Err(RuntimeError::InvalidBinaryLiteral(r))
        }
    }

    fn str_operate<F, G, H>(&self, r: LiteralValue, result: G, op_cb: F) -> RuntimeValue
    where F: Fn(String) -> H, G: Fn(H) -> LiteralValue
    {
        return match r {
            LiteralValue::Str(b) => Ok(result(op_cb(b))),
            _ => Err(RuntimeError::InvalidBinaryLiteral(r))
        }
    }

    fn bool_operate<F, G, H>(&self, r: LiteralValue, result: G, op_cb: F) -> RuntimeValue
    where F: Fn(bool) -> H, G: Fn(H) -> LiteralValue
    {
        return match r {
            LiteralValue::Bool(b) => Ok(result(op_cb(b))),
            _ => Err(RuntimeError::InvalidBinaryLiteral(r))
        }
    }
}

impl<'declarator, 'parser> ExprVisitor<'declarator, 'parser, RuntimeValue> for ASTEvaluator<'declarator, 'parser> {
	fn visit_binary(&'declarator mut self, expr: &'parser Binary) -> RuntimeValue {
		let left = expr.left.evaluate(self)?;
		let right = expr.right.evaluate(self)?;

        use crate::scanner::Literal::*;

		return match expr.operator.token_type {
			Tokens::Plus => { match left {
				Num(n1) => { Ok(self.num_operate(right, Num, |n2| n1 + n2)?) },
				Str(s1) => { Ok(self.str_operate(right, Str, |s2| s1.clone() + &s2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Minus => { match left {
				Num(n1) => { Ok(self.num_operate(right, Num, |n2| n1 - n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Star => { match left {
				Num(n1) => { Ok(self.num_operate(right, Num, |n2| n1 * n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Slash => { match left {
				Num(n1) => { Ok(self.num_operate(right, Num, |n2| n1 / n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Less => { match left {
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 < n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Greater => { match left {
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 > n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::LessOrEqual => { match left {
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 <= n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::GreaterOrEqual => { match left {
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 >= n2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
			} },

			Tokens::Equality => { match left {
				Bool(b1) => { Ok(self.bool_operate(right, Bool, |b2| b1 == b2)?) },
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 == n2)?) },
				Str(s1) => { Ok(self.str_operate(right, Bool, |s2| s1 == s2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
            } },

			Tokens::Inequality => { match left {
				Bool(b1) => { Ok(self.bool_operate(right, Bool, |b2| b1 != b2)?) },
				Num(n1) => { Ok(self.num_operate(right, Bool, |n2| n1 != n2)?) },
				Str(s1) => { Ok(self.str_operate(right, Bool, |s2| s1 != s2)?) },
				_ => Err(RuntimeError::InvalidBinaryLiteral(left)),
            } },

			Tokens::Comma => Ok(right),
			_ => return Err(RuntimeError::InvalidBinaryOp(expr.operator.token_type.clone()))
		}
	}

	fn visit_grouping(&'declarator mut self, expr: &'parser Grouping) -> RuntimeValue {
		expr.expression.evaluate(self)
	}

	fn visit_literal(&'declarator mut self, expr: &'parser Literal) -> RuntimeValue {
		Ok(expr.value.clone())
	}

	fn visit_unary(&'declarator mut self, expr: &'parser Unary) -> RuntimeValue {
		let right = expr.right.evaluate(self)?;

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
			_ => return Err(RuntimeError::InvalidUnaryOp(expr.operator.token_type.clone()))
		}
	}

	fn visit_ternary(&mut self, expr: &'parser Ternary) -> RuntimeValue {
		let test = expr.condition.evaluate(self)?;

        // self.clone();
		match test {
			LiteralValue::Bool(true) => return Ok(expr.expr_if.evaluate(self)?),
			LiteralValue::Bool(false) => return Ok(expr.expr_else.evaluate(self)?),
			_ => return Err(RuntimeError::InvalidConditional(test.clone()))
		}
	}

	fn visit_variable(&'declarator mut self, expr: &Variable) -> RuntimeValue {
		let stack_opt = self.stack.get(&expr.identifier.lexeme.clone());
        if let None = stack_opt {
            return Err(RuntimeError::UndefinedVariable(expr.identifier.lexeme.clone()));
        }
        Ok(stack_opt.unwrap().evaluate(self)?)
	}

	fn visit_assign(&mut self, expr: &'parser Assign) -> RuntimeValue {
		self.stack.insert(expr.identifier.lexeme.clone(), &expr.expression);
        Ok(LiteralValue::Nil)
	}
}