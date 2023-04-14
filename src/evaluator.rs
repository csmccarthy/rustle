

use crate::scanner::{ Tokens, Literal as LiteralValue };
use crate::exprs::{ ExprVisitor, Binary, Grouping, Literal, Unary, Ternary, Variable, Assign, OrExpr, AndExpr, Call, Lambda, Property, AssignmentTarget, This };
// use std::collections::HashMap;
use crate::environment::{ Environment };
// use crate::stmts::{ FunStmt };

// use std::sync::atomic::{ Ordering };


#[derive(Debug)]
pub enum RuntimeError {
	InvalidBinaryOp(Tokens),
	InvalidBinaryLiteral(LiteralValue),
	InvalidConditional(LiteralValue),
	InvalidUnaryOp(Tokens),
	InvalidUnaryLiteral(LiteralValue),
	UndefinedVariable(String),
	UndefinedFunction(String),
	InvalidCallable(LiteralValue),
	InvalidAccessor(LiteralValue),
	InvalidLValue(LiteralValue),
	MismatchedArguments(usize, usize),
	Return(LiteralValue),
	Break,
	Continue,
}

pub type RuntimeResult<T> = std::result::Result<T, RuntimeError>;
pub type RuntimeValue = RuntimeResult<LiteralValue>;

pub struct ASTEvaluator<'declarator> {
    stack: &'declarator mut Environment
}

impl<'declarator> ASTEvaluator<'declarator> {
    pub fn new(map: &'declarator mut Environment)
    -> ASTEvaluator<'declarator>
    {
        ASTEvaluator { stack: map }
    }

    fn num_operate<F, G, H>(& self, r: LiteralValue, result: G, op_cb: F) -> RuntimeValue
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

impl<'declarator, 'parser> ExprVisitor<'parser, RuntimeValue> for ASTEvaluator<'declarator> {
	fn visit_binary<'evaluator>(&'evaluator mut self, expr: &'parser Binary) -> RuntimeValue {
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

	fn visit_grouping<'evaluator>(&'evaluator mut self, expr: &'parser Grouping) -> RuntimeValue {
		expr.expression.evaluate(self)
	}

	fn visit_literal<'evaluator>(&'evaluator mut self, expr: &'parser Literal) -> RuntimeValue {
		Ok(expr.value.clone())
	}

	fn visit_unary<'evaluator>(&'evaluator mut self, expr: &'parser Unary) -> RuntimeValue {
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
		match test {
			LiteralValue::Bool(true) => return Ok(expr.expr_if.evaluate(self)?),
			LiteralValue::Bool(false) => return Ok(expr.expr_else.evaluate(self)?),
			_ => return Err(RuntimeError::InvalidConditional(test.clone()))
		}
	}

	fn visit_variable<'evaluator>(&'evaluator mut self, expr: &Variable) -> RuntimeValue {
		let expr = self.stack.get(&expr.identifier.lexeme)?;
        Ok(expr)
	}

	fn visit_assign(&mut self, expr: &'parser Assign) -> RuntimeValue {
		let val = expr.expression.evaluate(self)?;
		match expr.identifier.assignment_target() {
			Some(AssignmentTarget::Variable(str)) => {
				self.stack.assign(&str, val)?;
			},
			Some(AssignmentTarget::Field(inst_expr, prop)) => {
				let inst_lit = inst_expr.evaluate(self)?;
				match inst_lit {
					LiteralValue::Instance(_, inst_uid) => {
						self.stack.assign_instance(&inst_uid, prop, val)?;
					},
					_ => return Err(RuntimeError::InvalidLValue(inst_lit)),
				}
			}
			_ => return Err(RuntimeError::InvalidLValue(expr.identifier.evaluate(self)?)),
		}
		Ok(LiteralValue::Nil)
	}

	fn visit_or(&mut self, expr: &'parser OrExpr) -> RuntimeValue {
        let val = expr.left.evaluate(self)?;
        return match val {
            LiteralValue::Bool(true) => Ok(LiteralValue::Bool(true)),
            LiteralValue::Bool(false) => {
                let rval = expr.right.evaluate(self)?;
                return match rval {
                    LiteralValue::Bool(b) => Ok(LiteralValue::Bool(b)),
                    _ => Err(RuntimeError::InvalidConditional(rval.clone()))
                };
            }
            _ => return Err(RuntimeError::InvalidConditional(val.clone()))
        }
	}

	fn visit_and(&mut self, expr: &'parser AndExpr) -> RuntimeValue {
        let val = expr.left.evaluate(self)?;
        return match val {
            LiteralValue::Bool(false) => Ok(LiteralValue::Bool(false)),
            LiteralValue::Bool(true) => {
                let rval = expr.right.evaluate(self)?;
                return match rval {
                    LiteralValue::Bool(b) => Ok(LiteralValue::Bool(b)),
                    _ => Err(RuntimeError::InvalidConditional(rval.clone()))
                };
            }
            _ => return Err(RuntimeError::InvalidConditional(val.clone()))
        }
	}

	fn visit_call(&mut self, expr: &'parser Call) -> RuntimeValue {
        let mut literal_args = Vec::new();
        for arg in &expr.args {
            literal_args.push(arg.evaluate(self)?);
        }
		let literal = expr.identifier.evaluate(self)?;
		if let LiteralValue::Func(uid, opt) = literal {
			return self.stack.call_fxn(&uid, &literal_args, opt);
			// let record = map.get(&expr.identifier.lexeme);
			// if let Some(uid) = record { return Ok(LiteralValue::Func(*uid)); }
		}
		Err(RuntimeError::InvalidCallable(literal.clone()))
	}

	fn visit_lambda(&mut self, expr: &'parser Lambda) -> RuntimeValue {
		// let fxn = expr.clone();
        let fxn_uid = self.stack.store_fxn(&expr.stmt);
        Ok(LiteralValue::Func(fxn_uid, None))
	}

	fn visit_property(&mut self, expr: &'parser Property) -> RuntimeValue {
		let literal = expr.object.evaluate(self)?;
		if let LiteralValue::Instance(_, ref uid) = literal {
			return Ok(self.stack.get_field(uid, &expr.identifier.lexeme))?;
		}
		Err(RuntimeError::InvalidAccessor(literal.clone()))
	}

	fn visit_this(&mut self, _expr: &'parser This) -> RuntimeValue {
		let expr = self.stack.get("this")?;
        Ok(expr)
		// let literal = expr.object.evaluate(self)?;
		// if let LiteralValue::Instance(_, ref uid) = literal {
		// 	return Ok(self.stack.get_field(uid, &expr.identifier.lexeme))?;
		// }
		// Err(RuntimeError::InvalidAccessor(literal.clone()))
	}

	// fn visit_instantiation(&mut self, expr: &'parser Instantiation) -> RuntimeValue {
	// 	let expr = self.stack.get("this")?;
    //     Ok(expr)
    //     // let uid = self.stack.instantiate(expr)?;
    //     // Ok(LiteralValue::Instance(expr.name.lexeme.clone(), uid))
	// 	// let literal = expr.object.evaluate(self)?;
	// 	// if let LiteralValue::Instance(_, ref uid) = literal {
	// 	// 	return Ok(self.stack.get_field(uid, &expr.identifier.lexeme))?;
	// 	// }
	// 	// Err(RuntimeError::InvalidAccessor(literal.clone()))
	// }
}