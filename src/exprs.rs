use crate::scanner::{ Token, Literal as LiteralValue };
use std::fmt::Display;
use crate::evaluator::{ ASTEvaluator, RuntimeValue };

pub trait ExprVisitor<R> {
	fn visit_binary(&self, expr: &Binary) -> R;
	fn visit_grouping(&self, expr: &Grouping) -> R;
	fn visit_literal(&self, expr: &Literal) -> R;
	fn visit_unary(&self, expr: &Unary) -> R;
	fn visit_ternary(&self, expr: &Ternary) -> R;
}

pub trait Evaluable {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue;
}

pub trait Expr: Display+Evaluable {}


pub struct Binary {
	pub left: Box<dyn Expr>,
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Binary {}

impl Evaluable for Binary {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue {
		visitor.visit_binary(self)
	}
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {} {})", self.left, self.operator.lexeme, self.right)
    }
}


pub struct Grouping {
	pub expression: Box<dyn Expr>,
}

impl Expr for Grouping {}

impl Evaluable for Grouping {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue {
		visitor.visit_grouping(self)
	}
}

impl Display for Grouping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({})", self.expression)
    }
}


pub struct Literal {
	pub value: LiteralValue,
}

impl Expr for Literal {}

impl Evaluable for Literal {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue {
		visitor.visit_literal(self)
	}
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value)
    }
}


pub struct Unary {
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Unary {}

impl Evaluable for Unary {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue {
		visitor.visit_unary(self)
	}
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {})", self.operator.lexeme, self.right)
    }
}


pub struct Ternary {
	pub condition: Box<dyn Expr>,
	pub expr_if: Box<dyn Expr>,
	pub expr_else: Box<dyn Expr>,
}

impl Expr for Ternary {}

impl Evaluable for Ternary {
	fn evaluate(&self, visitor: &ASTEvaluator) -> RuntimeValue {
		visitor.visit_ternary(self)
	}
}

impl Display for Ternary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} ? {} : {})", self.condition, self.expr_if, self.expr_else)
    }
}