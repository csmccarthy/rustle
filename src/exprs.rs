use crate::scanner::{ Token, Literal as LiteralValue };
use std::fmt::Display;
use crate::evaluator::{ ASTEvaluator, RuntimeValue };

pub trait ExprVisitor<'parser, R> {
	fn visit_binary<'evaluator>(&'evaluator mut self, expr: &'parser Binary) -> R;
	fn visit_grouping<'evaluator>(&'evaluator mut self, expr: &'parser Grouping) -> R;
	fn visit_literal<'evaluator>(&'evaluator mut self, expr: &'parser Literal) -> R;
	fn visit_unary<'evaluator>(&'evaluator mut self, expr: &'parser Unary) -> R;
	fn visit_ternary<'evaluator>(&'evaluator mut self, expr: &'parser Ternary) -> R;
	fn visit_assign<'evaluator>(&'evaluator mut self, expr: &'parser Assign) -> R;
	fn visit_variable<'evaluator>(&'evaluator mut self, expr: &'parser Variable) -> R;
}

pub trait Evaluable {
	fn evaluate<'evaluator, 'declarator, 'parser>(
		&'parser self,
		visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>
	) -> RuntimeValue;
}

pub trait AssignmentTarget {
	fn assignment_target(&self) -> Option<Token>;
}

pub trait Expr: Display+Evaluable+AssignmentTarget {}


pub struct Binary {
	pub left: Box<dyn Expr>,
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Binary {}

impl AssignmentTarget for Binary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Binary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
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

impl AssignmentTarget for Grouping {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Grouping {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
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

impl AssignmentTarget for Literal {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Literal {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
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

impl AssignmentTarget for Unary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Unary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
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

impl AssignmentTarget for Ternary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Ternary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
		visitor.visit_ternary(self)
	}
}

impl Display for Ternary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} ? {} : {})", self.condition, self.expr_if, self.expr_else)
    }
}


pub struct Assign {
	pub identifier: Token,
	pub expression: Box<dyn Expr>,
}

impl Expr for Assign {}

impl AssignmentTarget for Assign {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Assign {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
		visitor.visit_assign(self)
	}
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} = {})", self.identifier.lexeme, self.expression)
    }
}


pub struct Variable {
	pub identifier: Token,
}

impl Expr for Variable {}

impl AssignmentTarget for Variable {
	fn assignment_target(&self) -> Option<Token> { Some(self.identifier.clone()) }
}

impl Evaluable for Variable {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator, 'parser>) -> RuntimeValue {
		visitor.visit_variable(self)
	}
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "var {}", self.identifier.lexeme)
    }
}