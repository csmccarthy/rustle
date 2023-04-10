use crate::scanner::{ Token, Literal as LiteralValue };
use crate::evaluator::{ ASTEvaluator, RuntimeValue };
// use crate::declarator::{ ASTDeclarator, RuntimeDeclaration };
use crate::stmts::{ Stmt, FunStmt };
// use crate::environment::Environment;
use std::fmt::Display;
// use std::rc::{ Rc };

pub trait ExprVisitor<'parser, R> {
	fn visit_binary<'evaluator>(&'evaluator mut self, expr: &'parser Binary) -> R;
	fn visit_grouping<'evaluator>(&'evaluator mut self, expr: &'parser Grouping) -> R;
	fn visit_literal<'evaluator>(&'evaluator mut self, expr: &'parser Literal) -> R;
	fn visit_unary<'evaluator>(&'evaluator mut self, expr: &'parser Unary) -> R;
	fn visit_ternary<'evaluator>(&'evaluator mut self, expr: &'parser Ternary) -> R;
	fn visit_assign<'evaluator>(&'evaluator mut self, expr: &'parser Assign) -> R;
	fn visit_variable<'evaluator>(&'evaluator mut self, expr: &'parser Variable) -> R;
	fn visit_or<'evaluator>(&'evaluator mut self, expr: &'parser OrExpr) -> R;
	fn visit_and<'evaluator>(&'evaluator mut self, expr: &'parser AndExpr) -> R;
	fn visit_call<'evaluator>(&'evaluator mut self, expr: &'parser Call) -> R;
	fn visit_lambda<'evaluator>(&'evaluator mut self, expr: &'parser Lambda) -> R;
}

pub trait Evaluable {
	fn evaluate<'evaluator, 'declarator, 'parser>(
		&'parser self,
		visitor: &'evaluator mut ASTEvaluator<'declarator>
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

impl Binary {
	pub fn boxed_new(left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>) -> Box<Binary> {
		Box::new(Binary { left, operator, right })
	}
}

impl Expr for Binary {}

impl AssignmentTarget for Binary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Binary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Grouping {
	pub fn boxed_new(expression: Box<dyn Expr>) -> Box<Grouping> {
		Box::new(Grouping { expression })
	}
}

impl Expr for Grouping {}

impl AssignmentTarget for Grouping {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Grouping {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Literal {
	pub fn boxed_new(value: LiteralValue) -> Box<Literal> {
		Box::new(Literal { value })
	}
}

impl Expr for Literal {}

impl AssignmentTarget for Literal {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Literal {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Unary {
	pub fn boxed_new(operator: Token, right: Box<dyn Expr>) -> Box<Unary> {
		Box::new(Unary { operator, right })
	}
}

impl Expr for Unary {}

impl AssignmentTarget for Unary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Unary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Ternary {
	pub fn boxed_new(condition: Box<dyn Expr>, expr_if: Box<dyn Expr>, expr_else: Box<dyn Expr>) -> Box<Ternary> {
		Box::new(Ternary { condition, expr_if, expr_else })
	}
}

impl Expr for Ternary {}

impl AssignmentTarget for Ternary {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Ternary {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Assign {
	pub fn boxed_new(identifier: Token, expression: Box<dyn Expr>) -> Box<Assign> {
		Box::new(Assign { identifier, expression })
	}
}

impl Expr for Assign {}

impl AssignmentTarget for Assign {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Assign {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
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

impl Variable {
	pub fn boxed_new(identifier: Token) -> Box<Variable> {
		Box::new(Variable { identifier })
	}
}

impl Expr for Variable {}

impl AssignmentTarget for Variable {
	fn assignment_target(&self) -> Option<Token> { Some(self.identifier.clone()) }
}

impl Evaluable for Variable {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_variable(self)
	}
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "var {}", self.identifier.lexeme)
    }
}


pub struct OrExpr { // TODO: probably merge with or
	pub left: Box<dyn Expr>,
	pub right: Box<dyn Expr>,
}

impl OrExpr {
	pub fn boxed_new(left: Box<dyn Expr>, right: Box<dyn Expr>) -> Box<OrExpr> {
		Box::new(OrExpr { left, right })
	}
}

impl Expr for OrExpr {}

impl AssignmentTarget for OrExpr {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for OrExpr {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_or(self)
	}
}

impl Display for OrExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {})", self.left, self.right)
    }
}


pub struct AndExpr {
	pub left: Box<dyn Expr>,
	pub right: Box<dyn Expr>,
}

impl AndExpr {
	pub fn boxed_new(left: Box<dyn Expr>, right: Box<dyn Expr>) -> Box<AndExpr> {
		Box::new(AndExpr { left, right })
	}
}

impl Expr for AndExpr {}

impl AssignmentTarget for AndExpr {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for AndExpr {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_and(self)
	}
}

impl Display for AndExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {})", self.left, self.right)
    }
}


pub struct Call {
	pub identifier: Token,
	pub args: Vec<Box<dyn Expr>>,
}

impl Call {
	pub fn boxed_new(identifier: Token, args: Vec<Box<dyn Expr>>) -> Box<Call> {
		Box::new(Call { identifier, args })
	}
}

impl Expr for Call {}

impl AssignmentTarget for Call {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Call {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_call(self)
	}
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} (", self.identifier)?;
		for expr in &self.args {
			write!(f, "{}", expr)?;
		}
		write!(f, ")")
    }
}

pub struct Lambda {
	pub stmt: FunStmt
}

impl Lambda {
	pub fn boxed_new(params: Vec<Token>, block: Box<dyn Stmt>) -> Box<Lambda> {
		let token = Token {
			token_type: crate::scanner::Tokens::Identifier,
			literal: LiteralValue::Str(String::from("lambda")),
			lexeme: String::from("lambda"),
			line: 0
		};
		let stmt = FunStmt::new(token, params, block);
		Box::new(Lambda { stmt })
	}
}


impl Expr for Lambda {}

impl Clone for Lambda {
	fn clone(&self) -> Self {
		Lambda { stmt: self.stmt.clone() }
	}
}

impl AssignmentTarget for Lambda {
	fn assignment_target(&self) -> Option<Token> { None }
}

impl Evaluable for Lambda {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_lambda(self)
	}
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "lmbda (")?;
		for expr in &self.stmt.aux.params {
			write!(f, "{}", expr)?;
		}
		write!(f, ")")
    }
}