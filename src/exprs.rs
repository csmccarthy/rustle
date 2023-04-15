
use crate::scanner::{ Token, Literal as LiteralValue };
use crate::evaluator::{ ASTEvaluator, RuntimeValue };
use crate::stmts::{ Stmt, FunStmt };
use crate::analyzer::{ ASTAnalyzer, Analyzed, SemanticResult };

use std::fmt::Display;


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
	fn visit_property<'evaluator>(&'evaluator mut self, expr: &'parser Property) -> R;
	fn visit_this<'evaluator>(&'evaluator mut self, expr: &'parser This) -> R;
	fn visit_super<'evaluator>(&'evaluator mut self, expr: &'parser Super) -> R;
}

pub trait Evaluable {
	fn evaluate<'evaluator, 'declarator, 'parser>(
		&'parser self,
		visitor: &'evaluator mut ASTEvaluator<'declarator>
	) -> RuntimeValue;
}

pub enum AssignmentTarget<'a> {
	Variable(String),
	Field(&'a Box<dyn Expr>, String)
}

pub trait Assignable {
	fn assignment_target(&self) -> Option<AssignmentTarget>;
}

pub trait Expr: Display+Evaluable+Assignable+Analyzed {}


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

impl Analyzed for Binary {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_binary(self)
	}
}

impl Assignable for Binary {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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

impl Analyzed for Grouping {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_grouping(self)
	}
}

impl Assignable for Grouping {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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

impl Analyzed for Literal {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_literal(self)
	}
}

impl Assignable for Literal {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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

impl Analyzed for Unary {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_unary(self)
	}
}

impl Assignable for Unary {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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

impl Analyzed for Ternary {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_ternary(self)
	}
}

impl Assignable for Ternary {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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
	pub identifier: Box<dyn Expr>,
	pub expression: Box<dyn Expr>,
}

impl Assign {
	pub fn boxed_new(identifier: Box<dyn Expr>, expression: Box<dyn Expr>) -> Box<Assign> {
		Box::new(Assign { identifier, expression })
	}
}

impl Expr for Assign {}

impl Analyzed for Assign {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_assign(self)
	}
}

impl Assignable for Assign {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
}

impl Evaluable for Assign {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_assign(self)
	}
}

impl Display for Assign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} = {})", self.identifier, self.expression)
    }
}


pub struct Variable {
	pub identifier: Token,
}

impl Variable {
	pub fn new(identifier: Token) -> Variable {
		Variable { identifier }
	}

	pub fn boxed_new(identifier: Token) -> Box<Variable> {
		Box::new(Variable::new(identifier))
	}
}

impl Expr for Variable {}

impl Analyzed for Variable {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_variable(self)
	}
}

impl Assignable for Variable {
	fn assignment_target(&self) -> Option<AssignmentTarget> {
		Some(AssignmentTarget::Variable(self.identifier.lexeme.clone()))
	}
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

impl Analyzed for OrExpr {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_or(self)
	}
}

impl Assignable for OrExpr {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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

impl Analyzed for AndExpr {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_and(self)
	}
}

impl Assignable for AndExpr {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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
	pub identifier: Box<dyn Expr>,
	pub args: Vec<Box<dyn Expr>>,
}

impl Call {
	pub fn boxed_new(identifier: Box<dyn Expr>, args: Vec<Box<dyn Expr>>) -> Box<Call> {
		Box::new(Call { identifier, args })
	}
}

impl Expr for Call {}

impl Analyzed for Call {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_call(self)
	}
}

impl Assignable for Call {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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
			line: 0 // TODO: Fix this to use an actual token
		};
		let stmt = FunStmt::new(token, params, block);
		Box::new(Lambda { stmt })
	}
}


impl Expr for Lambda {}

impl Analyzed for Lambda {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_lambda(self)
	}
}

impl Clone for Lambda {
	fn clone(&self) -> Self {
		Lambda { stmt: self.stmt.clone() }
	}
}

impl Assignable for Lambda {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
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


pub struct Property {
	pub object: Box<dyn Expr>,
	pub identifier: Token,
}

impl Property {
	pub fn boxed_new(object: Box<dyn Expr>, identifier: Token) -> Box<Property> {
		Box::new(Property { object, identifier })
	}
}

impl Expr for Property {}

impl Analyzed for Property {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_property(self)
	}
}

impl Assignable for Property {
	fn assignment_target(&self) -> Option<AssignmentTarget> {
		Some(AssignmentTarget::Field(&self.object, self.identifier.lexeme.clone()))
	}
}

impl Evaluable for Property {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_property(self)
	}
}

impl Display for Property {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}.{}", self.object, self.identifier.lexeme)
    }
}


pub struct This {
	pub identifier: Token,
}

impl This {
	pub fn boxed_new(identifier: Token) -> Box<This> {
		Box::new(This { identifier })
	}
}

impl Expr for This {}

impl Analyzed for This {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_this(self)
	}
}

impl Assignable for This {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
}

impl Evaluable for This {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_this(self)
	}
}

impl Display for This {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "var {}", self.identifier.lexeme)
    }
}


pub struct Super {
	pub identifier: Token,
}

impl Super {
	pub fn boxed_new(identifier: Token) -> Box<Super> {
		Box::new(Super { identifier })
	}
}

impl Expr for Super {}

impl Analyzed for Super {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_super(self)
	}
}

impl Assignable for Super {
	fn assignment_target(&self) -> Option<AssignmentTarget> { None }
}

impl Evaluable for Super {
	fn evaluate<'evaluator, 'declarator, 'parser>(&'parser self, visitor: &'evaluator mut ASTEvaluator<'declarator>) -> RuntimeValue {
		visitor.visit_super(self)
	}
}

impl Display for Super {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "var {}", self.identifier.lexeme)
    }
}



