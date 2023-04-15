

// use crate::evaluator::{ RuntimeValue };
use crate::declarator::{ ASTDeclarator, RuntimeDeclaration };
use crate::environment::{Environment, PropertyStore};
use crate::exprs::{ Expr };
use crate::scanner::Token;
use crate::analyzer::{ ASTAnalyzer, Analyzed, SemanticResult };

// use std::cell::{ RefCell };
// use std::sync::Arc;
use std::rc::{ Rc };

pub trait StmtVisitor<'parser, R> {
	fn visit_expr(&mut self, stmt: &'parser ExprStmt) -> R;
	fn visit_print(&mut self, stmt: &'parser Print) -> R;
	fn visit_var(&mut self, stmt: &'parser VarStmt) -> R;
	fn visit_block(&mut self, stmt: &'parser BlockStmt) -> R;
	fn visit_if(&mut self, stmt: &'parser IfStmt) -> R;
	fn visit_while(&mut self, stmt: &'parser WhileLoop) -> R;
	fn visit_for(&mut self, stmt: &'parser ForLoop) -> R;
	fn visit_fxn(&mut self, stmt: &'parser FunStmt) -> R;
	fn visit_return(&mut self, stmt: &'parser ReturnStmt) -> R;
	fn visit_break(&mut self, stmt: &'parser BreakStmt) -> R;
	fn visit_continue(&mut self, stmt: &'parser ContinueStmt) -> R;
	fn visit_class(&mut self, stmt: &'parser ClassStmt) -> R;
	fn visit_instantiation(&mut self, stmt: &'parser InstantiationStmt) -> R;
}

pub trait Executable {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration;
}

pub trait NeedsSemicolon {
    fn needs_semicolon(&self) -> bool;
}

pub trait FunctionDef {
    fn function_def(&self) -> Option<FunStmt>;
}

pub trait Stmt: Executable+NeedsSemicolon+Analyzed {}


pub struct ExprStmt {
	pub expression: Box<dyn Expr>,
}

impl ExprStmt {
	pub fn boxed_new(expression: Box<dyn Expr>) -> Box<ExprStmt> {
		Box::new(ExprStmt { expression })
	}
}

impl Stmt for ExprStmt {}

impl Executable for ExprStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_expr(self)
	}
}

impl NeedsSemicolon for ExprStmt {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for ExprStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for ExprStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_expr(self)
	}
}


pub struct Print {
    pub expression: Box<dyn Expr>,
}

impl Print {
	pub fn boxed_new(expression: Box<dyn Expr>) -> Box<Print> {
		Box::new(Print { expression })
	}
}

impl Stmt for Print {}

impl Executable for Print {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_print(self)
	}
}

impl NeedsSemicolon for Print {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for Print {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for Print {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_print(self)
	}
}


pub struct VarStmt {
    pub name: Token, // TODO: Identifier
	pub expression: Box<dyn Expr>, // TODO: Change to option
}

impl VarStmt {
	pub fn boxed_new(expression: Box<dyn Expr>, name: Token) -> Box<VarStmt> {
		Box::new(VarStmt { expression, name })
	}
}

impl Stmt for VarStmt {}

impl Executable for VarStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_var(self)
	}
}

impl NeedsSemicolon for VarStmt {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for VarStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for VarStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_var(self)
	}
}


pub struct BlockStmt {
    pub stmts: Vec<Box<dyn Stmt>>,
}

impl BlockStmt {
	pub fn boxed_new(stmts: Vec<Box<dyn Stmt>>) -> Box<BlockStmt> {
		Box::new(BlockStmt { stmts })
	}
}

impl Stmt for BlockStmt {}

impl Executable for BlockStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_block(self)
	}
}

impl NeedsSemicolon for BlockStmt {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for BlockStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for BlockStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_block(self)
	}
}


pub struct IfStmt {
    pub condition: Box<dyn Expr>,
    pub stmt_if: Box<dyn Stmt>,
    pub stmt_else: Option<Box<dyn Stmt>>,
}

impl IfStmt {
	pub fn boxed_new(condition: Box<dyn Expr>, stmt_if: Box<dyn Stmt>, stmt_else: Option<Box<dyn Stmt>>) -> Box<IfStmt> {
		Box::new(IfStmt { condition, stmt_if, stmt_else })
	}
}

impl Stmt for IfStmt {}

impl Executable for IfStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_if(self)
	}
}

impl NeedsSemicolon for IfStmt {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for IfStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for IfStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_if(self)
	}
}


pub struct WhileLoop {
    pub condition: Box<dyn Expr>,
    pub block: Box<dyn Stmt>,
}

impl WhileLoop {
	pub fn boxed_new(condition: Box<dyn Expr>, block: Box<dyn Stmt>) -> Box<WhileLoop> {
		Box::new(WhileLoop { condition, block })
	}
}

impl Stmt for WhileLoop {}

impl Executable for WhileLoop {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_while(self)
	}
}

impl NeedsSemicolon for WhileLoop {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for WhileLoop {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for WhileLoop {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_while(self)
	}
}


pub struct ForLoop {
    pub init: Box<dyn Stmt>,
    pub condition: Box<dyn Expr>,
    pub incrementor: Box<dyn Expr>,
    pub block: Box<dyn Stmt>,
}

impl ForLoop {
	pub fn boxed_new(init: Box<dyn Stmt>, condition: Box<dyn Expr>, incrementor: Box<dyn Expr>, block: Box<dyn Stmt>) -> Box<ForLoop> {
		Box::new(ForLoop { init, condition, incrementor, block })
	}
}

impl Stmt for ForLoop {}

impl Executable for ForLoop {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_for(self)
	}
}

impl NeedsSemicolon for ForLoop {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for ForLoop {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for ForLoop {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_for(self)
	}
}

// #[derive(Clone)]
pub struct FunStmtAux {
    pub name: Token,
    pub params: Vec<Token>,
    pub block: Box<dyn Stmt>,
}


pub struct FunStmt {
	pub aux: Rc<FunStmtAux>,
	pub closure: Option<Environment>,
}

impl FunStmt {
	pub fn new(name: Token, params: Vec<Token>, block: Box<dyn Stmt>) -> FunStmt {
		FunStmt { aux: Rc::new(FunStmtAux { name, params, block }), closure: None }
	}

	// pub fn boxed_new(name: Token, params: Vec<Token>, block: Box<dyn Stmt>) -> Box<FunStmt> {
	// 	Box::new(FunStmt::new(name, params, block))
	// }
}

impl Stmt for FunStmt {}

impl Clone for FunStmt {
	fn clone(&self) -> Self {
		FunStmt { aux: self.aux.clone(), closure: self.closure.clone() }
	}
}

impl Executable for FunStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_fxn(self)
	}
}

impl NeedsSemicolon for FunStmt {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for FunStmt {
	fn function_def(&self) -> Option<FunStmt> {
	
		Some(self.clone())
	}
}

impl Analyzed for FunStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_fxn(self)
	}
}


pub struct ReturnStmt {
    pub expression: Option<Box<dyn Expr>>,
}

impl ReturnStmt {
	pub fn boxed_new(expression: Option<Box<dyn Expr>>) -> Box<ReturnStmt> {
		Box::new(ReturnStmt { expression })
	}
}

impl Stmt for ReturnStmt {}

impl Executable for ReturnStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_return(self)
	}
}

impl NeedsSemicolon for ReturnStmt {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for ReturnStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for ReturnStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_return(self)
	}
}


pub struct BreakStmt {}

impl BreakStmt {
	pub fn boxed_new() -> Box<BreakStmt> {
		Box::new(BreakStmt {})
	}
}

impl Stmt for BreakStmt {}

impl Executable for BreakStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_break(self)
	}
}

impl NeedsSemicolon for BreakStmt {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for BreakStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for BreakStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_break(self)
	}
}


pub struct ContinueStmt {}

impl ContinueStmt {
	pub fn boxed_new() -> Box<ContinueStmt> {
		Box::new(ContinueStmt {})
	}
}

impl Stmt for ContinueStmt {}

impl Executable for ContinueStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_continue(self)
	}
}

impl NeedsSemicolon for ContinueStmt {
	fn needs_semicolon(&self) -> bool { true }
}

impl FunctionDef for ContinueStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for ContinueStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_continue(self)
	}
}


pub struct ClassStmt {
	pub name: Token,
	pub super_expr: Option<Box<dyn Expr>>,
	pub super_name: Option<Token>,
	pub methods: Vec<FunStmt>,
}

impl ClassStmt {
	pub fn boxed_new(name: Token, super_expr: Option<Box<dyn Expr>>, super_name: Option<Token>, methods: Vec<FunStmt>) -> Box<ClassStmt> {
		Box::new(ClassStmt { name, super_expr, super_name, methods })
	}
}

impl Stmt for ClassStmt {}

impl Executable for ClassStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_class(self)
	}
}

impl NeedsSemicolon for ClassStmt {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for ClassStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for ClassStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_class(self)
	}
}




pub struct InstantiationStmt {
	pub name: Token,
	pub properties: PropertyStore,
}

impl InstantiationStmt {
	pub fn boxed_new(name: Token, properties: PropertyStore) -> Box<InstantiationStmt> {
		Box::new(InstantiationStmt { name, properties })
	}
}

impl Stmt for InstantiationStmt {}

impl Executable for InstantiationStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_instantiation(self)
	}
}

impl NeedsSemicolon for InstantiationStmt {
	fn needs_semicolon(&self) -> bool { false }
}

impl FunctionDef for InstantiationStmt {
	fn function_def(&self) -> Option<FunStmt> { None }
}

impl Analyzed for InstantiationStmt {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult {
		visitor.visit_instantiation(self)
	}
}