

// use crate::evaluator::{ RuntimeValue };
use crate::declarator::{ ASTDeclarator, RuntimeDeclaration };
use crate::environment::Environment;
use crate::exprs::{ Expr };
use crate::scanner::Token;

// use std::cell::{ RefCell };
use std::sync::Arc;

pub trait StmtVisitor<'parser, R> {
	fn visit_expr(&mut self, expr: &'parser ExprStmt) -> R;
	fn visit_print(&mut self, expr: &'parser Print) -> R;
	fn visit_var(&mut self, expr: &'parser VarStmt) -> R;
	fn visit_block(&mut self, expr: &'parser BlockStmt) -> R;
	fn visit_if(&mut self, expr: &'parser IfStmt) -> R;
	fn visit_while(&mut self, expr: &'parser WhileLoop) -> R;
	fn visit_for(&mut self, expr: &'parser ForLoop) -> R;
	fn visit_fxn(&mut self, expr: &'parser FunStmt) -> R;
	fn visit_return(&mut self, expr: &'parser ReturnStmt) -> R;
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

pub trait Stmt: Executable+NeedsSemicolon {}


pub struct ExprStmt {
	pub expression: Box<dyn Expr>,
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


pub struct Print {
    pub expression: Box<dyn Expr>,
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


pub struct VarStmt {
    pub name: String,
	pub expression: Box<dyn Expr>,
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


pub struct BlockStmt {
    pub stmts: Vec<Box<dyn Stmt>>,
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


pub struct IfStmt {
    pub condition: Box<dyn Expr>,
    pub stmt_if: Box<dyn Stmt>,
    pub stmt_else: Option<Box<dyn Stmt>>,
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


pub struct WhileLoop {
    pub condition: Box<dyn Expr>,
    pub block: Box<dyn Stmt>,
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


pub struct ForLoop {
    pub init: Box<dyn Stmt>,
    pub condition: Box<dyn Expr>,
    pub incrementor: Box<dyn Expr>,
    pub block: Box<dyn Stmt>,
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

// #[derive(Clone)]
pub struct FunStmtAux {
    pub name: Token,
    pub params: Vec<Token>,
    pub block: Box<dyn Stmt>,
}


pub struct FunStmt {
	pub aux: Arc<FunStmtAux>,
	pub closure: Option<Environment>,
}

impl FunStmt {
	pub fn new(name: Token, params: Vec<Token>, block: Box<dyn Stmt>) -> FunStmt {
		FunStmt { aux: Arc::new(FunStmtAux { name, params, block }), closure: None }
	}

	pub fn new_boxed(name: Token, params: Vec<Token>, block: Box<dyn Stmt>) -> Box<FunStmt> {
		Box::new(FunStmt::new(name, params, block))
	}
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


pub struct ReturnStmt {
    pub expression: Option<Box<dyn Expr>>,
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

