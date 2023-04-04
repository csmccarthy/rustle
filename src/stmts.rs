

// use crate::evaluator::{ RuntimeValue };
use crate::declarator::{ ASTDeclarator, RuntimeDeclaration };
use crate::exprs::{ Expr };

pub trait StmtVisitor<'parser, R> {
	fn visit_expr(&mut self, expr: &'parser ExprStmt) -> R;
	fn visit_print(&mut self, expr: &'parser Print) -> R;
	fn visit_var(&mut self, expr: &'parser VarStmt) -> R;
	fn visit_block(&mut self, expr: &'parser BlockStmt) -> R;
}

pub trait Executable {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration;
}

pub trait NeedsSemicolon {
    fn needs_semicolon(&self) -> bool;
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
