

// use crate::evaluator::{ RuntimeValue };
use crate::declarator::{ ASTDeclarator, RuntimeDeclaration };
use crate::exprs::{ Expr };

pub trait StmtVisitor<'parser, R> {
	fn visit_expr(&mut self, expr: &'parser ExprStmt) -> R;
	fn visit_print(&mut self, expr: &'parser Print) -> R;
	fn visit_var(&mut self, expr: &'parser VarStmt) -> R;
}

pub trait Executable {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration;
}

pub trait Stmt: Executable {}


pub struct ExprStmt {
	pub expression: Box<dyn Expr>,
}

impl Stmt for ExprStmt {}

impl Executable for ExprStmt {
	fn execute<'declarator, 'parser>(&'parser self, visitor: &'declarator mut ASTDeclarator<'parser>) -> RuntimeDeclaration {
		visitor.visit_expr(self)
	}
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
