use crate::stmts::{ StmtVisitor, ExprStmt, Print, VarStmt };
use crate::exprs::{ Expr };
// use crate::scanner::{ Literal };
use crate::evaluator::{ ASTEvaluator, RuntimeResult };
use std::collections::HashMap;

pub struct ASTDeclarator<'parser> {
    stack: HashMap<String, &'parser Box<dyn Expr>>
}

pub type RuntimeDeclaration = RuntimeResult<()>;

impl<'parser> ASTDeclarator<'parser> {
    pub fn new() -> ASTDeclarator<'parser> {
        ASTDeclarator { stack: HashMap::new() }
    }
}

impl<'parser> StmtVisitor<'parser, RuntimeDeclaration> for ASTDeclarator<'parser> {
	fn visit_expr(&mut self, stmt: &'parser ExprStmt) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(&mut self.stack);
        stmt.expression.evaluate(&mut eval)?;
        Ok(())
    }

	fn visit_print(&mut self, stmt: &'parser Print) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(&mut self.stack);
        let val = stmt.expression.evaluate(&mut eval)?;
        println!("{}", val);
        Ok(())
    }

	fn visit_var(&mut self, stmt: &'parser VarStmt) -> RuntimeDeclaration {
        // println!("here");
        // let eval = ASTEvaluator::new(&mut self.stack);
        // let val = stmt.expression.evaluate(&eval)?; // This will change if functions are first class
        self.stack.insert(stmt.name.clone(), &stmt.expression);
        Ok(())
    }
}