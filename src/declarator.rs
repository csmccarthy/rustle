use crate::stmts::{ StmtVisitor, ExprStmt, Print, VarStmt, BlockStmt };
// use crate::exprs::{ Expr };
// use crate::scanner::{ Literal };
use crate::environment::{ Environment };
use crate::evaluator::{ ASTEvaluator, RuntimeResult };
// use std::collections::HashMap;

pub struct ASTDeclarator<'parser> {
    stack: &'parser mut Environment<'parser>
}

pub type RuntimeDeclaration = RuntimeResult<()>;

impl<'parser> ASTDeclarator<'parser> {
    pub fn new(stack: &'parser mut Environment<'parser>) -> ASTDeclarator<'parser> {
        ASTDeclarator { stack }
    }
}

impl<'parser> StmtVisitor<'parser, RuntimeDeclaration> for ASTDeclarator<'parser> {
	fn visit_expr(&mut self, stmt: &'parser ExprStmt) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        stmt.expression.evaluate(&mut eval)?;
        Ok(())
    }

	fn visit_print(&mut self, stmt: &'parser Print) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        let val = stmt.expression.evaluate(&mut eval)?;
        println!("{}", val);
        Ok(())
    }

	fn visit_var(&mut self, stmt: &'parser VarStmt) -> RuntimeDeclaration {
        self.stack.define(stmt.name.clone(), &stmt.expression)?;
        Ok(())
    }

	fn visit_block(&mut self, stmt: &'parser BlockStmt) -> RuntimeDeclaration {
        self.stack.nest();
        for st in &stmt.stmts {
            match st.execute(self) {
                Ok(_) => (),
                e => { self.stack.unnest(); return e; }
            }
        }
        self.stack.unnest();
        Ok(())
    }
}