use crate::scanner::Literal;
use crate::stmts::{ StmtVisitor, ExprStmt, Print, VarStmt, BlockStmt, IfStmt, WhileLoop, ForLoop };
// use crate::exprs::{ Expr };
// use crate::scanner::{ Literal };
use crate::environment::{ Environment };
use crate::evaluator::{ ASTEvaluator, RuntimeResult, RuntimeError };
// use std::collections::HashMap;

pub struct ASTDeclarator<'parser> {
    stack: &'parser mut Environment
}

pub type RuntimeDeclaration = RuntimeResult<()>;

impl<'parser> ASTDeclarator<'parser> {
    pub fn new(stack: &'parser mut Environment) -> ASTDeclarator<'parser> {
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
        let mut eval = ASTEvaluator::new(self.stack);
        let val = stmt.expression.evaluate(&mut eval)?;
        self.stack.define(stmt.name.clone(), val)?;
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

	fn visit_if(&mut self, stmt: &'parser IfStmt) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        let val = stmt.condition.evaluate(&mut eval)?;
        self.stack.nest();
        match val {
            Literal::Bool(true) => stmt.stmt_if.execute(self)?,
            Literal::Bool(false) => {
                if let Some(st) = &stmt.stmt_else { st.execute(self)?; }
            },
            _ => return Err(RuntimeError::InvalidConditional(val.clone())),
        }
        self.stack.unnest();
        Ok(())
    }

	fn visit_while(&mut self, stmt: &'parser WhileLoop) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        let mut val = stmt.condition.evaluate(&mut eval)?;
        if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        while let Literal::Bool(b) = val {
            if !b { break; }
            stmt.block.execute(self)?;
            let mut eval = ASTEvaluator::new(self.stack);
            val = stmt.condition.evaluate(&mut eval)?;
            if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        }
        Ok(())
    }

	fn visit_for(&mut self, stmt: &'parser ForLoop) -> RuntimeDeclaration {
        stmt.init.execute(self)?;
        let mut eval = ASTEvaluator::new(self.stack);
        let mut val = stmt.condition.evaluate(&mut eval)?;
        if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        while let Literal::Bool(b) = val {
            if !b { break; }
            stmt.block.execute(self)?;
            let mut eval = ASTEvaluator::new(self.stack);
            stmt.incrementor.evaluate(&mut eval)?;
            val = stmt.condition.evaluate(&mut eval)?;
            if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        }
        Ok(())
    }
}