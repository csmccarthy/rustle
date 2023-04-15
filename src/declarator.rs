
use crate::scanner::Literal;
use crate::stmts::{ Stmt, StmtVisitor, ExprStmt, Print, VarStmt, BlockStmt, IfStmt, WhileLoop, ForLoop, FunStmt, ReturnStmt, BreakStmt, ContinueStmt, ClassStmt, InstantiationStmt };
use crate::environment::{ Environment, PropertyStore };
use crate::evaluator::{ ASTEvaluator, RuntimeResult, RuntimeError };

use std::collections::{ HashMap };

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
        println!("> {}", val);
        Ok(())
    }

	fn visit_var(&mut self, stmt: &'parser VarStmt) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        let val = stmt.expression.evaluate(&mut eval)?;
        self.stack.define(stmt.name.lexeme.clone(), val)?;
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
        match val {
            Literal::Bool(true) => stmt.stmt_if.execute(self)?,
            Literal::Bool(false) => {
                if let Some(st) = &stmt.stmt_else { st.execute(self)?; }
            },
            _ => return Err(RuntimeError::InvalidConditional(val.clone())),
        }
        Ok(())
    }

	fn visit_while(&mut self, stmt: &'parser WhileLoop) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        let mut val = stmt.condition.evaluate(&mut eval)?;
        if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        while let Literal::Bool(b) = val {
            if !b { break; }
            if let Err(e) = stmt.block.execute(self) {
                match e {
                    RuntimeError::Break => break,
                    RuntimeError::Continue => (), // TODO investigate if condition is checked
                    err => return Err(err)
                }
            }
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
            if let Err(e) = stmt.block.execute(self) {
                match e {
                    RuntimeError::Break => break,
                    RuntimeError::Continue => (), // TODO investigate if the incrementor still needs to run
                    err => return Err(err)
                }
            }
            let mut eval = ASTEvaluator::new(self.stack);
            stmt.incrementor.evaluate(&mut eval)?;
            val = stmt.condition.evaluate(&mut eval)?;
            if !matches!(val, Literal::Bool(_)) { return Err(RuntimeError::InvalidConditional(val.clone())) }
        }
        Ok(())
    }

	fn visit_fxn(&mut self, stmt: &'parser FunStmt) -> RuntimeDeclaration {
        self.stack.store_fxn(stmt);
        Ok(())
    }

	fn visit_return(&mut self, stmt: &'parser ReturnStmt) -> RuntimeDeclaration {
        let mut eval = ASTEvaluator::new(self.stack);
        match &stmt.expression {
            None => Err(RuntimeError::Return(Literal::Nil)),
            Some(expr) => {
                let val = expr.evaluate(&mut eval)?;
                Err(RuntimeError::Return(val))
            }
        }
    }

	fn visit_break(&mut self, _stmt: &'parser BreakStmt) -> RuntimeDeclaration {
        Err(RuntimeError::Break)
    }

	fn visit_continue(&mut self, _stmt: &'parser ContinueStmt) -> RuntimeDeclaration {
        Err(RuntimeError::Continue)
    }

	fn visit_class(&mut self, stmt: &'parser ClassStmt) -> RuntimeDeclaration {
        let mut prop_map: PropertyStore = HashMap::new();

        let mut super_uid_opt: Option<usize> = None;
        if let Some(super_expr) = &stmt.super_expr {
            let mut eval = ASTEvaluator::new(self.stack);
            let super_lit = super_expr.evaluate(&mut eval)?;
            match super_lit {
                Literal::Class(uid, idx) => {
                    super_uid_opt = Some(uid);
                    self.stack.inherit_methods(&uid, &mut prop_map);
                    self.stack.define(String::from("super"), Literal::Class(uid, idx))?;
                },
                lit => return Err(RuntimeError::InvalidSuperClass(lit.clone())),
            }
        }

        let mut method_idx_set = HashMap::new();
        for method in &stmt.methods {
            let idx = self.stack.store_fxn(method);
            method_idx_set.insert(method.aux.name.lexeme.clone(), idx);
            prop_map.insert(method.aux.name.lexeme.clone(), Literal::Func(idx, None));
        }
        let class_uid = self.stack.assign_methods(method_idx_set);

        let mut stmts: Vec<Box<dyn Stmt>> = Vec::new();

        let mut params = Vec::new();
        if let Some(init) = &prop_map.get("init") {
            let init_params = match init {
                Literal::Func(fxn_uid, _) => {
                    self.stack.get_params(fxn_uid)?
                }
                _ => { return Ok(()); },
            };
            params = init_params;
        }
        let constructor_stmt = InstantiationStmt::boxed_new(stmt.name.clone(), prop_map);
        stmts.push(constructor_stmt);

        let block = BlockStmt::boxed_new(stmts);
        let constructor_fxn = FunStmt::new(stmt.name.clone(), params, block);
        let constructor_uid = self.stack.store_fxn(&constructor_fxn);
        self.stack.define(stmt.name.lexeme.clone(), Literal::Class(class_uid, constructor_uid))?;
        if let Some(super_uid) = super_uid_opt { self.stack.add_generation(&class_uid, &super_uid); }
        Ok(())
    }

	fn visit_instantiation(&mut self, stmt: &'parser InstantiationStmt) -> RuntimeDeclaration {
        let uid = self.stack.instantiate(stmt)?;
        let instance = Literal::Instance(stmt.name.lexeme.clone(), uid);
        Err(RuntimeError::Return(instance))
    }
}