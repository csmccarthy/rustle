use std::collections::HashMap;

use crate::stmts::{ Stmt, StmtVisitor, ExprStmt, Print, VarStmt, BlockStmt, IfStmt, WhileLoop, ForLoop, FunStmt, ReturnStmt, BreakStmt, ContinueStmt, ClassStmt, InstantiationStmt };
use crate::exprs::{ Expr, ExprVisitor, Binary, Grouping, Literal, Unary, Ternary, Variable, Assign, OrExpr, AndExpr, Call, Lambda, Property, This, Super };

#[derive(Debug)]
pub enum SemanticError {
    UninitializedVariable,
    ReturnOutsideFunction,
    BreakOutsideLoop,
    ContinueOutsideLoop,
    RedeclaredVariable,
    UnusedVariable,
    ThisOutsideClass,
    SuperOutsideClass,
    NonGlobalClass,
    CyclicSuperclass,
}

#[derive(Clone)]
enum FunctionType {
    None,
    Function,
    Method,
}

enum VarState {
    Declared,
    Defined,
    Accessed,
}

pub trait Analyzed {
	fn accept<'analyzer, 'parser>(&'parser self, visitor: &'analyzer mut ASTAnalyzer) -> SemanticResult;
}

pub struct ASTAnalyzer {
    scopes: Vec<HashMap<String, VarState>>,
    fxn_type: FunctionType,
    in_loop: bool,
}

impl ASTAnalyzer {
    pub fn new() -> ASTAnalyzer {
        ASTAnalyzer { scopes: Vec::new(), fxn_type: FunctionType::None, in_loop: false }
    }

    fn declare(&mut self, name: String) -> SemanticResult {
        let num_scopes = self.scopes.len();
        if num_scopes == 0 { return Ok(()); }
        if self.scopes.get(num_scopes - 1).unwrap().contains_key(&name) {
            return Err(SemanticError::RedeclaredVariable);
        }
        self.scopes.get_mut(num_scopes - 1).unwrap().insert(name, VarState::Declared);
        Ok(())
    }

    fn define(&mut self, name: String) {
        let num_scopes = self.scopes.len();
        if num_scopes == 0 { return }

        self.scopes.get_mut(num_scopes - 1).unwrap().insert(name, VarState::Defined);
    }

    fn nest(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn unnest(&mut self) {
        self.scopes.pop();
    }
    
    fn resolve_stmt_vec(&mut self, stmts: &Vec<Box<dyn Stmt>>) -> SemanticResult {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Box<dyn Stmt>) -> SemanticResult {
        stmt.accept(self)
    }

    fn resolve_expr(&mut self, expr: &Box<dyn Expr>) -> SemanticResult {
        expr.accept(self)
    }

    fn resolve_local(&mut self, _expr: &dyn Expr, name: String) {
        for i in (0..self.scopes.len()).rev() {
            let frame = self.scopes.get_mut(i).unwrap();
            if frame.contains_key(&name) {
                frame.insert(name.clone(), VarState::Accessed);
                // TODO: implement
            }
        }
    }

    fn resolve_fxn(&mut self, fxn: &FunStmt, fxn_type: FunctionType) -> SemanticResult {
        self.nest();
        for param in &fxn.aux.params {
            self.declare(param.lexeme.clone())?;
        }
        self.nest();
        if !matches!(self.fxn_type, FunctionType::Method) {
            let enclosing = self.fxn_type.clone();
            self.fxn_type = fxn_type;
            self.resolve_stmt(&fxn.aux.block)?;
            self.fxn_type = enclosing;
        } else { self.resolve_stmt(&fxn.aux.block)?; }
        self.unnest();
        self.unnest();
        Ok(())
    }
}

pub type SemanticResult = Result<(), SemanticError>;

impl<'parser> StmtVisitor<'parser, SemanticResult> for ASTAnalyzer {
    fn visit_expr(&mut self, stmt: &'parser ExprStmt) -> SemanticResult {
        self.resolve_expr(&stmt.expression)?;
        Ok(())
    }

    fn visit_print(&mut self, stmt: &'parser Print) -> SemanticResult {
        self.resolve_expr(&stmt.expression)?;
        Ok(())
    }

    fn visit_var(&mut self, stmt: &'parser VarStmt) -> SemanticResult {
        self.declare(stmt.name.lexeme.clone())?;
        self.resolve_expr(&stmt.expression)?;
        self.define(stmt.name.lexeme.clone());
        Ok(())
    }

    fn visit_block(&mut self, stmt: &'parser BlockStmt) -> SemanticResult {
        self.nest();
        self.resolve_stmt_vec(&stmt.stmts)?;
        for tup in self.scopes.get(self.scopes.len() - 1).unwrap().iter() {
            if !matches!(tup.1, &VarState::Accessed) {
                return Err(SemanticError::UnusedVariable)
            }
        }
        self.unnest();
        Ok(())
    }

    fn visit_if(&mut self, stmt: &'parser IfStmt) -> SemanticResult {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.stmt_if)?;
        if let Some(stmt_else) = &stmt.stmt_else {
            self.resolve_stmt(&stmt_else)?;
        }

        Ok(())
    }

    fn visit_while(&mut self, stmt: &'parser WhileLoop) -> SemanticResult {
        self.resolve_expr(&stmt.condition)?;
        let enclosing = self.in_loop;
        self.in_loop = true;
        self.resolve_stmt(&stmt.block)?;
        self.in_loop = enclosing;
        Ok(())
    }

    fn visit_for(&mut self, stmt: &'parser ForLoop) -> SemanticResult {
        self.resolve_stmt(&stmt.init)?;
        self.resolve_expr(&stmt.condition)?;
        self.resolve_expr(&stmt.incrementor)?;
        let enclosing = self.in_loop;
        self.in_loop = true;
        self.resolve_stmt(&stmt.block)?;
        self.in_loop = enclosing;
        Ok(())
    }

    fn visit_fxn(&mut self, stmt: &'parser FunStmt) -> SemanticResult {
        self.declare(stmt.aux.name.lexeme.clone())?;
        self.define(stmt.aux.name.lexeme.clone());
        self.resolve_fxn(stmt, FunctionType::Function)?;
        // self.resolve_stmt(&stmt.aux.block)?;
        Ok(())
    }

    fn visit_return(&mut self, stmt: &'parser ReturnStmt) -> SemanticResult {
        if let FunctionType::None = self.fxn_type {
            return Err(SemanticError::ReturnOutsideFunction);
        }
        if let Some(return_expr) = &stmt.expression {
            self.resolve_expr(&return_expr)?;
        }
        Ok(())
    }

    fn visit_break(&mut self, _stmt: &'parser BreakStmt) -> SemanticResult {
        if self.in_loop == false {
            return Err(SemanticError::BreakOutsideLoop);
        }
        Ok(())
    }

    fn visit_continue(&mut self, _stmt: &'parser ContinueStmt) -> SemanticResult {
        if self.in_loop == false {
            return Err(SemanticError::ContinueOutsideLoop);
        }
        Ok(())
    }

    fn visit_class(&mut self, stmt: &'parser ClassStmt) -> SemanticResult {
        if self.scopes.len() > 0 {
            return Err(SemanticError::NonGlobalClass);
        }
        self.declare(stmt.name.lexeme.clone())?;
        self.define(stmt.name.lexeme.clone());

        if let Some(ref var) = &stmt.super_expr {
            if stmt.name.lexeme == stmt.super_name.as_ref().unwrap().lexeme {
                return Err(SemanticError::CyclicSuperclass);
            }
            self.resolve_expr(var)?;
        }

        self.nest();
        self.define(String::from("this"));
        for method in &stmt.methods {
            self.resolve_fxn(method, FunctionType::Method)?;
        }
        self.unnest();
        Ok(())
    }

    fn visit_instantiation(&mut self, _stmt: &'parser InstantiationStmt) -> SemanticResult {
        Ok(())
    }
}


impl<'parser> ExprVisitor<'parser, SemanticResult> for ASTAnalyzer {
    fn visit_binary<'evaluator>(&'evaluator mut self, expr: &'parser Binary) -> SemanticResult {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_grouping<'evaluator>(&'evaluator mut self, expr: &'parser Grouping) -> SemanticResult {
        self.resolve_expr(&expr.expression)?;
        Ok(())
    }

    fn visit_literal<'evaluator>(&'evaluator mut self, _expr: &'parser Literal) -> SemanticResult {
        Ok(())
    }

    fn visit_unary<'evaluator>(&'evaluator mut self, expr: &'parser Unary) -> SemanticResult {
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_ternary<'evaluator>(&'evaluator mut self, expr: &'parser Ternary) -> SemanticResult {
        self.resolve_expr(&expr.condition)?;
        self.resolve_expr(&expr.expr_if)?;
        self.resolve_expr(&expr.expr_else)?;
        Ok(())
    }

    fn visit_assign<'evaluator>(&'evaluator mut self, expr: &'parser Assign) -> SemanticResult {
        self.resolve_expr(&expr.identifier)?;
        self.resolve_expr(&expr.expression)?;
        // self.resolve_local(expr, expr.identifier.lexeme.clone()); // TODO: Might have to do something here for resolution?
        Ok(())
    }

    fn visit_variable<'evaluator>(&'evaluator mut self, expr: &'parser Variable) -> SemanticResult {
        let num_scopes = self.scopes.len();

        if num_scopes > 0 {
            let current_frame = self.scopes.get(num_scopes - 1).unwrap();
            let name = &expr.identifier.lexeme;
            if current_frame.contains_key(name)
                && matches!(current_frame.get(name).unwrap(), &VarState::Declared)
            {
                return Err(SemanticError::UninitializedVariable);
            }
        }

        self.resolve_local(expr, expr.identifier.lexeme.clone());
        Ok(())
    }

    fn visit_or<'evaluator>(&'evaluator mut self, expr: &'parser OrExpr) -> SemanticResult {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_and<'evaluator>(&'evaluator mut self, expr: &'parser AndExpr) -> SemanticResult {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_call<'evaluator>(&'evaluator mut self, expr: &'parser Call) -> SemanticResult {
        // TODO: identifier should be a variable access
        for arg in &expr.args {
            self.resolve_expr(arg)?;
        }
        Ok(())
    }

    fn visit_lambda<'evaluator>(&'evaluator mut self, expr: &'parser Lambda) -> SemanticResult {
        self.resolve_fxn(&expr.stmt, FunctionType::Function)?;
        Ok(())
    }

    fn visit_property<'evaluator>(&'evaluator mut self, expr: &'parser Property) -> SemanticResult {
        self.resolve_expr(&expr.object)
    }

    fn visit_this<'evaluator>(&'evaluator mut self, expr: &'parser This) -> SemanticResult {
        self.resolve_local(expr, String::from("this"));
        if !matches!(self.fxn_type, FunctionType::Method) {
            return Err(SemanticError::ThisOutsideClass);
        }
        Ok(())
    }

    fn visit_super<'evaluator>(&'evaluator mut self, expr: &'parser Super) -> SemanticResult {
        self.resolve_local(expr, String::from("super"));
        if !matches!(self.fxn_type, FunctionType::Method) {
            return Err(SemanticError::SuperOutsideClass);
        }
        Ok(())
    }

}