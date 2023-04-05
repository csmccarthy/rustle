use crate::declarator::ASTDeclarator;
// use crate::exprs::{ Expr };
use crate::evaluator::{ RuntimeError, RuntimeResult, RuntimeValue };
use crate::scanner::Literal;
use crate::stmts::FunStmt;
// use crate::scanner::{ Literal };

use std::collections::HashMap;



pub struct Environment<'parser> {
    // stack: Vec<HashMap<String, &'parser Box<dyn Expr>>> // TODO: Revisit once you figure out cycles
    stack: Vec<HashMap<String, Literal>>,
    functions: HashMap<String, &'parser FunStmt>
}



impl<'parser> Environment<'parser> {
    pub fn new() -> Environment<'parser> {
        let global = HashMap::new();
        let mut stack = Vec::new();
        stack.push(global);
        Environment { stack, functions: HashMap::new() }
    }

    pub fn define(&mut self, name: String, expr: Literal) -> RuntimeResult<()> {
		self.current_scope_mut().insert(name.to_owned(), expr);
        Ok(())
    }

    pub fn assign(&mut self, name: &str, expr: Literal) -> RuntimeResult<()> {
		let stack_opt = self.get_idx(name);
        if let Some(i) = stack_opt {
            self.stack.get_mut(i).unwrap().insert(name.to_owned(), expr);
            return Ok(());
        }
        return Err(RuntimeError::UndefinedVariable(name.to_owned()));
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Literal> {
        let len = self.stack.len();
        if len == 0 { eprintln!("Global scope was dropped"); }
        return self.stack.get_mut(len - 1).unwrap();
    }

    fn current_scope(&self) -> &HashMap<String, Literal> {
        let len = self.stack.len();
        if len == 0 { eprintln!("Global scope was dropped"); }
        return self.stack.get(len - 1).unwrap();
    }

    fn get_idx(&self, name: &str) -> Option<usize> {
        for stack_item in self.stack.iter().enumerate() {
            let frame: &HashMap<String, Literal> = stack_item.1;
            if let Some(_) = frame.get(name) { return Some(stack_item.0); }
        }
        None
    }

    pub fn get(&self, name: &str) -> RuntimeResult<Literal> {
		let stack_opt = self.current_scope().get(name);
        if let Some(expr) = stack_opt { return Ok(expr.clone()); }
        for i in (0..self.stack.len()).rev() {
            let scope = self.stack.get(i).unwrap();
            let frame: &HashMap<String, Literal> = scope.into();
            if let Some(expr) = frame.get(name) { return Ok(expr.clone()); }
        }
        return Err(RuntimeError::UndefinedVariable(name.to_owned()));
    }

    pub fn nest(&mut self) {
        let frame = HashMap::new();
        self.stack.push(frame);
    }

    pub fn unnest(&mut self) {
        self.stack.pop();
    }

    pub fn store_fxn(&mut self, fxn: &'parser FunStmt) {
        self.functions.insert(fxn.name.lexeme.clone(), fxn);
    }

    pub fn get_fxn(&self, name: &str) -> Option<&&FunStmt> {
        self.functions.get(name)
    }

    pub fn call_fxn(&self, name: &str, args: &Vec<Literal>) -> RuntimeValue {
        let fxn_opt = self.get_fxn(name);
        let fxn = match fxn_opt {
            None => return Err(RuntimeError::UndefinedFunction(name.to_owned())),
            Some(f) => f
        };
        if fxn.params.len() != args.len() {
            return Err(RuntimeError::MismatchedArguments(fxn.params.len(), args.len()));
        }
        let mut fxn_env = Environment::new();
        for arg_item in args.iter().zip(&fxn.params) {
            fxn_env.define(arg_item.1.lexeme.to_owned(), arg_item.0.to_owned())?;
        }
        let mut decl = ASTDeclarator::new(&mut fxn_env);
        match fxn.block.execute(&mut decl) {
            Ok(_) => Ok(Literal::Nil),
            Err(RuntimeError::Return(literal)) => Ok(literal),
            Err(e) => Err(e),
        }
        
    }
}