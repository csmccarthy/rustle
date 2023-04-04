// use crate::exprs::{ Expr };
use crate::evaluator::{ RuntimeError, RuntimeResult };
use crate::scanner::Literal;
// use crate::scanner::{ Literal };

use std::collections::HashMap;

pub struct Environment {
    // stack: Vec<HashMap<String, &'parser Box<dyn Expr>>> // TODO: Revisit once you figure out cycles
    stack: Vec<HashMap<String, Literal>>
}



impl Environment {
    pub fn new() -> Environment {
        let global = HashMap::new();
        let mut stack = Vec::new();
        stack.push(global);
        Environment { stack }
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
        // println!("not found :(");
        return Err(RuntimeError::UndefinedVariable(name.to_owned()));
    }

    pub fn nest(&mut self) {
        // println!("nesting");
        let frame = HashMap::new();
        self.stack.push(frame);
    }

    pub fn unnest(&mut self) {
        // println!("unnesting");
        self.stack.pop();
    }
}