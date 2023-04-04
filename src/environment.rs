use crate::exprs::{ Expr };
use crate::evaluator::{ RuntimeError, RuntimeResult };
// use crate::scanner::{ Literal };

use std::collections::HashMap;

pub struct Environment<'parser> {
    stack: Vec<HashMap<String, &'parser Box<dyn Expr>>>
}



impl<'parser> Environment<'parser> {
    pub fn new() -> Environment<'parser> {
        let global = HashMap::new();
        let mut stack = Vec::new();
        stack.push(global);
        Environment { stack }
    }

    pub fn define(&mut self, name: String, expr: &'parser Box<dyn Expr>) -> RuntimeResult<()> {
		self.current_scope_mut().insert(name.to_owned(), expr);
        Ok(())
    }

    pub fn assign(&mut self, name: &str, expr: &'parser Box<dyn Expr>) -> RuntimeResult<()> {
		let stack_opt = self.get_idx(name);
        if let Some(i) = stack_opt {
            self.stack.get_mut(i).unwrap().insert(name.to_owned(), expr);
            return Ok(());
        }
        return Err(RuntimeError::UndefinedVariable(name.to_owned()));
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, &'parser Box<dyn Expr>> {
        let len = self.stack.len();
        if len == 0 { eprintln!("Global scope was dropped"); }
        return self.stack.get_mut(len - 1).unwrap();
    }

    fn current_scope(&self) -> &HashMap<String, &'parser Box<dyn Expr>> {
        let len = self.stack.len();
        if len == 0 { eprintln!("Global scope was dropped"); }
        return self.stack.get(len - 1).unwrap();
    }

    fn get_idx(&self, name: &str) -> Option<usize> {
        for stack_item in self.stack.iter().enumerate() {
            let frame: &HashMap<String, &'parser Box<dyn Expr>> = stack_item.1;
            if let Some(_) = frame.get(name) { return Some(stack_item.0); }
        }
        None
    }

    pub fn get(&self, name: &str) -> RuntimeResult<&'parser Box<dyn Expr>> {
        // println!("getting");
		let stack_opt = self.current_scope().get(name);
        // if let Some(expr) = stack_opt { println!("{}", expr); }
        if let Some(expr) = stack_opt { return Ok(expr); }
        for scope in &self.stack[1..] {
            let frame: &HashMap<String, &'parser Box<dyn Expr>> = scope.into();
            if let Some(expr) = frame.get(name) { return Ok(expr); }
        }
        println!("not found :(");
        return Err(RuntimeError::UndefinedVariable(name.to_owned()));
    }

    pub fn nest(&mut self) {
        println!("nesting");
        let frame = HashMap::new();
        self.stack.push(frame);
    }

    pub fn unnest(&mut self) {
        println!("unnesting");
        self.stack.pop();
    }
}