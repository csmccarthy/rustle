use crate::declarator::ASTDeclarator;
use crate::evaluator::{ RuntimeError, RuntimeResult, RuntimeValue };
use crate::exprs::Lambda;
use crate::scanner::Literal;
use crate::stmts::{ FunStmt, FunStmtAux };

use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;
use std::sync::{ RwLock };
use std::rc::Rc;

static GLOBAL_FXN_ID: AtomicUsize = AtomicUsize::new(0);
// pub static GLOBAL_LMBDA_ID: AtomicUsize = AtomicUsize::new(0);


pub struct Environment {
    stack: Vec<HashMap<String, Literal>>,
    functions: Rc<RwLock<HashMap<usize, FunStmt>>>,
    lambdas: Rc<RwLock<HashMap<usize, Lambda>>>,
}



impl Environment {
    pub fn new() -> Environment {
        let global = HashMap::new();
        let mut stack = Vec::new();
        stack.push(global);
        Environment {
            stack,
            functions: Rc::new(RwLock::new(HashMap::new())),
            lambdas: Rc::new(RwLock::new(HashMap::new()))
        }
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
        for stack_item in self.stack.iter().enumerate().rev() {
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

    pub fn store_fxn(&mut self, fxn: &FunStmt) -> usize {
        let name = &fxn.aux.name.lexeme;
        let fxn_uid: usize = GLOBAL_FXN_ID.fetch_add(1, Ordering::SeqCst).into();
        self.current_scope_mut().insert(name.clone(), Literal::Func(fxn_uid));
        let mut fxn_clone = fxn.clone();
        let closure = self.clone();
        fxn_clone.closure = Some(closure);
        self.functions.write().unwrap().insert(fxn_uid, fxn_clone);
        fxn_uid
    }

    // pub fn store_lambda(&mut self, fxn: &Lambda) -> usize {
    //     // let name = &fxn.aux.name.lexeme;
    //     let fxn_uid: usize = GLOBAL_LMBDA_ID.fetch_add(1, Ordering::SeqCst).into();
	// 	let literal_name = format!("lambda {}", fxn_uid);
    //     // let fxn_uid: usize = GLOBAL_FXN_ID.fetch_add(1, Ordering::SeqCst).into();
    //     self.current_scope_mut().insert(literal_name, Literal::Func(fxn_uid));
    //     let mut fxn_clone = fxn.clone();
    //     let closure = self.clone();
    //     fxn_clone.stmt.closure = Some(closure);
    //     self.lambdas.write().unwrap().insert(fxn_uid, fxn_clone);
    //     fxn_uid
    // }

    fn call_fxn_block(&mut self, args: &Vec<Literal>, fxn_aux: &mut Rc<FunStmtAux>) -> RuntimeValue {
        self.nest(); // This nest creates a new stack frame for function arguments
        for arg_item in args.iter().zip(&fxn_aux.params) {
            if let Literal::Func(name) = arg_item.0 {
                self.current_scope_mut().insert(arg_item.1.lexeme.clone(), Literal::Func(*name));
            }
            self.define(arg_item.1.lexeme.to_owned(), arg_item.0.to_owned())?;
        }
        let mut decl = ASTDeclarator::new(self);
        let res = fxn_aux.block.execute(&mut decl);
        self.unnest();
        match res {
            Ok(_) => Ok(Literal::Nil),
            Err(RuntimeError::Return(literal)) => {
                if let Literal::Func(name) = &literal {
                    self.current_scope_mut().insert(literal.to_string(), Literal::Func(*name));
                }
                Ok(literal)
            },
            Err(e) => Err(e),
        }
    }

    pub fn call_fxn(&mut self, name: &str, args: &Vec<Literal>) -> RuntimeValue {
        let literal_opt = self.get(name);
        let fxn_literal = match literal_opt {
            Err(_) => { println!("here"); return Err(RuntimeError::UndefinedFunction(name.to_owned())) },
            Ok(f) => f
        };
        let fxn_clone = self.functions.clone();
        let unlocked_fxn = fxn_clone.read().unwrap();
        let fxn_opt = match fxn_literal {
            Literal::Func(name) => unlocked_fxn.get(&name),
            _ => return Err(RuntimeError::InvalidCallable(fxn_literal.clone()))
        };
        let fxn = match fxn_opt {
            None => { println!("no, here"); return Err(RuntimeError::UndefinedFunction(name.to_owned())) },
            Some(f) => f
        };
        if fxn.aux.params.len() != args.len() {
            return Err(RuntimeError::MismatchedArguments(fxn.aux.params.len(), args.len()));
        }
        let mut borrow = fxn.clone();
        drop(unlocked_fxn);

        return match borrow.closure {
            None => { self.call_fxn_block(args, &mut borrow.aux) },
            Some(mut env) => { env.call_fxn_block(args, &mut borrow.aux) },
        }
        
    }
    
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Environment {
            stack: self.stack.clone(),
            functions: self.functions.clone(),
            lambdas: self.lambdas.clone(),
        }
    }
}