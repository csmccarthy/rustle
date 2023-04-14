use crate::declarator::ASTDeclarator;
use crate::evaluator::{ RuntimeError, RuntimeResult, RuntimeValue };
// use crate::exprs::Instantiation;
// use crate::exprs::Lambda;
use crate::scanner::{Literal, Token};
use crate::stmts::{ FunStmt, FunStmtAux, InstantiationStmt };

use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;
use std::sync::{ RwLock };
use std::rc::Rc;
// use std::sync::Arc;

static GLOBAL_FXN_ID: AtomicUsize = AtomicUsize::new(0);
static GLOBAL_INSTANCE_ID: AtomicUsize = AtomicUsize::new(0);

type FunctionStore = HashMap<usize, FunStmt>;
type RcLockedFunctionStore = Rc<RwLock<FunctionStore>>;

pub type PropertyStore = HashMap<String, Literal>;
type InstanceStore = HashMap<usize, PropertyStore>;
type RcLockedInstanceStore = Rc<RwLock<InstanceStore>>;

pub struct Environment {
    stack: Vec<HashMap<String, Literal>>,
    functions: RcLockedFunctionStore,
    instances: RcLockedInstanceStore,
    // lambdas: Rc<RwLock<HashMap<usize, Lambda>>>,
}



impl Environment {
    pub fn new() -> Environment {
        let global = HashMap::new();
        let mut stack = Vec::new();
        stack.push(global);
        Environment {
            stack,
            functions: RcLockedFunctionStore::default(),
            instances: RcLockedInstanceStore::default(),
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

    pub fn assign_instance(&mut self, uid: &usize, name: String, val: Literal) -> RuntimeResult<()> {
        let mut unlocked = self.instances.write().unwrap();
        let inst = unlocked.get_mut(uid).unwrap();
        inst.insert(name, val);
        Ok(())
    }

    pub fn get_field(&mut self, uid: &usize, name: &str) -> RuntimeResult<Literal> {
        let unlocked = self.instances.read().unwrap();
        let inst = unlocked.get(uid).unwrap();
        match inst.get(name) {
            Some(lit) => Ok(lit.clone()),
            None => Ok(Literal::Nil),
        }
    }
    
    pub fn instantiate(&mut self, stmt: &InstantiationStmt) -> RuntimeResult<usize> {
        let inst_uid: usize = GLOBAL_INSTANCE_ID.fetch_add(1, Ordering::SeqCst).into();
        let mut bound_props: PropertyStore = HashMap::new();
        for prop in &stmt.properties {
            let name = prop.0.clone();
            match prop.1 {
                Literal::Func(idx, _) => bound_props.insert(name, Literal::Func(*idx, Some(inst_uid))),
                x => bound_props.insert(name, x.clone()),
            };
        }
        self.instances.write().unwrap().insert(inst_uid, bound_props);
        let unlocked = self.instances.read().unwrap();
        let inst = unlocked.get(&inst_uid).unwrap();
        if let Some(lit) = inst.get("init") {
            let mut args = Vec::new();
            for arg in self.stack.get(self.stack.len() - 2).unwrap().values() {
                args.push(arg.clone());
            }
            // println!("{}", args.len());
            let lit = lit.clone();
            drop(unlocked);
            match lit {
                Literal::Func(fxn_uid, inst_opt) => { self.call_fxn(&fxn_uid, &args, inst_opt)?; },
                _ => return Err(RuntimeError::InvalidCallable(lit.clone())),
            }
        }
        Ok(inst_uid)
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

    pub fn get_params(&self, uid: &usize) -> RuntimeResult<Vec<Token>> {
        let unlocked_fxn = self.functions.read().unwrap();
        let fxn_opt = unlocked_fxn.get(uid);
        let fxn = match fxn_opt {
            None => { return Err(RuntimeError::UndefinedFunction(String::from(""))) }, // TODO: give called token
            Some(f) => f
        };
        Ok(fxn.aux.params.clone())
    }

    pub fn store_fxn(&mut self, fxn: &FunStmt) -> usize {
        let name = &fxn.aux.name.lexeme;
        let fxn_uid: usize = GLOBAL_FXN_ID.fetch_add(1, Ordering::SeqCst).into();
        self.current_scope_mut().insert(name.clone(), Literal::Func(fxn_uid, None));
        let mut fxn_clone = fxn.clone();
        let closure = self.clone();
        fxn_clone.closure = Some(closure);
        self.functions.write().unwrap().insert(fxn_uid, fxn_clone);
        fxn_uid
    }

    fn call_fxn_block(&mut self, args: &Vec<Literal>, fxn_aux: &mut Rc<FunStmtAux>, opt: Option<usize>) -> RuntimeValue {
        self.nest(); // This nest creates a new stack frame for function arguments
        for arg_item in args.iter().zip(&fxn_aux.params) {
            if let Literal::Func(name, inst_uid) = arg_item.0 {
                self.current_scope_mut().insert(arg_item.1.lexeme.clone(), Literal::Func(*name, *inst_uid));
            }
            self.define(arg_item.1.lexeme.to_owned(), arg_item.0.to_owned())?;
        }
        if let Some(inst_uid) = opt {
            self.define(String::from("this"), Literal::Instance(String::from("self"), inst_uid))?; // TODO: Fix name given here
        }
        let mut decl = ASTDeclarator::new(self);
        let res = fxn_aux.block.execute(&mut decl);
        self.unnest();
        match res {
            Ok(_) => Ok(Literal::Nil),
            Err(RuntimeError::Return(literal)) => {
                if let Literal::Func(name, inst_uid) = &literal {
                    self.current_scope_mut().insert(literal.to_string(), Literal::Func(*name, *inst_uid));
                }
                Ok(literal)
            },
            Err(e) => Err(e),
        }
    }

    pub fn call_fxn(&mut self, uid: &usize, args: &Vec<Literal>, opt: Option<usize>) -> RuntimeValue {
        let unlocked_fxn = self.functions.read().unwrap();
        let fxn_opt = unlocked_fxn.get(uid);
        let fxn = match fxn_opt {
            None => { return Err(RuntimeError::UndefinedFunction(String::from(""))) }, // TODO: give called token
            Some(f) => f
        };
        if fxn.aux.params.len() != args.len() {
            return Err(RuntimeError::MismatchedArguments(fxn.aux.params.len(), args.len()));
        }
        let mut borrow = fxn.clone();
        drop(unlocked_fxn);

        return match borrow.closure {
            None => { self.call_fxn_block(args, &mut borrow.aux, opt) },
            Some(mut env) => { env.call_fxn_block(args, &mut borrow.aux, opt) },
        }
        
    }
    
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Environment {
            stack: self.stack.clone(),
            functions: self.functions.clone(),
            instances: self.instances.clone(),
        }
    }
}