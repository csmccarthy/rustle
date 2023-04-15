use crate::declarator::ASTDeclarator;
use crate::evaluator::{ RuntimeError, RuntimeResult, RuntimeValue };
use crate::scanner::{Literal, Token};
use crate::stmts::{ FunStmt, FunStmtAux, InstantiationStmt };

use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::{ HashMap };
use std::cell::RefCell;
use std::rc::Rc;

static GLOBAL_FXN_ID: AtomicUsize = AtomicUsize::new(0);
static GLOBAL_CLASS_ID: AtomicUsize = AtomicUsize::new(0);
static GLOBAL_INSTANCE_ID: AtomicUsize = AtomicUsize::new(0);

pub type FxnUID = usize;
pub type ClassUID = usize;
pub type InstanceUID = usize;

type FunctionStore = HashMap<FxnUID, FunStmt>;
type RcLockedFunctionStore = Rc<RefCell<FunctionStore>>;

pub type PropertyStore = HashMap<String, Literal>;
type InstanceStore = HashMap<InstanceUID, PropertyStore>;
type RcLockedInstanceStore = Rc<RefCell<InstanceStore>>;

type MethodStore = HashMap<ClassUID, HashMap<String, usize>>;
type RcLockedMethodStore = Rc<RefCell<MethodStore>>;

type Hierarchy = HashMap<ClassUID, ClassUID>;
type RcLockedHierarchy = Rc<RefCell<Hierarchy>>;

pub struct Environment {
    stack: Vec<HashMap<String, Literal>>,
    functions: RcLockedFunctionStore,
    class_methods: RcLockedMethodStore,
    instances: RcLockedInstanceStore,
    hierarchy: RcLockedHierarchy,
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
            class_methods: RcLockedMethodStore::default(),
            hierarchy: RcLockedHierarchy::default(),
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
        let mut unlocked = self.instances.borrow_mut();
        let inst = unlocked.get_mut(uid).unwrap();
        inst.insert(name, val);
        Ok(())
    }

    pub fn add_generation(&mut self, class_uid: &ClassUID, super_uid: &ClassUID) {
        let mut unlocked = self.hierarchy.borrow_mut();
        unlocked.insert(*class_uid, *super_uid);
    }

    pub fn get_field(&mut self, uid: &usize, name: &str) -> RuntimeResult<Literal> {
        let unlocked = self.instances.borrow();
        let inst = unlocked.get(uid).unwrap();
        match inst.get(name) {
            Some(lit) => Ok(lit.clone()),
            None => Ok(Literal::Nil),
        }
    }

    pub fn assign_methods(&mut self, method_set: HashMap<String, FxnUID>) -> usize {
        let class_uid: usize = GLOBAL_CLASS_ID.fetch_add(1, Ordering::SeqCst).into();
        self.class_methods.borrow_mut().insert(class_uid, method_set);
        class_uid
    }

    pub fn inherit_methods(&self, super_uid: &usize, prop_store: &mut PropertyStore) {
        let method_store = self.class_methods.borrow();
        let mut super_uid = Some(*super_uid);
        while let Some(uid) = super_uid {
            let super_methods = method_store.get(&uid).unwrap();
            for method_tup in super_methods {
                prop_store.insert(method_tup.0.clone(), Literal::Func(*method_tup.1, None));
            }
            let unlocked = self.hierarchy.borrow();
            super_uid = unlocked.get(&uid).copied();
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
        self.instances.borrow_mut().insert(inst_uid, bound_props);
        self.run_user_constructor(inst_uid)?;
        Ok(inst_uid)
    }

    fn run_user_constructor(&mut self, inst_uid: usize) -> RuntimeResult<()> {
        let unlocked = self.instances.borrow();
        let inst = unlocked.get(&inst_uid).unwrap();
        if let Some(lit) = inst.get("init") {
            let mut args = Vec::new();
            for arg in self.stack.get(self.stack.len() - 2).unwrap().values() {
                args.push(arg.clone());
            }
            let lit = lit.clone();
            drop(unlocked);
            match lit {
                Literal::Func(fxn_uid, inst_opt) => {
                    match self.call_fxn(&fxn_uid, &args, inst_opt) {
                        Err(RuntimeError::Return(_)) => (),
                        Err(e) => return Err(e),
                        _ => ()
                    }
                },
                _ => return Err(RuntimeError::InvalidCallable(lit.clone())),
            }
        }
        Ok(())
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
        let unlocked_fxn = self.functions.borrow();
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
        self.functions.borrow_mut().insert(fxn_uid, fxn_clone);
        fxn_uid
    }

    pub fn get_class_fxn(&self, super_uid: &ClassUID, name: &str) -> RuntimeValue {
        let method_store = self.class_methods.borrow();
        let super_methods = method_store.get(super_uid).unwrap();
        match super_methods.get(name) {
            Some(uid) => Ok(Literal::Func(*uid, None)),
            None => Err(RuntimeError::InvalidCallable(Literal::Nil)) // TODO: Fix this
        }
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
        let unlocked_fxn = self.functions.borrow();
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
            class_methods: self.class_methods.clone(),
            hierarchy: self.hierarchy.clone(),
        }
    }
}