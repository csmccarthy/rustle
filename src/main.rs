#[macro_use]
extern crate lazy_static;

mod scanner;
mod error_reporter;
mod parser;
mod exprs;
mod evaluator;
mod stmts;
mod declarator;
mod environment;
mod analyzer;

use scanner::{ Scanner };
use error_reporter::ErrorReporter;
use declarator::{ ASTDeclarator };
use parser::{ Parser, SyntaxError };
use stmts::{ Stmt };
use environment::{ Environment };
use analyzer::{ ASTAnalyzer };

use std::fs::File;
use std::path::Path;
use std::io::{ self, Read, Write };



fn run(line: String) { // Should use bytes instead of String
    let mut scanner = Scanner::new(line.to_owned());
    let reporter = ErrorReporter;
    scanner.scan_tokens(reporter);
    let mut parser = Parser::new(scanner.tokens);
    parser.parse();
    let errs_ref: &Vec<SyntaxError> = &parser.errs;
    if errs_ref.len() > 0 {
        println!("Parse Errors:");
        for err in errs_ref {
            eprintln!("{}", err);
        }
        return;
    }
    
    let mut analyzer = ASTAnalyzer::new();
    for stmt in &parser.stmts {
        let mut errs = Vec::new();
        if let Err(e) = stmt.accept(&mut analyzer) {
            errs.push(e);
        }
        if errs.len() > 0 {
            println!("Semantic Errors:");
            for err in errs { println!("{:?}", err); }
            return;
        }
    }

    let mut env = Environment::new();
    let mut decl = ASTDeclarator::new(&mut env);
    for stmt in &parser.stmts {
        if io::stdout().flush().is_err() { eprintln!("Issue flushing statement to stdout") };
        let stmt: &Box<dyn Stmt> = stmt;
        let res = stmt.execute(&mut decl);
        match res {
            Err(e) => eprintln!("Runtime Error: {:?}", e),
            _ => ()
        }
    }
}


fn main() {
    let path = Path::new("example.rsl");
    let file = match File::open(&path) {
        Err(e) => panic!("Couldn't open {}: {}", path.display(), e),
        Ok(file) => file
    };

    let mut source = String::new();
    if let Err(e) = io::BufReader::new(file).read_to_string(&mut source) {
        println!("File read failed, please ensure the file exists and is accessible to the compiler.");
        println!("Err: {}", e);
    } else {
        run(source);
    }
}
