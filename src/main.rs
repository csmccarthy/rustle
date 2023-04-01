#[macro_use]
extern crate lazy_static;

mod scanner;
mod error_reporter;
mod parser;
mod exprs;
mod evaluator;


use scanner::Scanner;
// use scanner::{ Token, Tokens, Literal as LiteralValue };
use error_reporter::ErrorReporter;
use evaluator::{ ASTEvaluator };
use parser::{ Parser };

use std::fs::File;
use std::path::Path;
use std::io::{ self, Read, Write };



fn run(line: String) { // Should use bytes instead of String
    let mut scanner = Scanner::new(line.to_owned());
    let reporter = ErrorReporter;
    scanner.scan_tokens(reporter);
    // for token in &scanner.tokens {
    //     println!("{:?}", token.token_type);
    // }
    let mut parser = Parser::new(scanner.tokens);
    parser.parse();
    println!("Errors:");
    for err in parser.errs {
        eprintln!("{}", err);
    }
    println!("\nStatements:");
    let eval = ASTEvaluator;
    for expr in parser.exprs {
        print!("{} →  ", expr);
        if io::stdout().flush().is_err() { eprintln!("Issue flushing expression to stdout") };
        let res = expr.evaluate(&eval);
        match res {
            Err(_e) => eprintln!("Runtime Error :("),
            Ok(val) => println!("{}", val),
        }
    }
}


fn main() {

    // let printer = AstPrinter;
    // printer.print(&expression4);

    let path = Path::new("example2.rulox");
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
