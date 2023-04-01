use crate::scanner::Scanner;

pub struct ErrorReporter;

impl ErrorReporter {

    pub fn report(&self, scanner: &Scanner, error: String) {
        println!("Error detected on line {}", scanner.line);
        println!("{}", error);
    }

}