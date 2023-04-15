use crate::scanner::Scanner;

pub struct ErrorReporter;

impl ErrorReporter {

    pub fn report(&self, scanner: &Scanner, error: String) {
        println!("Error detected on line {}", scanner.line);
        println!("{}", error);
    }

}

// TODO: Expand this functionality, tokens need to be beefed up with more info