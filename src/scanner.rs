use crate::error_reporter::ErrorReporter;
// use crate::stmts::FunStmt;

use std::fmt::Display;
// use std::format::Debug
use std::ops::RangeInclusive;
use std::collections::HashMap;
use std::clone::Clone;
// use std::marker::Copy;


#[derive(Debug, Clone, PartialEq)]
pub enum Tokens {
    // 1 char
    Plus, Minus,
    Star, Slash,
    LeftBrace, RightBrace,
    LeftParenthesis, RightParenthesis,
    LeftBracket, RightBracket,
    Not,
    Assign,
    Semicolon,
    Comma,
    Period,
    Less, Greater,
    Question,
    Colon,

    // 2 char
    Increment, Decrement,
    PlusEqual, MinusEqual,
    Equality, Inequality,
    LessOrEqual, GreaterOrEqual,

    // 3+ char
    Comment,
    Number,
    Identifier,
    String,

    // Keywords
    And, Class, Else, False, Fxn,
    For, If, Nil, Or, Print, Return,
    Super, This, True, Var, While,
    Break, Continue,

    EOF
}

lazy_static! {
    static ref KEYWORD_MAP: HashMap<String, Tokens> = HashMap::from([
        (String::from("and"), Tokens::And),
        (String::from("class"), Tokens::Class),
        (String::from("else"), Tokens::Else),
        (String::from("false"), Tokens::False),
        (String::from("fxn"), Tokens::Fxn),
        (String::from("for"), Tokens::For),
        (String::from("if"), Tokens::If),
        (String::from("nil"), Tokens::Nil),
        (String::from("or"), Tokens::Or),
        (String::from("print"), Tokens::Print),
        (String::from("return"), Tokens::Return),
        (String::from("super"), Tokens::Super),
        (String::from("this"), Tokens::This),
        (String::from("true"), Tokens::True),
        (String::from("var"), Tokens::Var),
        (String::from("while"), Tokens::While),
        (String::from("break"), Tokens::Break),
        (String::from("continue"), Tokens::Continue),
    ]);
}

const CAPS: RangeInclusive<char> = RangeInclusive::new('A', 'Z');
const LOWERS: RangeInclusive<char> = RangeInclusive::new('a', 'z');
const NUMS: RangeInclusive<char> = RangeInclusive::new('0', '9');

type FxnUID = usize;
type InstanceUID = usize;

// These can't hold -any- mutable state in them. Only values with copy semantics
#[derive(Clone)]
pub enum Literal {
    Num(f64),
    Str(String),
    Bool(bool),
    Func(FxnUID, Option<InstanceUID>),
    // Class(String),
    Instance(String, InstanceUID),
    Nil,
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Num(num) => write!(f, "{}", &num),
            Literal::Str(str) => write!(f, "{}", &str),
            Literal::Bool(bool) => write!(f, "{}", &bool),
            Literal::Func(idx, inst_idx_opt) => {
                if let Some(inst_idx) = inst_idx_opt {
                    write!(f, "fn(), vtable idx {}, bound instance: {}", &idx, &inst_idx)
                } else {
                    write!(f, "fn(), vtable idx {}", &idx)
                }
            },
            // Literal::Class(str) => write!(f, "class {}", &str),
            Literal::Instance(str, idx) => { write!(f, "instance {}, vtable idx {}", &str, idx) },
            Literal::Nil => write!(f, "()"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Num(num) => write!(f, "{}", &num),
            Literal::Str(str) => write!(f, "{}", &str),
            Literal::Bool(bool) => write!(f, "{}", &bool),
            Literal::Func(idx, inst_idx_opt) => {
                if let Some(inst_idx) = inst_idx_opt {
                    write!(f, "fn(), vtable idx {}, bound instance: {}", &idx, &inst_idx)
                } else {
                    write!(f, "fn(), vtable idx {}", &idx)
                }
            },
            // Literal::Class(str) => write!(f, "class {}", &str),
            Literal::Instance(str, idx) => { write!(f, "instance {}, vtable idx {}", &str, idx) },
            Literal::Nil => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: Tokens,
    pub literal: Literal,
    pub lexeme: String,
    pub line: u16,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}: {:?} {}", self.line, self.token_type, self.lexeme)
    }
}

pub struct Scanner
{
    start: u16,
    current: u16,
    source: String,
    pub line: u16,
    pub tokens: Vec<Token>,
}

impl Scanner {

    pub fn new(source: String) -> Scanner {
        Scanner {
            start: 0,
            current: 0,
            source,
            line: 1,
            tokens: Vec::new(),
        }
    }
    
    fn advance(&mut self) -> Option<char> {
        let char = match self.source.chars().nth(self.current.into()) { // TODO: improve chars arr
            None => return None,
            Some(char) => char,
        };
        self.current += 1;
        Some(char)
    }

    fn peek(&mut self) -> Option<char> {
        self.source.chars().nth(self.current.into())
    }

    fn peek_2(&mut self) -> Option<char> {
        self.source.chars().nth((self.current + 1).into())
    }

    fn advance_if_fxn<F>(&mut self, test_fxn: F) -> Option<char>
    where F: Fn(&char) -> bool
    {
        let char_opt = self.peek();
        if let None = char_opt { return None; }
        let char = char_opt.unwrap();
        if test_fxn(&char) {
            self.current += 1;
            return Some(char);
        }
        return None
    }

    fn advance_if_char(&mut self, test_char: char) -> Option<char> {
        self.advance_if_fxn(|&char| { char == test_char })
    }

    fn add_token(&mut self, token: Tokens) {
        let lexeme = self.get_source_range(0, 0);
        let t = Token {
            token_type: token, lexeme, line: self.line, literal: Literal::Nil
        };
        self.tokens.push(t);
    }

    fn get_source_range(&mut self, start_offset: u16, end_offset: u16) -> String {
        let start_range: usize = (self.start + start_offset).into();
        let end_range: usize = (self.current - end_offset).into();
        self.source[start_range..end_range].to_owned()
    }

    fn add_token_literal(&mut self, token_enum: Tokens, start_offset: u16, end_offset: u16)
    {
        let literal_str = self.get_source_range(start_offset, end_offset);
        let literal = match token_enum {
            Tokens::String => Literal::Str(literal_str.to_owned()),
            Tokens::Comment => Literal::Str(literal_str.to_owned()),
            Tokens::Number => Literal::Num(literal_str.parse::<f64>().unwrap()),
            Tokens::Identifier => Literal::Str(literal_str.to_owned()),
            _ => { eprintln!("Attempting to add a literal for non-literal token type"); return }
        };
        let lexeme = self.get_source_range(0, 0);
        // println!("{}", lexeme);
        let t = Token {
            token_type: token_enum, literal, lexeme, line: self.line,
        };
        self.tokens.push(t);
    }

    fn attempt_match_2(&mut self, char: char, two_char: Tokens, one_char: Tokens) {
        if let Some(_) = self.advance_if_char(char) { self.add_token(two_char); }
        else { self.add_token(one_char); }
    }

    pub fn scan_tokens(&mut self, reporter: ErrorReporter) {
        let mut char_opt = self.advance();
        while let Some(char_some) = char_opt {
            let char = char_some;
            match char {
                ' ' | '\t' | '\r' => (),
                '*' => self.add_token(Tokens::Star),
                '{' => self.add_token(Tokens::LeftBrace),
                '}' => self.add_token(Tokens::RightBrace),
                '(' => self.add_token(Tokens::LeftParenthesis),
                ')' => self.add_token(Tokens::RightParenthesis),
                '[' => self.add_token(Tokens::LeftBracket),
                ']' => self.add_token(Tokens::RightBracket),
                ';' => self.add_token(Tokens::Semicolon),
                ',' => self.add_token(Tokens::Comma),
                '.' => self.add_token(Tokens::Period),
                '?' => self.add_token(Tokens::Question),
                ':' => self.add_token(Tokens::Colon),

                '!' => self.attempt_match_2('=', Tokens::Inequality, Tokens::Not),
                '<' => self.attempt_match_2('=', Tokens::LessOrEqual, Tokens::Less),
                '>' => self.attempt_match_2('=', Tokens::GreaterOrEqual, Tokens::Greater),
                '=' => self.attempt_match_2('=', Tokens::Equality, Tokens::Assign),

                '+' => {
                    if let Some(_) = self.advance_if_char('+') { self.add_token(Tokens::Increment); }
                    else { self.attempt_match_2('=', Tokens::PlusEqual, Tokens::Plus); }
                },
                '-' => {
                    if let Some(_) = self.advance_if_char('+') { self.add_token(Tokens::Decrement); }
                    else { self.attempt_match_2('=', Tokens::MinusEqual, Tokens::Minus); }
                },
                // Match a single/multiline string
                '"' => {
                    while let Some(_) = self.advance_if_fxn(|&char| {
                        char != '"'
                    }) {}
                    if let None = self.advance() {
                        reporter.report(self, String::from("Unterminated string"));
                        return // TODO: No return
                    }
                    else { self.add_token_literal(Tokens::String, 1, 1); }
                },
                '/' => {
                    // println!("Matching slash");
                    // Match a single line comment
                    if let Some(_) = self.advance_if_char('/') {
                        // println!("Matching comment");
                        while let Some(_) = self.advance_if_fxn(|&char| { char != '\n' }) {}
                        self.add_token_literal(Tokens::Comment, 2, 0);
                    }
                    // Match a multiline comment
                    else if let Some(_) = self.advance_if_char('*') {
                        let mut nest_ctr = 1_u8;
                        'ml_comment: while nest_ctr != 0 {
                            while self.advance_if_fxn(|&char| {
                                char != '*' && char != '/' && char != '\n'
                            }).is_some() {}
                            if let None = self.peek() {
                                reporter.report(self, String::from("Unterminated multiline comment"));
                                break 'ml_comment; // TODO: No break
                            }
                            if self.advance_if_char('\n').is_some() { self.line += 1; }
                            else if self.advance_if_char('*').is_some() && self.advance_if_char('/').is_some() {
                                nest_ctr -= 1;
                            }
                            else if self.advance_if_char('/').is_some() && self.advance_if_char('*').is_some() {
                                nest_ctr += 1;
                            }
                        }
                        self.add_token_literal(Tokens::Comment, 0, 0);
                    }
                    else { self.add_token(Tokens::Slash) }
                },
                // Match a number literal
                '0'..='9' => {
                    while let Some(_) = self.advance_if_fxn(|char| { NUMS.contains(char) }) {}
                    if self.peek().unwrap_or('\0') == '.' && NUMS.contains(&self.peek_2().unwrap_or('\0')) {
                        self.advance();
                        while let Some(_) = self.advance_if_fxn(|char| { NUMS.contains(char) }) {}
                    }
                    self.add_token_literal(Tokens::Number, 0, 0);
                },
                // Match an identifier or a reserved keyword
                'A'..='Z' | 'a'..='z' | '_' => {
                    while let Some(_) = self.advance_if_fxn(|char| {
                        if char == &'_' { return true }
                        if CAPS.contains(char) { return true }
                        if LOWERS.contains(char) { return true }
                        NUMS.contains(char)
                    }) {}
                    let text = self.get_source_range(0, 0);
                    if let Some(token_type) = KEYWORD_MAP.get(&text) {
                        self.add_token(token_type.clone());
                    }
                    else { self.add_token_literal(Tokens::Identifier, 0, 0); }
                },
                // Increment our line counter
                '\n' => {
                    self.line += 1
                },
                def => reporter.report(self, format!("Unexpected token: {}", def)) // TODO: Error reporter
            }
            self.start = self.current;
            char_opt = self.advance();
        }
        self.add_token(Tokens::EOF);
    }
}