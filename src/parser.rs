use std::fmt::Display;

use crate::exprs::{ Expr, Binary, Grouping, Literal, Unary, Ternary };
use crate::scanner::{ Token, Tokens, Literal as LiteralValue };

pub struct Parser {
    pub tokens: Vec<Token>,
    current: u8,
    pub exprs: Vec<Box<dyn Expr>>,
    pub errs: Vec<SyntaxError>,
}

pub enum SyntaxError {
    UnexpectedToken(Token, Vec<Token>),
    ExpectedLiteral(u16, Vec<Token>),
    EOFReached,
    EmptyFile,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::UnexpectedToken(expected, found) => {
                let seq = found.iter()
                    .map(|tk| tk.lexeme.clone())
                    .fold("".to_owned(), |acc, el| acc + " " + &el);
                return write!(
                    f, "Err on line {}:\nUnexpected token: {:?}\nFound {}",
                    expected.line, expected.token_type, seq
                )
            },
            SyntaxError::ExpectedLiteral(line, found) => {
                let seq = found.iter()
                    .map(|tk| tk.lexeme.clone())
                    .fold("".to_owned(), |acc, el| acc + " " + &el);
                return write!(
                    f, "Err on line {}:\nExpected Literal\nFound {}",
                    line, seq
                )
            },
            SyntaxError::EOFReached => {
                return write!(f, "Reached end of file while parsing expression")
            },
            SyntaxError::EmptyFile => {
                return write!(f, "Empty file")
            },
        }
    }
}

type ParseResult<T> = std::result::Result<T, SyntaxError>;

const EQUALITY_TOKENS: [ Tokens ; 2 ] = [Tokens::Equality, Tokens::Inequality];
const EXPRESSION_GROUP_TOKENS: [ Tokens ; 1 ] = [Tokens::Comma];
const COMPARISON_TOKENS: [ Tokens ; 4 ] = [
    Tokens::Greater, Tokens::Less, Tokens::GreaterOrEqual, Tokens::LessOrEqual
];
const TERM_TOKENS: [ Tokens ; 2 ] = [Tokens::Minus, Tokens::Plus];
const FACTOR_TOKENS: [ Tokens ; 2 ] = [Tokens::Slash, Tokens::Star];
const UNARY_TOKENS: [ Tokens ; 2 ] = [Tokens::Not, Tokens::Minus];

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0, exprs: Vec::new(), errs: Vec::new() }
    }

    fn match_token_fxn<F>(&mut self, test_fxn: F) -> bool
    where F: Fn(&Tokens) -> bool
    {
        while self.peek().is_some() && self.peek().unwrap().token_type == Tokens::Comment {
            self.advance();
        }
        let token_opt = self.peek();
        if let None = token_opt { return false; }
        let tk_type = &token_opt.unwrap().token_type;
        
        let matched = test_fxn(tk_type);
        if matched { self.current += 1; }
        matched
    }

    fn match_token(&mut self, token_type: Tokens) -> bool {
        self.match_token_fxn(|token| &token_type == token)
    }

    fn match_token_vec(&mut self, vec: &Vec<Tokens>) -> bool {
        self.match_token_fxn(|token| vec.contains(token))
    }

    fn expression(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.expression_group()
    }

    fn expression_group(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.ternary() }, Vec::from(EXPRESSION_GROUP_TOKENS))
    }

    fn ternary(&mut self) -> ParseResult<Box<dyn Expr>> {
        let mut expr = self.equality()?;
        while self.match_token(Tokens::Question) {
            let expr_if = self.equality()?;
            if !self.consume(Tokens::Colon) {
                return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()));
            }
            let carried_else = self.ternary()?;
            expr = self.push_ternary(expr, expr_if, carried_else);
        }
        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.comparison() }, Vec::from(EQUALITY_TOKENS))
    }

    fn comparison(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.term() }, Vec::from(COMPARISON_TOKENS))
    }

    fn term(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.factor() }, Vec::from(TERM_TOKENS))
    }

    fn factor(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.unary() }, Vec::from(FACTOR_TOKENS))
    }

    fn parse_binary<F>(&mut self, base_expr_cb: F, tokens: Vec<Tokens>) -> ParseResult<Box<dyn Expr>>
    where F: Fn(&mut Parser) -> ParseResult<Box<dyn Expr>>
    {
        let mut expr = base_expr_cb(self)?;
        while self.match_token_vec(&tokens) {
            let operator = self.previous().unwrap().clone();
            let right = base_expr_cb(self)?;
            expr = self.push_binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Box<dyn Expr>> {
        if self.match_token_vec(&Vec::from(UNARY_TOKENS)) {
            let operator = self.previous().unwrap().clone();
            let right = self.unary()?;
            return Ok(self.push_unary(operator, right));
        }
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Box<dyn Expr>> {
        if self.match_token(Tokens::Number) || self.match_token(Tokens::String) {
            let literal = self.previous().unwrap().literal.clone();
            return Ok(self.push_literal(literal))
        }
        else if self.match_token(Tokens::True) {
            return Ok(self.push_literal(LiteralValue::Bool(true)))
        }
        else if self.match_token(Tokens::False) {
            return Ok(self.push_literal(LiteralValue::Bool(false)))
        }
        else if self.match_token(Tokens::Nil) {
            return Ok(self.push_literal(LiteralValue::Nil))
        }
        else if self.match_token(Tokens::LeftParenthesis) {
            let expr = self.expression()?;
            if !self.consume(Tokens::RightParenthesis) {
                return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()));
            }
            return Ok(self.push_grouping(expr));
        }
        else if self.match_token(Tokens::EOF) {
            return Err(SyntaxError::EOFReached);
        }
        Err(SyntaxError::ExpectedLiteral(self.peek().unwrap().line, Vec::new()))
    }

    fn push_literal(&mut self, literal: LiteralValue) -> Box<dyn Expr>
    { Box::new(Literal { value: literal }) }

    fn push_unary(&mut self, operator: Token, right: Box<dyn Expr>) -> Box<dyn Expr>
    { Box::new(Unary { operator, right }) }

    fn push_binary(&mut self, left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>) -> Box<dyn Expr>
    { Box::new(Binary { left, operator, right }) }

    fn push_ternary(&mut self, condition: Box<dyn Expr>, expr_if: Box<dyn Expr>, expr_else: Box<dyn Expr>) -> Box<dyn Expr>
    { Box::new(Ternary { condition, expr_if, expr_else, }) }

    fn push_grouping(&mut self, expression: Box<dyn Expr>) -> Box<dyn Expr>
    { Box::new(Grouping { expression }) }

    fn previous(&self) -> Option<&Token> {
        let cur_idx: usize = (self.current - 1).into();
        let token_opt = self.tokens.get(cur_idx);
        token_opt
    }

    fn peek(&mut self) -> Option<&Token> {
        let cur_idx: usize = self.current.into();
        let token_opt = self.tokens.get(cur_idx);
        token_opt
    }

    fn sync(&mut self, err: &mut SyntaxError) {
        let mut token_opt = self.advance();
        while let Some(tk) = token_opt {
            if tk.token_type == Tokens::Semicolon { return }
            match tk.token_type {
                Tokens::Class
                    | Tokens::Fun
                    | Tokens::Var
                    | Tokens::For
                    | Tokens::If
                    | Tokens::While
                    | Tokens::Print
                    | Tokens::Return => return,
                _ => {
                    match err {
                        SyntaxError::UnexpectedToken(_, vec) => vec.push(tk.clone()),
                        SyntaxError::ExpectedLiteral(_, vec) => vec.push(tk.clone()),
                        _ => (),
                    }
                }
            }
            token_opt = self.advance();
        }
    }

    fn consume(&mut self, token_type: Tokens) -> bool {
        self.match_token(token_type.clone())
    }

    fn advance(&mut self) -> Option<&Token> {
        let cur_idx: usize = self.current.into();
        let token_opt = self.tokens.get(cur_idx);
        self.current += 1;
        token_opt
    }

    pub fn parse(&mut self) {
        if let None = self.peek() { self.errs.push(SyntaxError::EmptyFile); }
        while self.peek().is_some() && self.peek().unwrap().token_type != Tokens::EOF {
            let expr_res = self.expression();
            if let Err(mut e) = expr_res {
                self.sync(&mut e);
                self.errs.push(e);
            } else if let Ok(expr) = expr_res {
                self.exprs.push(expr);
                if !self.consume(Tokens::Semicolon) {
                    let mut err = SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new());
                    self.sync(&mut err);
                    self.errs.push(err);
                }
            }
        }
    }
}