use std::fmt::Display;

use crate::exprs::{ Expr, Binary, Grouping, Literal, Unary, Ternary, Variable, Assign };
use crate::stmts::{ Stmt, ExprStmt, Print, VarStmt, BlockStmt };
use crate::scanner::{ Token, Tokens, Literal as LiteralValue };

pub struct Parser {
    pub tokens: Vec<Token>,
    current: u8,
    pub stmts: Vec<Box<dyn Stmt>>,
    pub errs: Vec<SyntaxError>,
}

pub enum SyntaxError {
    UnexpectedToken(Token, Vec<Token>),
    ExpectedLiteral(u16, Vec<Token>),
    NotLValue(Box<dyn Expr>),
    EOFReached,
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
            SyntaxError::NotLValue(expr) => {
                return write!(f, "Invalid expression given for L Value: {}", expr)
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
        Parser { tokens, current: 0, stmts: Vec::new(), errs: Vec::new() }
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

    fn declaration(&mut self) -> ParseResult<Box<dyn Stmt>> {
        if self.match_token(Tokens::Var) { return Ok(self.decl_stmt()?) }
        self.stmt()
    }

    fn stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        if self.match_token(Tokens::Print) { return Ok(self.print_stmt()?) }
        else if self.match_token(Tokens::LeftBrace) { return Ok(self.block()?) }
        self.expression_stmt()
    }

    fn block(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let mut stmts = Vec::new();
        while self.peek().is_some()
                && !self.match_token(Tokens::EOF)
                && !self.match_token(Tokens::RightBrace)
        {
            let stmt = self.declaration()?;
            let needs_semicolon = stmt.needs_semicolon();
            stmts.push(stmt);
            if needs_semicolon && !self.match_token(Tokens::Semicolon) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
            }
        }
        if self.peek().is_none() || self.previous().unwrap().token_type == Tokens::EOF {
            return Err(SyntaxError::EOFReached);
        }
        Ok(self.push_block(stmts))
    }

    fn decl_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let name = match self.advance() {
            None => return Err(SyntaxError::EOFReached),
            Some(tk) if tk.token_type == Tokens::Identifier => {
                tk.lexeme.clone()
            }
            Some(_) => return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        };
        if self.consume(Tokens::Assign) {
            let expr = self.expression()?;
            return Ok(self.push_var_stmt(name, expr));
        }
        let expr = self.push_literal(LiteralValue::Nil);
        Ok(self.push_var_stmt(name, expr))
    }

    fn expression_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let expr = self.expression()?;
        Ok(self.push_expr_stmt(expr))
    }

    fn print_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let expr = self.expression()?;
        Ok(self.push_print_stmt(expr))
    }

    fn expression(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.expression_group()
    }

    fn expression_group(&mut self) -> ParseResult<Box<dyn Expr>> {
        self.parse_binary(|s| { s.assignment() }, Vec::from(EXPRESSION_GROUP_TOKENS))
    }

    fn assignment(&mut self) -> ParseResult<Box<dyn Expr>> {
        let expr = self.ternary()?;
        if self.match_token(Tokens::Assign) {
            let val = self.assignment()?;
            match expr.assignment_target() {
                Some(i) => return Ok(self.push_assignment(i, val)),
                None => return Err(SyntaxError::NotLValue(expr))
            }
        }
        Ok(expr)
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
        else if self.match_token(Tokens::Identifier) {
            return Ok(self.push_variable(self.previous().unwrap().clone()));
        }
        else if self.match_token(Tokens::EOF) {
            return Err(SyntaxError::EOFReached);
        }
        Err(SyntaxError::ExpectedLiteral(self.peek().unwrap().line, Vec::new()))
    }

    fn push_var_stmt(&mut self, name: String, expression: Box<dyn Expr>) -> Box<dyn Stmt>
    { Box::new(VarStmt { name, expression }) }

    fn push_expr_stmt(&mut self, expression: Box<dyn Expr>) -> Box<dyn Stmt>
    { Box::new(ExprStmt { expression }) }

    fn push_block(&mut self, stmts: Vec<Box<dyn Stmt>>) -> Box<dyn Stmt>
    { Box::new(BlockStmt { stmts }) }

    fn push_print_stmt(&mut self, expression: Box<dyn Expr>) -> Box<dyn Stmt>
    { Box::new(Print { expression }) }

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

    fn push_assignment(&mut self, identifier: Token, expression: Box<dyn Expr>) -> Box<dyn Expr>
    { Box::new(Assign { identifier, expression }) }

    fn push_variable(&mut self, identifier: Token) -> Box<dyn Expr>
    { Box::new(Variable { identifier }) }

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
        // if let None = self.peek() { self.errs.push(SyntaxError::EmptyFile); }
        while self.peek().is_some() && self.peek().unwrap().token_type != Tokens::EOF {
            let stmt_res = self.declaration();
            if let Err(mut e) = stmt_res {
                self.sync(&mut e);
                self.errs.push(e);
            } else if let Ok(stmt) = stmt_res {
                let needs_semicolon = stmt.needs_semicolon();
                self.stmts.push(stmt);
                if  needs_semicolon && !self.consume(Tokens::Semicolon) {
                    let mut err = SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new());
                    self.sync(&mut err);
                    self.errs.push(err);
                }
            }
        }
    }
}