use std::fmt::Display;

use crate::exprs::{ Expr, Binary, Grouping, Literal, Unary, Ternary, Variable, Assign, OrExpr, AndExpr, Call, Lambda };
use crate::stmts::{ Stmt, ExprStmt, Print, VarStmt, BlockStmt, IfStmt, WhileLoop, ForLoop, FunStmt, ReturnStmt, BreakStmt, ContinueStmt };
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
        if self.match_token(Tokens::Var) { return Ok(self.var_decl()?) }
        if self.match_token(Tokens::Fun) { return Ok(self.fun_decl()?) }
        self.stmt()
    }

    fn stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        if self.match_token(Tokens::Print) { return Ok(self.print_stmt()?) }
        else if self.match_token(Tokens::LeftBrace) { return Ok(self.block()?) }
        else if self.match_token(Tokens::If) { return Ok(self.if_stmt()?) }
        else if self.match_token(Tokens::While) { return Ok(self.while_loop()?) }
        else if self.match_token(Tokens::For) { return Ok(self.for_loop()?) }
        else if self.match_token(Tokens::Return) { return Ok(self.return_stmt()?) }
        else if self.match_token(Tokens::Break) { return Ok(BreakStmt::boxed_new()) } // TODO: Handle outside of loop
        else if self.match_token(Tokens::Continue) { return Ok(ContinueStmt::boxed_new()) } // TODO: Handle outside of loop
        self.expression_stmt()
    }

    fn return_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        return match self.expression() {
            Err(_) => Ok(ReturnStmt::boxed_new(None)),
            Ok(exp) => Ok(ReturnStmt::boxed_new(Some(exp))),
        }
    }

    fn while_loop(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let condition = self.expression()?;
        if !self.match_token(Tokens::LeftBrace) {
            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
        let block = self.block()?;
        Ok(WhileLoop::boxed_new(condition, block))
    }

    fn for_loop(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let init: Box<dyn Stmt>;
        if self.match_token(Tokens::Var) { init = self.var_decl()? }
        else { init = self.expression_stmt()? }
        if !self.match_token(Tokens::Semicolon) {
            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
        let condition = self.expression()?;
        if !self.match_token(Tokens::Semicolon) {
            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
        let incrementor = self.expression()?;
        if !self.match_token(Tokens::LeftBrace) {
            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
        let block = self.block()?;
        Ok(ForLoop::boxed_new(init, condition, incrementor, block))
    }

    fn if_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let condition = self.expression()?;
        if !self.match_token(Tokens::LeftBrace) {
            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
        let stmt_if = self.block()?;
        let mut stmt_else: Option<Box<dyn Stmt>> = None;
        if self.match_token(Tokens::Else) {
            if !self.match_token(Tokens::LeftBrace) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
            }
            stmt_else = Some(self.block()?);
        }
        Ok(IfStmt::boxed_new(condition, stmt_if, stmt_else))
    }

    fn block(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let mut stmts = Vec::new();
        while self.peek().is_some()
                && !self.match_token(Tokens::EOF)
                && !self.match_token(Tokens::RightBrace)
        {
            let stmt = self.declaration()?;
            let needs_semicolon = stmt.needs_semicolon();
            if needs_semicolon && !self.match_token(Tokens::Semicolon) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
            }
            stmts.push(stmt);
        }
        if self.peek().is_none() || self.previous().unwrap().token_type == Tokens::EOF {
            return Err(SyntaxError::EOFReached);
        }
        Ok(BlockStmt::boxed_new(stmts))
    }

    fn var_decl(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let name = match self.advance() {
            None => return Err(SyntaxError::EOFReached),
            Some(tk) if tk.token_type == Tokens::Identifier => {
                tk.lexeme.clone()
            }
            Some(_) => return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        };
        if self.consume(Tokens::Assign) {
            let expr = self.expression()?;
            return Ok(VarStmt::boxed_new(expr, name));
        }
        let expr = Literal::boxed_new(LiteralValue::Nil);
        Ok(VarStmt::boxed_new(expr, name))
    }

    fn extract_name(&mut self) -> ParseResult<Token> {
        match self.advance() {
            None => return Err(SyntaxError::EOFReached),
            Some(tk) if tk.token_type == Tokens::Identifier => {
                Ok(tk.clone())
            }
            Some(_) => return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
        }
    }

    fn fun_decl(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let name = self.extract_name()?;
        if !self.match_token(Tokens::LeftParenthesis) {
            return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()))
        }
        let mut params = Vec::new();
        if self.match_token(Tokens::RightParenthesis) {
            if !self.match_token(Tokens::LeftBrace) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
            }
            let block = self.block()?;
            return Ok(FunStmt::boxed_new(name, params, block));
        } else {
            params.push(self.extract_name()?);
            while self.match_token(Tokens::Comma) {
                params.push(self.extract_name()?);
            }
            if !self.match_token(Tokens::RightParenthesis) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()));
            }
            if !self.match_token(Tokens::LeftBrace) {
                return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
            }
            let block = self.block()?;
            return Ok(FunStmt::boxed_new(name, params, block));
        }
    }

    fn expression_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let expr = self.expression()?;
        Ok(ExprStmt::boxed_new(expr))
    }

    fn print_stmt(&mut self) -> ParseResult<Box<dyn Stmt>> {
        let expr = self.expression()?;
        Ok(Print::boxed_new(expr))
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
                Some(i) => return Ok(Assign::boxed_new(i, val)),
                None => return Err(SyntaxError::NotLValue(expr))
            }
        }
        Ok(expr)
    }

    fn ternary(&mut self) -> ParseResult<Box<dyn Expr>> {
        let mut expr = self.lambda()?;
        while self.match_token(Tokens::Question) {
            let expr_if = self.lambda()?;
            if !self.consume(Tokens::Colon) {
                return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()));
            }
            let carried_else = self.ternary()?;
            expr = Ternary::boxed_new(expr, expr_if, carried_else);
        }
        Ok(expr)
    }

    fn lambda(&mut self) -> ParseResult<Box<dyn Expr>> {
        if self.match_token(Tokens::Fun) {
            // let name = self.extract_name()?;
            if !self.match_token(Tokens::LeftParenthesis) {
                return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()))
            }
            let mut params = Vec::new();
            if self.match_token(Tokens::RightParenthesis) {
                if !self.match_token(Tokens::LeftBrace) {
                    return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
                }
                let block = self.block()?;
                return Ok(Lambda::boxed_new(params, block));
            } else {
                params.push(self.extract_name()?);
                while self.match_token(Tokens::Comma) {
                    params.push(self.extract_name()?);
                }
                if !self.match_token(Tokens::RightParenthesis) {
                    return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()));
                }
                if !self.match_token(Tokens::LeftBrace) {
                    return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()))
                }
                let block = self.block()?;
                return Ok(Lambda::boxed_new(params, block));
            }
        }
        Ok(self.logic_or()?)
    }

    fn logic_or(&mut self) -> ParseResult<Box<dyn Expr>> {
        let mut expr = self.logic_and()?;
        while self.match_token(Tokens::Or) {
            let right = self.logic_and()?;
            expr = OrExpr::boxed_new(expr, right);
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> ParseResult<Box<dyn Expr>> {
        let mut expr = self.equality()?;
        while self.match_token(Tokens::And) {
            let right = self.equality()?;
            expr = AndExpr::boxed_new(expr, right);
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
            expr = Binary::boxed_new(expr, operator, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Box<dyn Expr>> {
        if self.match_token_vec(&Vec::from(UNARY_TOKENS)) {
            let operator = self.previous().unwrap().clone();
            let right = self.unary()?;
            return Ok(Unary::boxed_new(operator, right));
        }
        self.call()
    }

    fn call(&mut self) -> ParseResult<Box<dyn Expr>> {
        let expr = self.primary()?;
        let identifier = match self.previous() {
            Some(tk) if tk.token_type == Tokens::Identifier => {
                Some(tk.clone())
            },
            _ => None
        };
        // println!("{:?}", identifier);
        if let Some(tk) = identifier {
            loop {
                if self.match_token(Tokens::LeftParenthesis) {
                    // println!("making fxn call");
                    let mut args = Vec::new();
                    if self.match_token(Tokens::RightParenthesis) {
                        return Ok(Call::boxed_new(tk, args));
                    } else {
                        args.push(self.assignment()?);
                        while self.match_token(Tokens::Comma) {
                            args.push(self.assignment()?);
                        }
                        if !self.match_token(Tokens::RightParenthesis) {
                            return Err(SyntaxError::UnexpectedToken(self.previous().unwrap().clone(), Vec::new()));
                        }
                        return Ok(Call::boxed_new(tk, args));
                    }
                } else { break; }
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> ParseResult<Box<dyn Expr>> {
        if self.match_token(Tokens::Number) || self.match_token(Tokens::String) {
            let literal = self.previous().unwrap().literal.clone();
            return Ok(Literal::boxed_new(literal))
        }
        else if self.match_token(Tokens::True) {
            return Ok(Literal::boxed_new(LiteralValue::Bool(true)))
        }
        else if self.match_token(Tokens::False) {
            return Ok(Literal::boxed_new(LiteralValue::Bool(false)))
        }
        else if self.match_token(Tokens::Nil) {
            return Ok(Literal::boxed_new(LiteralValue::Nil))
        }
        else if self.match_token(Tokens::LeftParenthesis) {
            let expr = self.expression()?;
            if !self.consume(Tokens::RightParenthesis) {
                return Err(SyntaxError::UnexpectedToken(self.peek().unwrap().clone(), Vec::new()));
            }
            return Ok(Grouping::boxed_new(expr));
        }
        else if self.match_token(Tokens::Identifier) {
            return Ok(Variable::boxed_new(self.previous().unwrap().clone()));
        }
        else if self.match_token(Tokens::EOF) {
            // println!("here");
            return Err(SyntaxError::EOFReached);
        }
        Err(SyntaxError::ExpectedLiteral(self.peek().unwrap().line, Vec::new()))
    }

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

    fn ignore_comments(&mut self) -> bool {
        while self.peek().is_some() && self.peek().unwrap().token_type == Tokens::Comment {
            self.advance();
        }
        if self.peek().is_none() { return false; }
        true
    }

    pub fn parse(&mut self) {
        // if let None = self.peek() { self.errs.push(SyntaxError::EmptyFile); }
        while self.peek().is_some()
            && self.ignore_comments()
            && self.peek().unwrap().token_type != Tokens::EOF
        {
            // println!("{:?}", self.peek().unwrap().token_type);
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