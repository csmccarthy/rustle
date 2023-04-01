use crate::scanner::{ Token, Literal as LiteralValue };
use std::fmt::Display;

pub trait ExprVisitor<R> {
	fn visit_binary(&self, expr: &Binary) -> R;
	fn visit_grouping(&self, expr: &Grouping) -> R;
	fn visit_literal(&self, expr: &Literal) -> R;
	fn visit_unary(&self, expr: &Unary) -> R;
	fn visit_ternary(&self, expr: &Ternary) -> R;
}

pub trait Expr: Display {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R
	where Self: Sized;
}



pub struct Binary {
	pub left: Box<dyn Expr>,
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Binary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_binary(self)
	}
}

pub struct Grouping {
	pub expression: Box<dyn Expr>,
}

impl Expr for Grouping {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_grouping(self)
	}
}

pub struct Literal {
	pub value: String,
}

impl Expr for Literal {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_literal(self)
	}
}

pub struct Unary {
	pub operator: Token,
	pub right: Box<dyn Expr>,
}

impl Expr for Unary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_unary(self)
	}
}

pub struct Ternary {
	pub condition: Box<dyn Expr>,
	pub expr_if: Box<dyn Expr>,
	pub expr_else: Box<dyn Expr>,
}

impl Expr for Ternary {
	fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
		visitor.visit_ternary(self)
	}
}

