use std::fs::File;
use std::path::Path;
use std::io::{ Write };

fn build_exprs(mut file: File, exprs: [ [ String ; 2 ] ; 5]) -> Result<(), Box<dyn std::error::Error>> {
    write!(file, "use crate::scanner::{{ Token, Literal as LiteralValue }};\n")?;
    write!(file, "use std::fmt::Display;\n\n")?;

    write!(file, "pub trait ExprVisitor<R> {{\n")?;
    for expr in exprs.as_ref().into_iter() {
        write!(
            file, "\tfn visit_{}(&self, expr: &{}) -> R;\n",
            expr[0].to_lowercase(),
            expr[0],
        )?;
    }
    write!(file, "}}\n\n")?;

    write!(file, "pub trait Expr: Display {{\n")?;
    write!(file, "\tfn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R\n")?;
    write!(file, "\twhere Self: Sized;\n")?;
    write!(file, "}}\n\n\n\n")?;

    for expr in exprs.as_ref().into_iter() {
        write!(file, "pub struct {} {{\n", expr[0])?;
        for field in expr[1].split(',') {
            write!(file, "\tpub {},\n", field)?;
        }
        write!(file, "}}\n\n")?;

        write!(file, "impl Expr for {} {{\n", expr[0])?;
        write!(file, "\tfn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {{\n")?;
        write!(file, "\t\tvisitor.visit_{}(self)\n", expr[0].to_lowercase())?;
        write!(file, "\t}}\n")?;
        write!(file, "}}\n\n")?;
    }
    Ok(())
}

pub fn main() {
    let path = Path::new("./src/bin/exprs.rs");
    println!("{}", path.display());
    let file = match File::create(&path) {
        Err(e) => panic!("Couldn't create {}: {}", path.display(), e),
        Ok(file) => file
    };

    let exprs: [ [ String ; 2 ] ; 5] = [
        [
            "Binary".to_owned(),
            "left: Box<dyn Expr>,operator: Token,right: Box<dyn Expr>".to_owned()
        ],
        [
            "Grouping".to_owned(),
            "expression: Box<dyn Expr>".to_owned()
        ],
        [
            "Literal".to_owned(),
            "value: String".to_owned()
        ],
        [
            "Unary".to_owned(),
            "operator: Token,right: Box<dyn Expr>".to_owned()
        ],
        [
            "Ternary".to_owned(),
            "condition: Box<dyn Expr>,expr_if: Box<dyn Expr>,expr_else: Box<dyn Expr>".to_owned()
        ],
    ];
    match build_exprs(file, exprs) {
        Ok(_) => println!("Expression structs built successfully."),
        Err(e) => eprintln!("Error encountered during expression sruct creation: {}", e),
    }
}