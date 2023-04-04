// use std::collections::HashMap;

// struct Environment<'a, 'b> {
//     example: &'a HashMap<String, &'b Box<i8>>
// }

// struct Ternary {
//     condition: 
// }

// impl<'a, 'b> Test<'a, 'b> {
//     fn visit_ternary(&'declarator mut self, expr: &'parser Ternary) -> RuntimeValue {
//         let test = expr.condition.evaluate(self)?;
    
//         match test {
//             LiteralValue::Bool(true) => return Ok(expr.expr_if.evaluate(self)?),
//             LiteralValue::Bool(false) => return Ok(expr.expr_else.evaluate(self)?),
//             _ => return Err(RuntimeError::InvalidConditional(test.clone()))
//         }
//     }
// }


