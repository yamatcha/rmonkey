use crate::ast::Node;
use crate::object::Object;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    #[test]
    fn test_eval_integer_expression() {
        let tests = [("5".to_string(), 5), ("10".to_string(), 10)];
        for tt in tests.iter() {
            let evaluated = test_eval(tt.0.clone());
            test_integer_object(evaluated, tt.1);
        }
    }
    fn test_eval(input: String) -> Object {
        let l = Lexer::new(&input);
        let mut p = Parser::new(l);
        let mut program = p.parse_program().unwrap();
        return program.eval();
    }
    fn test_integer_object(obj: Object, expected: i64) -> bool {
        obj == Object::Integer(expected)
    }
}
