use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug, Eq, PartialEq, Clone)]
enum Priority {
    INT,
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

pub struct Parser {
    l: Lexer,

    cur_token: Token,
    peek_token: Token,
}

impl Iterator for Parser {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.next_token();
        if self.cur_token == Token::EOF {
            return None;
        }
        Some(self.cur_token.clone())
    }
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Some(x) => program.statements.push(x),
                _ => {}
            };
            self.next_token();
        }
        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let value = match &self.peek_token {
            Token::IDENT(x) => Some(x),
            _ => return None,
        }?
        .clone();
        let name = Expression::Identifier {
            token: self.peek_token.clone(),
            value: value.to_string(),
        };
        self.next_token();
        if !self.expect_peek(Token::ASSIGN) {
            return None;
        }
        if !self.cur_token_is(Token::SEMICOLON) {
            self.next_token()
        }
        Some(Statement::LetStatement {
            token,
            name,
            value: Expression::Defa,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();
        while self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ReturnStatement {
            token: token,
            value: Expression::Defa,
        })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Priority::LOWEST)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ExpressionStatement { token, expression })
    }

    fn parse_expression(&mut self, precedence: Priority) -> Option<Expression> {
        let mut left_exp = self.prefix_fn(self.cur_token.clone())?;
        while !self.peek_token_is(Token::SEMICOLON)
            && (precedence.clone() as i32) < (self.peek_precedence() as i32)
        {
            self.next_token();
            left_exp = match self.infix_fn(Box::new(left_exp.clone()), self.cur_token.clone()) {
                Some(x) => x,
                None => left_exp,
            };
        }
        Some(left_exp)
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let value = match &self.cur_token {
            Token::IDENT(x) => Some(x),
            _ => None,
        }?;
        Some(Expression::Identifier {
            token: self.cur_token.clone(),
            value: value.to_string(),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let value = match &self.cur_token {
            Token::INT(x) => Some(x.parse::<i64>().unwrap()),
            _ => None,
        }?;
        Some(Expression::IntegerLiteral {
            token: self.cur_token.clone(),
            value: value,
        })
    }
    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(Token::TRUE),
        })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let prefix_token = self.cur_token.clone();
        self.next_token();
        Some(Expression::PrefixExpression {
            token: prefix_token.clone(),
            operator: prefix_token.to_string(),
            right: Box::new(self.parse_expression(Priority::PREFIX)?),
        })
    }
    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Option<Expression> {
        let infix_token = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        Some(Expression::InfixExpression {
            token: infix_token.clone(),
            left: left,
            operator: infix_token.to_string(),
            right: Box::new(self.parse_expression(precedence)?),
        })
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }
    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }
    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }
    fn prefix_fn(&mut self, token: Token) -> Option<Expression> {
        match &token {
            Token::IDENT(_) => self.parse_identifier(),
            Token::INT(_) => self.parse_integer_literal(),
            Token::BANG => self.parse_prefix_expression(),
            Token::MINUS => self.parse_prefix_expression(),
            Token::TRUE => self.parse_boolean(),
            Token::FALSE => self.parse_boolean(),
            _ => None,
        }
    }
    fn infix_fn(&mut self, left: Box<Expression>, token: Token) -> Option<Expression> {
        match token {
            Token::EQ => self.parse_infix_expression(left),
            Token::NOTEq => self.parse_infix_expression(left),
            Token::LT => self.parse_infix_expression(left),
            Token::GT => self.parse_infix_expression(left),
            Token::PLUS => self.parse_infix_expression(left),
            Token::MINUS => self.parse_infix_expression(left),
            Token::SLASH => self.parse_infix_expression(left),
            Token::ASTERISK => self.parse_infix_expression(left),
            _ => None,
        }
    }
    fn peek_precedence(&self) -> Priority {
        match self.peek_token {
            Token::EQ => Priority::EQUALS,
            Token::NOTEq => Priority::EQUALS,
            Token::LT => Priority::LESSGREATER,
            Token::GT => Priority::LESSGREATER,
            Token::PLUS => Priority::SUM,
            Token::MINUS => Priority::SUM,
            Token::SLASH => Priority::PRODUCT,
            Token::ASTERISK => Priority::PRODUCT,
            _ => Priority::LOWEST,
        }
    }
    fn cur_precedence(&self) -> Priority {
        match self.cur_token {
            Token::EQ => Priority::EQUALS,
            Token::NOTEq => Priority::EQUALS,
            Token::LT => Priority::LESSGREATER,
            Token::GT => Priority::LESSGREATER,
            Token::PLUS => Priority::SUM,
            Token::MINUS => Priority::SUM,
            Token::SLASH => Priority::PRODUCT,
            Token::ASTERISK => Priority::PRODUCT,
            _ => Priority::LOWEST,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let tests = ["x", "y", "foobar"];
        let l = Lexer::new(&input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);
        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            let a = match stmt {
                Statement::LetStatement {
                    token,
                    name:
                        Expression::Identifier {
                            token: _,
                            value: identvalue,
                        },
                    value,
                } => Some((token, identvalue, value)),
                _ => None,
            }
            .unwrap();
            assert_eq!(a.0.clone(), Token::LET);
            assert_eq!(a.1.clone(), tt.to_string());
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let l = Lexer::new(&input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 3);
        for stmt in program.statements {
            let value = match stmt {
                Statement::ReturnStatement {
                    token,
                    value: _value,
                } => Some(token),
                _ => None,
            }
            .unwrap();
            assert!(value == Token::RETURN)
        }
    }

    #[test]
    fn test_identifer_expression() {
        let input = "foobar;";
        let l = Lexer::new(&input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        test_identifer(stmt.1.clone(), "foobar".to_string());
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(&input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        test_integer_literal(stmt.1.clone(), 5);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [("!5;", "!", 5), ("-15", "-", 15)];
        for (_, tt) in prefix_tests.iter().enumerate() {
            let l = Lexer::new(&tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = match &program.statements[0] {
                Statement::ExpressionStatement { token, expression } => Some((token, expression)),
                _ => None,
            }
            .unwrap();
            let exp = match stmt.1 {
                Expression::PrefixExpression {
                    token,
                    operator,
                    right,
                } => Some((token.clone(), operator.clone(), right.clone())),
                _ => None,
            }
            .unwrap();
            assert_eq!(exp.1, tt.1);
            test_integer_literal(*exp.2, tt.2);
        }
    }
    enum Literal {
        I32(i32),
        I64(i64),
        STR(String),
    }
    fn test_infix_expression(exp: Expression, left: Literal, operator: String, right: Literal) {
        let op_exp = match exp {
            Expression::InfixExpression {
                token: _,
                left,
                operator,
                right,
            } => Some((*left, operator, *right)),
            _ => None, //TODO
        }
        .unwrap();
        test_literal_expression(op_exp.0, left);
        assert_eq!(op_exp.1, operator);
        test_literal_expression(op_exp.2, right);
    }
    fn test_literal_expression(exp: Expression, expected: Literal) {
        match expected {
            Literal::I32(x) => test_integer_literal(exp, x as i64),
            Literal::I64(x) => test_integer_literal(exp, x),
            Literal::STR(x) => test_identifer(exp, x),
            _ => (),
        }
    }
    fn test_integer_literal(il: Expression, value: i64) {
        let ig = match il {
            Expression::IntegerLiteral { token, value } => Some((token.clone(), value.clone())),
            _ => None,
        }
        .unwrap();
        assert_eq!(ig, (Token::INT(value.to_string()), value))
    }
    fn test_boolean_literal(exp: Expression, value: bool) {
        let bo = match exp {
            Expression::Boolean { token, value } => Some((token, value)),
            _ => None,
        }
        .unwrap();
        assert_eq!(bo.0.to_string(), value.to_string());
        assert_eq!(bo.1.to_string(), value.to_string());
    }
    fn test_identifer(il: Expression, value: String) {
        let ident = match il {
            Expression::Identifier { token, value } => Some((token.clone(), value.clone())),
            _ => None,
        }
        .unwrap();
        assert_eq!(ident, (Token::IDENT(value.clone()), value));
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];
        for (_, tt) in infix_tests.iter().enumerate() {
            let l = Lexer::new(&tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = match &program.statements[0] {
                Statement::ExpressionStatement { token, expression } => Some((token, expression)),
                _ => None,
            }
            .unwrap();
            test_infix_expression(
                stmt.1.clone(),
                Literal::I64(tt.1),
                tt.2.to_string(),
                Literal::I64(tt.3),
            );
        }
    }
    #[test]
    fn test_boolean_expression() {
        let tests = [("true;", true), ("false", false)];
        for tt in tests.iter() {
            let l = Lexer::new(&tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = match &program.statements[0] {
                Statement::ExpressionStatement { token, expression } => Some((token, expression)),
                _ => None,
            }
            .unwrap();
            test_boolean_literal(stmt.1.clone(), tt.1)
        }
    }
    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("(3 + 4); -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];
        for tt in tests.iter() {
            let l = Lexer::new(&tt.0.to_string());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            let actual = program.to_string();
            assert_eq!(actual, tt.1);
        }
    }
}
