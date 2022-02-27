use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

// type PrefixParseFn = Box<dyn Fn() -> Expression>;
// type InfixParseFn = Box<dyn Fn(Expression) -> Expression>;

enum Priority {
    // INT,
    LOWEST,
    // EQUALS,
    // LESSGREATER,
    // SUM,
    // PRODUCT,
    // PREFIX,
    // CALL,
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
        let prefix = self.prefix_fn();
        return prefix;
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
    fn prefix_fn(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::IDENT(_) => self.parse_identifier(),
            _ => None,
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
        let ident = match stmt.1 {
            Expression::Identifier { token, value } => Some((token.clone(), value.clone())),
            _ => None,
        }
        .unwrap();
        assert_eq!(
            ident,
            (Token::IDENT("foobar".to_string()), "foobar".to_string())
        );
    }
}
