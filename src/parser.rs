use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("NoPrefixParseFnError: there is not parsing prefix function for {0}")]
    NoPrefixParseFnError(Token),
    #[error("PeekError: expected next token to be {0}")]
    PeekError(Token),
    #[error("invalid error")]
    TokenError,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Priority {
    // INT,
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

    pub fn parse_program(&mut self) -> Result<Program, Error> {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();
        let value = match &self.peek_token {
            Token::IDENT(x) => Ok(x),
            _ => Err(Error::TokenError),
        }?
        .clone();
        let name = Expression::Identifier {
            token: self.peek_token.clone(),
            value: value.to_string(),
        };
        self.next_token();
        self.expect_peek(Token::ASSIGN)?;
        self.next_token();
        let value = self.parse_expression(Priority::LOWEST)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::LetStatement { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();
        self.next_token();
        let value = self.parse_expression(Priority::LOWEST)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::ReturnStatement { token, value })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Priority::LOWEST)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        Ok(Statement::ExpressionStatement { token, expression })
    }

    fn parse_expression(&mut self, precedence: Priority) -> Result<Expression, Error> {
        let mut left_exp = self.prefix_fn(self.cur_token.clone())?;
        while !self.peek_token_is(Token::SEMICOLON)
            && (precedence.clone() as i32) < (self.peek_precedence() as i32)
        {
            self.next_token();
            left_exp = match self.infix_fn(Box::new(left_exp.clone()), self.cur_token.clone()) {
                Ok(x) => x,
                Err(Error::TokenError) => left_exp,
                Err(x) => panic!("{:?}", x),
            };
        }
        Ok(left_exp)
    }

    fn parse_identifier(&mut self) -> Result<Expression, Error> {
        let value = match &self.cur_token {
            Token::IDENT(x) => Ok(x),
            _ => Err(Error::TokenError),
        }?;
        Ok(Expression::Identifier {
            token: self.cur_token.clone(),
            value: value.to_string(),
        })
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, Error> {
        let value = match &self.cur_token {
            Token::INT(x) => Ok(x.parse::<i64>().unwrap()),
            _ => Err(Error::TokenError),
        }?;
        Ok(Expression::IntegerLiteral {
            token: self.cur_token.clone(),
            value: value,
        })
    }
    fn parse_boolean(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(Token::TRUE),
        })
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, Error> {
        let prefix_token = self.cur_token.clone();
        self.next_token();
        Ok(Expression::PrefixExpression {
            token: prefix_token.clone(),
            operator: prefix_token.to_string(),
            right: Box::new(self.parse_expression(Priority::PREFIX)?),
        })
    }
    fn parse_infix_expression(&mut self, left: Box<Expression>) -> Result<Expression, Error> {
        let infix_token = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        Ok(Expression::InfixExpression {
            token: infix_token.clone(),
            left,
            operator: infix_token.to_string(),
            right: Box::new(self.parse_expression(precedence)?),
        })
    }
    fn parse_call_expression(&mut self, function: Box<Expression>) -> Result<Expression, Error> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::CallExpression {
            token: self.cur_token.clone(),
            function,
            arguments,
        })
    }
    fn parse_call_arguments(&mut self) -> Result<Vec<Box<Expression>>, Error> {
        let mut args = Vec::new();
        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Ok(args);
        }
        self.next_token();
        args.push(Box::new(self.parse_expression(Priority::LOWEST)?));
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            args.push(Box::new(self.parse_expression(Priority::LOWEST)?));
        }
        self.expect_peek(Token::RPAREN)?;
        return Ok(args);
    }
    fn parse_gropued_expression(&mut self) -> Result<Expression, Error> {
        self.next_token();
        let exp = self.parse_expression(Priority::LOWEST);
        self.expect_peek(Token::RPAREN)?;

        exp
    }
    fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        self.expect_peek(Token::LPAREN)?;

        self.next_token();
        let condition = self.parse_expression(Priority::LOWEST)?;
        self.expect_peek(Token::RPAREN)?;
        self.expect_peek(Token::LBRACE)?;

        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(Token::ELSE) {
            self.next_token();
            self.expect_peek(Token::LBRACE)?;

            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };
        Ok(Expression::IfExpression {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }
    fn parse_block_statement(&mut self) -> Result<Statement, Error> {
        let token = self.cur_token.clone();
        self.next_token();
        let mut statements = Vec::new();
        while !self.cur_token_is(Token::RBRACE) && !self.cur_token_is(Token::EOF) {
            let stmt = self.parse_statement();
            match stmt {
                Ok(x) => statements.push(Box::new(x)),
                Err(_) => (),
            }
            self.next_token();
        }
        Ok(Statement::BlockStatement { token, statements })
    }
    fn parse_function_expression(&mut self) -> Result<Expression, Error> {
        let token = self.cur_token.clone();
        self.expect_peek(Token::LPAREN)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(Token::LBRACE)?;
        let body = self.parse_block_statement()?;
        Ok(Expression::FunctionalLiteral {
            token,
            parameters: parameters,
            body: Box::new(body),
        })
    }
    fn parse_function_parameters(&mut self) -> Result<Vec<Box<Expression>>, Error> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        let ident = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.to_string(),
        };
        identifiers.push(Box::new(ident));
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.to_string(),
            };
            identifiers.push(Box::new(ident));
        }
        self.expect_peek(Token::RPAREN)?;
        Ok(identifiers)
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }
    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }
    fn expect_peek(&mut self, t: Token) -> Result<(), Error> {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(Error::PeekError(t))
        }
    }
    fn prefix_fn(&mut self, token: Token) -> Result<Expression, Error> {
        match &token {
            Token::IDENT(_) => self.parse_identifier(),
            Token::INT(_) => self.parse_integer_literal(),
            Token::BANG => self.parse_prefix_expression(),
            Token::MINUS => self.parse_prefix_expression(),
            Token::TRUE => self.parse_boolean(),
            Token::FALSE => self.parse_boolean(),
            Token::LPAREN => self.parse_gropued_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_expression(),
            _ => Err(Error::NoPrefixParseFnError(token)),
        }
    }
    fn infix_fn(&mut self, left: Box<Expression>, token: Token) -> Result<Expression, Error> {
        match token {
            Token::EQ => self.parse_infix_expression(left),
            Token::NOTEq => self.parse_infix_expression(left),
            Token::LT => self.parse_infix_expression(left),
            Token::GT => self.parse_infix_expression(left),
            Token::PLUS => self.parse_infix_expression(left),
            Token::MINUS => self.parse_infix_expression(left),
            Token::SLASH => self.parse_infix_expression(left),
            Token::ASTERISK => self.parse_infix_expression(left),
            Token::LPAREN => self.parse_call_expression(left),
            _ => Err(Error::TokenError),
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
            Token::LPAREN => Priority::CALL,
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
            Token::LPAREN => Priority::CALL,
            _ => Priority::LOWEST,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_let_statements() {
        let tests = [
            ("let x = 5;", "x", Literal::I64(5)),
            ("let y = true;", "y", Literal::BOOL(true)),
            ("let foobar = y;", "foobar", Literal::STR("y".to_string())),
        ];
        for tt in tests.iter() {
            let l = Lexer::new(&tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            let letstmt = match stmt {
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
            assert_eq!(letstmt.0.clone(), Token::LET);
            assert_eq!(letstmt.1.clone(), tt.1.to_string());
            test_literal_expression(letstmt.2.clone(), tt.2.clone());
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 5;", Literal::I64(5)),
            ("return true;", Literal::BOOL(true)),
            ("return foobar;", Literal::STR("foobar".to_string())),
        ];
        for tt in tests.iter() {
            let l = Lexer::new(&tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            let returnstmt = match stmt {
                Statement::ReturnStatement { token, value } => Some((token, value)),
                _ => None,
            }
            .unwrap();
            assert_eq!(*returnstmt.0, Token::RETURN);
            test_literal_expression(returnstmt.1.clone(), tt.1.clone());
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
        let prefix_tests = [
            ("!5;", "!", Literal::I64(5)),
            ("-15;", "-", Literal::I64(15)),
            ("!foobar;", "!", Literal::STR("foobar".to_string())),
            ("-foobar;", "-", Literal::STR("foobar".to_string())),
            ("!true;", "!", Literal::BOOL(true)),
            ("!false;", "!", Literal::BOOL(false)),
        ];
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
            test_literal_expression(*exp.2, tt.2.clone());
        }
    }
    #[derive(Clone)]
    enum Literal {
        // I32(i32),
        I64(i64),
        STR(String),
        BOOL(bool),
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
            // Literal::I32(x) => test_integer_literal(exp, x as i64),
            Literal::I64(x) => test_integer_literal(exp, x),
            Literal::STR(x) => test_identifer(exp, x),
            Literal::BOOL(x) => test_boolean_literal(exp, x as bool),
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
            ("5 + 5;", Literal::I64(5), "+", Literal::I64(5)),
            ("5 - 5;", Literal::I64(5), "-", Literal::I64(5)),
            ("5 * 5;", Literal::I64(5), "*", Literal::I64(5)),
            ("5 / 5;", Literal::I64(5), "/", Literal::I64(5)),
            ("5 > 5;", Literal::I64(5), ">", Literal::I64(5)),
            ("5 < 5;", Literal::I64(5), "<", Literal::I64(5)),
            ("5 == 5;", Literal::I64(5), "==", Literal::I64(5)),
            ("5 != 5;", Literal::I64(5), "!=", Literal::I64(5)),
            (
                "true == true;",
                Literal::BOOL(true),
                "==",
                Literal::BOOL(true),
            ),
            (
                "true != false;",
                Literal::BOOL(true),
                "!=",
                Literal::BOOL(false),
            ),
            (
                "false == false;",
                Literal::BOOL(false),
                "==",
                Literal::BOOL(false),
            ),
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
            test_infix_expression(stmt.1.clone(), tt.1.clone(), tt.2.to_string(), tt.3.clone());
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
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        let ifexp = match stmt.1 {
            Expression::IfExpression {
                token,
                condition,
                consequence,
                alternative: None,
            } => Some((token, condition, consequence)),
            _ => None,
        }
        .unwrap();
        test_infix_expression(
            *(ifexp.1).clone(),
            Literal::STR("x".to_string()),
            "<".to_string(),
            Literal::STR("y".to_string()),
        );
        let consq = match *(ifexp.2).clone() {
            Statement::BlockStatement {
                token: _,
                statements,
            } => Some(statements),
            _ => None,
        }
        .unwrap();
        assert_eq!(consq.len(), 1);
        let consqexp = match *(consq[0].clone()) {
            Statement::ExpressionStatement {
                token: _,
                expression,
            } => Some(expression),
            _ => None,
        }
        .unwrap();
        test_identifer(consqexp, "x".to_string());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        let ifexp = match stmt.1 {
            Expression::IfExpression {
                token,
                condition,
                consequence,
                alternative: Some(alternative),
            } => Some((token, condition, consequence, alternative)),
            _ => None,
        }
        .unwrap();
        test_infix_expression(
            *(ifexp.1).clone(),
            Literal::STR("x".to_string()),
            "<".to_string(),
            Literal::STR("y".to_string()),
        );
        let consq = match *(ifexp.2).clone() {
            Statement::BlockStatement {
                token: _,
                statements,
            } => Some(statements),
            _ => None,
        }
        .unwrap();
        assert_eq!(consq.len(), 1);
        let consqexp = match *(consq[0].clone()) {
            Statement::ExpressionStatement {
                token: _,
                expression,
            } => Some(expression),
            _ => None,
        }
        .unwrap();
        test_identifer(consqexp, "x".to_string());
        let alter = match *(ifexp.3).clone() {
            Statement::BlockStatement {
                token: _,
                statements,
            } => Some(statements),
            _ => None,
        }
        .unwrap();
        assert_eq!(alter.len(), 1);
        let alterexp = match *(alter[0].clone()) {
            Statement::ExpressionStatement {
                token: _,
                expression,
            } => Some(expression),
            _ => None,
        }
        .unwrap();
        test_identifer(alterexp, "y".to_string());
    }

    #[test]
    fn test_functional_literal_parsing() {
        let input = "fn(x,y) { x + y }";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        let function = match stmt.1 {
            Expression::FunctionalLiteral {
                token,
                parameters,
                body,
            } => Some((token, parameters, body)),
            _ => None,
        }
        .unwrap();
        assert_eq!(function.1.len(), 2);
        test_literal_expression(*function.1[0].clone(), Literal::STR("x".to_string()));
        test_literal_expression(*function.1[1].clone(), Literal::STR("y".to_string()));
        let body_stmt = match *(function.2).clone() {
            Statement::BlockStatement {
                token: _,
                statements,
            } => Some(statements),
            _ => None,
        }
        .unwrap();
        assert_eq!(body_stmt.len(), 1);
        let body_stmt_exp = match *body_stmt[0].clone() {
            Statement::ExpressionStatement {
                token: _,
                expression,
            } => Some(expression),
            _ => None,
        }
        .unwrap();
        test_infix_expression(
            body_stmt_exp,
            Literal::STR("x".to_string()),
            "+".to_string(),
            Literal::STR("y".to_string()),
        )
    }

    #[test]
    fn test_functional_parameter_parsing() {
        let tests = [
            ("fn() {};", Vec::from([])),
            ("fn(x) {};", Vec::from(["x"])),
            ("fn(x,y,z) {};", Vec::from(["x", "y", "z"])),
        ];
        for tt in tests.iter() {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = match &program.statements[0] {
                Statement::ExpressionStatement { token, expression } => Some((token, expression)),
                _ => None,
            }
            .unwrap();
            let function = match stmt.1 {
                Expression::FunctionalLiteral {
                    token,
                    parameters,
                    body,
                } => Some((token, parameters, body)),
                _ => None,
            }
            .unwrap();
            assert_eq!(function.1.len(), tt.1.len());
            for (i, ident) in tt.1.iter().enumerate() {
                test_literal_expression(*(function.1[i]).clone(), Literal::STR(ident.to_string()))
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::ExpressionStatement { token, expression } => Some((token, expression)),
            _ => None,
        }
        .unwrap();
        let exp = match stmt.1 {
            Expression::CallExpression {
                token,
                function,
                arguments,
            } => Some((token, function, arguments)),
            _ => None,
        }
        .unwrap();
        test_literal_expression(*exp.1.clone(), Literal::STR("add".to_string()));
        assert_eq!(exp.2.len(), 3);
        test_literal_expression(*exp.2[0].clone(), Literal::I64(1));
        test_infix_expression(
            *exp.2[1].clone(),
            Literal::I64(2),
            "*".to_string(),
            Literal::I64(3),
        );
        test_infix_expression(
            *exp.2[2].clone(),
            Literal::I64(4),
            "+".to_string(),
            Literal::I64(5),
        );
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
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
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
