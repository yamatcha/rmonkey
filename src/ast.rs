use crate::token::Token;

pub trait Node {
    fn token_literal(&mut self) -> Token;
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    LetStatement {
        token: Token,
        name: Expression,
        value: Expression,
    },
    ReturnStatement {
        token: Token,
        value: Expression,
    },
    ExpressionStatement {
        token: Token,
        expression: Expression,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Identifier { token: Token, value: String },
    Defa,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&mut self) -> Token {
        if self.statements.len() > 0 {
            return token_literal(self.statements[0].clone());
        } else {
            return Token::ILLEGAL;
        }
    }
}

fn token_literal(stmt: Statement) -> Token {
    match stmt {
        Statement::LetStatement {
            token,
            name: _,
            value: _,
        } => token,
        Statement::ReturnStatement { token, value: _ } => token,
        Statement::ExpressionStatement {
            token,
            expression: _,
        } => token,
    }
}
