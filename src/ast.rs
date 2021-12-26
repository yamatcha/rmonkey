use crate::token::Token;

pub trait Node {
    fn token_literal(&mut self) -> Token;
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    LetStatement {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    ReturnStatement {
        token: Token,
        value: Expression,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
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
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&mut self) -> Token {
        self.token.clone()
    }
}
