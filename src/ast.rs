use crate::token::Token;
use std::fmt::{self, format};

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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::LetStatement { token, name, value } => format!("{} {}{};", token, name, value),
            Self::ReturnStatement { token, value } => format!("{} {};", token, value),
            Self::ExpressionStatement {
                token: _,
                expression,
            } => format!("{}", expression),
        };
        f.write_str(&s)?;
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    PrefixExpression {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        token: Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Defa,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Identifier { token, value: _ } => token.clone().to_string(),
            Self::IntegerLiteral { token, value: _ } => token.to_string(),
            Self::PrefixExpression {
                token: _,
                operator,
                right,
            } => format!("({}{})", operator, right),
            Self::InfixExpression {
                token: _,
                left,
                operator,
                right,
            } => format!("({} {} {})", left, operator, right),
            _ => "".to_string(),
        };
        f.write_str(&s)?;
        Ok(())
    }
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in self.statements.iter() {
            f.write_str(&s.to_string());
        }
        Ok(())
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
