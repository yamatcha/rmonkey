use crate::{object::Object, token::Token};
use std::fmt;

pub trait Node {
    fn token_literal(&mut self) -> Token;
    fn eval(&mut self) -> Object;
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
    BlockStatement {
        token: Token,
        statements: Vec<Box<Statement>>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::LetStatement { token, name, value } => format!("{} {}={};", token, name, value),
            Self::ReturnStatement { token, value } => format!("{} {};", token, value),
            Self::ExpressionStatement {
                token: _,
                expression,
            } => format!("{}", expression),
            Self::BlockStatement {
                token: _,
                statements,
            } => format!(
                "{:}",
                statements
                    .iter()
                    .map(|x| (*x).to_string())
                    .collect::<Vec<_>>()
                    .join("")
            ),
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
    Boolean {
        token: Token,
        value: bool,
    },
    IfExpression {
        token: Token,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    FunctionalLiteral {
        token: Token,
        parameters: Vec<Box<Expression>>,
        body: Box<Statement>,
    },
    CallExpression {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
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
            Self::Boolean { token, value: _ } => token.to_string(),
            Self::IfExpression {
                token: _,
                condition,
                consequence,
                alternative: Some(alternative),
            } => {
                format!("if{} {}else {}", condition, consequence, alternative)
            }
            Self::IfExpression {
                token: _,
                condition,
                consequence,
                alternative: None,
            } => {
                format!("if{} {}", condition, consequence)
            }
            Self::FunctionalLiteral {
                token,
                parameters,
                body,
            } => {
                format!(
                    "{}({}){}",
                    token,
                    parameters
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                    body
                )
            }
            Self::CallExpression {
                token: _,
                function,
                arguments,
            } => format!(
                "{}({})",
                function,
                arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
            return self.statements[0].clone().token_literal();
        } else {
            return Token::ILLEGAL;
        }
    }
    fn eval(&mut self) -> Object {
        Object::NULL
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in self.statements.iter() {
            f.write_str(&s.to_string())?;
        }
        Ok(())
    }
}

impl Node for Statement {
    fn token_literal(&mut self) -> Token {
        match self {
            Statement::LetStatement {
                token,
                name: _,
                value: _,
            } => token.clone(),
            Statement::ReturnStatement { token, value: _ } => token.clone(),
            Statement::ExpressionStatement {
                token,
                expression: _,
            } => token.clone(),
            Statement::BlockStatement {
                token,
                statements: _,
            } => token.clone(),
        }
    }
    fn eval(&mut self) -> Object {
        Object::NULL
    }
}

impl Node for Expression {
    fn token_literal(&mut self) -> Token {
        match self {
            Expression::Identifier { token, value: _ } => token.clone(),
            Expression::IntegerLiteral { token, value: _ } => token.clone(),
            Expression::PrefixExpression {
                token,
                operator: _,
                right: _,
            } => token.clone(),
            Expression::InfixExpression {
                token,
                left: _,
                operator: _,
                right: _,
            } => token.clone(),
            Expression::Boolean { token, value: bool } => token.clone(),
            Expression::IfExpression {
                token,
                condition: _,
                consequence: _,
                alternative: _,
            } => token.clone(),
            Expression::FunctionalLiteral {
                token,
                parameters: _,
                body: _,
            } => token.clone(),
            Expression::CallExpression {
                token,
                function: _,
                arguments: _,
            } => token.clone(),
            Expression::Defa => Token::ILLEGAL,
        }
    }
    fn eval(&mut self) -> Object {
        Object::NULL
    }
}
