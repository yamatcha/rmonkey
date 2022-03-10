use self::Token::*;
use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,
    EQ,
    NOTEq,

    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            EOF => "\0",
            IDENT(x) => x,
            INT(x) => x,
            ASSIGN => "==",
            PLUS => "+",
            MINUS => "-",
            BANG => "!",
            ASTERISK => "*",
            SLASH => "/",
            LT => "<",
            GT => ">",
            EQ => "==",
            NOTEq => "!=",
            COMMA => ",",
            SEMICOLON => ";",
            LPAREN => "(",
            RPAREN => ")",
            LBRACE => "{",
            RBRACE => "}",
            FUNCTION => "fn",
            LET => "let",
            TRUE => "true",
            FALSE => "false",
            IF => "if",
            ELSE => "else",
            RETURN => "return",
            _ => "",
        })?;
        Ok(())
    }
}

pub fn lookup_ident(ident: String) -> Token {
    match ident.as_str() {
        "fn" => FUNCTION,
        "let" => LET,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        "return" => RETURN,
        _ => IDENT(ident),
    }
}
