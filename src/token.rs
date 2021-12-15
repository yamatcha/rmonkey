use self::TokenType::*;
use std::io::Error;
use std::io::ErrorKind;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Token(TokenType);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
}

pub const ILLEGAL: Token = Token(Illegal);
pub const EOF: Token = Token(Eof);

pub const IDENT: Token = Token(Ident);
pub const INT: Token = Token(Int);

pub const ASSIGN: Token = Token(Assign);
pub const PLUS: Token = Token(Plus);

pub const COMMA: Token = Token(Comma);
pub const SEMICOLON: Token = Token(Semicolon);

pub const LPAREN: Token = Token(Lparen);
pub const RPAREN: Token = Token(Rparen);
pub const LBRACE: Token = Token(Lbrace);
pub const RBRACE: Token = Token(Rbrace);

pub const FUNCTION: Token = Token(Function);
pub const LET: Token = Token(Let);

pub fn from_bytes(src: &[u8]) -> Result<Token, Error> {
    match src {
        b"ILLEGAL" => Ok(ILLEGAL),
        b"IDENT" => Ok(IDENT),
        b"" => Ok(INT),
        b"=" => Ok(ASSIGN),
        b"+" => Ok(PLUS),
        b"," => Ok(COMMA),
        b";" => Ok(SEMICOLON),
        b"(" => Ok(LPAREN),
        b")" => Ok(RPAREN),
        b"{" => Ok(LBRACE),
        b"}" => Ok(RBRACE),
        b"FUNCTION" => Ok(FUNCTION),
        b"LET" => Ok(LET),
        _ => Err(Error::new(ErrorKind::Other, "oh no!")),
    }
}
