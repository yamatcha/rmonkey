use self::Token::*;

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

//pub fn from_bytes(src: &[u8]) -> Result<Token, Error> {
//    match src {
//        b"ILLEGAL" => Ok(ILLEGAL),
//        b"IDENT" => Ok(IDENT),
//        b"" => Ok(INT),
//        b"=" => Ok(ASSIGN),
//        b"+" => Ok(PLUS),
//        b"," => Ok(COMMA),
//        b";" => Ok(SEMICOLON),
//        b"(" => Ok(LPAREN),
//        b")" => Ok(RPAREN),
//        b"{" => Ok(LBRACE),
//        b"}" => Ok(RBRACE),
//        b"FUNCTION" => Ok(FUNCTION),
//        b"LET" => Ok(LET),
//        _ => Err(Error::new(ErrorKind::Other, "oh no!")),
//    }
//}

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
