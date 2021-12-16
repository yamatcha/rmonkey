use crate::token;
use crate::token::Token;
pub struct Lexer {
    input: String,
    position: i32,
    read_position: i32,
    ch: u8,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut l = Self {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        return l;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as i32 {
            self.ch = 0;
        } else {
            self.ch = self.input.chars().nth(self.read_position as usize).unwrap() as u8;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position as usize;
        while is_letter(self.ch as char) {
            self.read_char()
        }
        self.input.clone().as_str()[position..self.position as usize].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position as usize;
        while is_digit(self.ch as char) {
            self.read_char()
        }
        self.input.clone().as_str()[position..self.position as usize].to_string()
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch as char {
            '=' => Token::ASSIGN,
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '!' => Token::BANG,
            '/' => Token::SLASH,
            '*' => Token::ASTERISK,
            '<' => Token::LT,
            '>' => Token::GT,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '\0' => Token::EOF,
            _ => {
                if is_letter(self.ch as char) {
                    return token::lookup_ident(self.read_identifier());
                } else if is_digit(self.ch as char) {
                    return Token::INT(self.read_number());
                } else {
                    Token::ILLEGAL
                }
            }
        };
        self.read_char();
        tok
    }
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_next_token() {
        let input: &str = "let five = 5;
let ten = 10;

let add = fn(x, y){
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
            ";
        let tests = [
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::GT,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::EOF,
        ];
        let mut l = Lexer::new(input);
        for test in tests.iter() {
            let tok = l.next_token();
            println!("{:?} token", tok);
            assert!(tok == *test);
        }
    }
}
