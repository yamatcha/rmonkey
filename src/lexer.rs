use crate::token;
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

    fn next_token(&mut self) -> token::Token {
        let tok = match self.ch as char {
            '=' => token::ASSIGN,
            ';' => token::SEMICOLON,
            '(' => token::LPAREN,
            ')' => token::RPAREN,
            ',' => token::COMMA,
            '+' => token::PLUS,
            '{' => token::LBRACE,
            '}' => token::RBRACE,
            _ => token::EOF,
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_next_token() {
        let input: &str = "=+(){},;";
        let tests = [
            token::ASSIGN,
            token::PLUS,
            token::LPAREN,
            token::RPAREN,
            token::LBRACE,
            token::RBRACE,
            token::COMMA,
            token::SEMICOLON,
            token::EOF,
        ];
        let mut l = Lexer::new(input);
        for test in tests.iter() {
            let tok = l.next_token();
            assert!(tok == *test);
        }
    }
}
