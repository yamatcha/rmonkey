use crate::lexer::*;
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Read;
use std::io::{self, Write};

static PROMPT: &str = ">> ";

pub fn start<R, W>(src: R, out: W) -> std::io::Result<()>
where
    R: Read,
    W: Write,
{
    let mut reader = BufReader::new(src);
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let l = Lexer::new(&line);
        for tok in l {
            println!("{:?}", tok);
        }
    }
}
