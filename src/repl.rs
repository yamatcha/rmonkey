use crate::lexer::*;
use crate::parser::*;
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Read;
use std::io::{self, Write};

static PROMPT: &str = ">> ";

pub fn start<R, W>(src: R, mut out: W) -> std::io::Result<()>
where
    R: Read,
    W: Write,
{
    let mut reader = BufReader::new(src);
    loop {
        out.write(PROMPT.as_bytes())?;
        io::stdout().flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let l = Lexer::new(&line);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        out.write(program.to_string().as_bytes())?;
        out.write(b"\n")?;
    }
}
