#![feature(once_cell)]
#![feature(decl_macro)]
#![feature(half_open_range_patterns)]
#![feature(let_else)]

use std::error::Error;
use std::fs;

use crate::parser::ParserError;

mod scanner;
mod parser;
mod ast;
mod types;
mod token;
mod interpreter;


fn main() -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string("example.torol")?;
    let tokens = scanner::scan(&source)?;
    let just_tokens = tokens.clone().into_iter().map(|tok| tok.tok).collect::<Vec<_>>();
    println!("{just_tokens:?}");
    let root = match parser::parse(tokens) {
        Ok(root) => root,
        Err(ref err @ ParserError::ParseError { ref derivation, .. }) => {
            eprintln!("{derivation}");
            return Err(Box::new(err.clone()));
        }
        Err(err) => return Err(Box::new(err)),
    };
    println!("{root}");
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init()?;
    interpreter.eval(root)?;
    Ok(())
}
