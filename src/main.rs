#![feature(once_cell)]
#![feature(let_else)]
#![feature(decl_macro)]
#![feature(half_open_range_patterns)]

use std::error::Error;
use std::fs;

mod scanner;
mod parser;
mod ast;
mod types;
mod token;
mod interpreter;


fn main() -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string("example.torol")?;
    let tokens = scanner::scan(&source)?;
    // let just_tokens = tokens.clone().into_iter().map(|tok| tok.tok).collect::<Vec<_>>();
    let root = parser::parse(tokens)?;
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.init()?;
    interpreter.eval(&root)?;
    Ok(())
}
