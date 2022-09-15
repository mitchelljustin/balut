#![feature(once_cell)]
#![feature(let_else)]
#![feature(decl_macro)]
#![feature(half_open_range_patterns)]

use std::fs;

use crate::parser::ParserError;

mod scanner;
mod parser;
mod ast;
mod types;
mod token;
mod interpreter;


fn main() {
    let source = fs::read_to_string("example.torol").unwrap();
    let tokens = scanner::scan(&source).unwrap();
    let just_tokens = tokens.clone().into_iter().map(|tok| tok.tok).collect::<Vec<_>>();
    println!("Tokens: {just_tokens:?}");
    match parser::parse(tokens) {
        Ok(root) => println!("{root:#}"),
        Err(ParserError::ParseError { derivation, loc, kind }) => {
            eprintln!("{derivation}");
            eprintln!("Parser Error at {loc}: {kind:?}");
        }
        Err(err) => panic!("Unknown error: {err:?}"),
    }
}
