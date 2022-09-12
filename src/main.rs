#![feature(once_cell)]
#![feature(let_else)]
#![feature(decl_macro)]
#![feature(half_open_range_patterns)]

use std::fs;

mod scanner;
mod parser;
mod ast;
mod types;


fn main() {
    let source = fs::read_to_string("example.torol").unwrap();
    let tokens = scanner::scan(source).unwrap();
    println!("{tokens:#?}");
    let root = parser::parse(tokens).unwrap();
    println!("{root:#}");
}
