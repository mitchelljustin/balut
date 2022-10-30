use crate::types::Int;
use std::cell::LazyCell;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Nomen(String),
    Sym(&'static str),
    IntLit(Int),
    StrLit(String),
    Newline,
    Indent,
    Dedent,
    EOF,
}

const SYMS: &[&'static str] = &[
    "::", "=>", ":", ".", "=", "+", "-", "*", "/", "(", ")", "[", "]", "~", "!", "&", ",",
];

pub fn find_sym(sym: &str) -> Option<&'static str> {
    let set: LazyCell<HashSet<&'static str>> = LazyCell::new(|| SYMS.iter().cloned().collect());
    set.get(sym).cloned()
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Default for Location {
    fn default() -> Self {
        Location { line: 1, col: 0 }
    }
}

#[derive(Debug, Clone)]
pub struct ScannedToken {
    pub tok: Token,
    pub loc: Location,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Location { line, col } = self;
        write!(f, "{line}:{col}")
    }
}
