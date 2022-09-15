use std::fmt::{Display, Formatter};
use crate::types::Integer;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Nomen(String),
    Sym(&'static str),
    IntLit(Integer),
    StrLit(String),
    Newline,
    Indent,
    Dedent,
    EOF,
}

const ALLOWED_SYMS: &[&'static str] = &[
    "::",
    "=>",
    ":",
    ".",
    "=",
    "+",
    "-",
    "*",
    "/",
    "(",
    ")",
    "~",
    "!",
    "&",
];

pub fn sym_allowed(sym: &str) -> Option<&'static str> {
    ALLOWED_SYMS
        .iter()
        .find(|&&s| s == sym)
        .map(|s| *s)
}

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Default for Location {
    fn default() -> Self {
        Location {
            line: 1,
            col: 0,
        }
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
