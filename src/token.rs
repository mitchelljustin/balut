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

#[derive(Default, Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct ScannedToken {
    pub t: Token,
    pub loc: Location,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Location { line, col } = self;
        write!(f, "{line}:{col}")
    }
}
