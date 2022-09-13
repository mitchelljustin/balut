use std::num::ParseIntError;
use std::str::FromStr;

use crate::scanner::ErrorKind::{IllegalChar, IntParseFailed};
use crate::token::{Location, ScannedToken, Token};
use crate::token::Token::*;

#[derive(Debug)]
pub struct ScannerError {
    kind: ErrorKind,
    location: usize,
}

#[derive(Debug)]
pub enum ErrorKind {
    IllegalChar(char),
    IntParseFailed(ParseIntError),
}

const SPACES_PER_INDENT: i32 = 2;

const ALLOWED_PUNCT: &[&'static str] = &[
    ":",
    ".",
    "::",
    "=",
    "=>",
    "+",
    "(",
    ")",
    "~",
];

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<ScannedToken>,
    index: usize,
    start: usize,
    loc: Location,
    indent_level: i32,
}

macro consume_all($self:expr, $pat:pat) {
while !$self.is_at_end() && matches!($self.current(), $pat) {
        $self.increment();
    }
}

pub fn scan(source: String) -> Result<Vec<ScannedToken>, ScannerError> {
    Scanner::new(source).scan()
}

impl Scanner {
    fn new(mut source: String) -> Scanner {
        source.push('\n');
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            index: 0,
            start: 0,
            loc: Default::default(),
            indent_level: 0,
        }
    }

    fn current(&self) -> char {
        self.source[self.index]
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), ErrorKind> {
        match self.advance() {
            'a'..='z' | '_' =>
                self.ident_like(Ident),
            'A'..='Z' =>
                self.ident_like(Nomen),
            '0'..='9' =>
                self.number()?,
            '"' =>
                self.string()?,
            ' ' => {}
            '\n' =>
                self.newline()?,
            first =>
                self.sym(first)?
        }
        Ok(())
    }

    fn sym(&mut self, first: char) -> Result<(), ErrorKind> {
        let one_char = first.to_string();
        if self.is_at_end() {
            let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == one_char) else {
                return Err(IllegalChar(first));
            };
            self.add(Sym(punct));
            return Ok(());
        }
        let second = self.current();
        let mut two_char = one_char.clone();
        two_char.push(second);
        if let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == two_char) {
            self.add(Sym(punct));
            self.increment();
            return Ok(());
        }
        if let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == one_char) {
            self.add(Sym(punct));
            return Ok(());
        }
        return Err(IllegalChar(first));
    }

    fn newline(&mut self) -> Result<(), ErrorKind> {
        consume_all!(self, '\n');
        for _ in 0..(self.index - self.start) {
            self.add(Newline);
        }
        let spaces_start = self.index;
        consume_all!(self, ' ');
        let n_spaces = (self.index - spaces_start) as i32;
        let level = n_spaces / SPACES_PER_INDENT;
        let diff = level - self.indent_level;
        for _ in 0..diff {
            self.add(Indent);
        }
        for _ in diff..0 {
            self.add(Dedent);
        }
        self.indent_level = level;
        Ok(())
    }

    fn lexeme(&self) -> String {
        self.source[self.start..self.index].into_iter().collect()
    }

    fn scan(mut self) -> Result<Vec<ScannedToken>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.index;
            self.scan_token()
                .map_err(|kind| ScannerError { kind, location: self.index })?;
        }
        self.add(EOF);
        return Ok(self.tokens);
    }

    fn number(&mut self) -> Result<(), ErrorKind> {
        consume_all!(self, '0'..='9');
        let lexeme = self.lexeme();
        let value = i32::from_str(&lexeme).map_err(IntParseFailed)?;
        self.add(IntLit(value));
        Ok(())
    }

    fn string(&mut self) -> Result<(), ErrorKind> {
        while !self.is_at_end() && self.current() != '"' {
            self.increment();
        }
        self.increment();
        let mut literal = self.lexeme();
        literal.remove(0);
        literal.remove(literal.len() - 1);
        self.add(StrLit(literal));
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.current();
        self.increment();
        return c;
    }

    fn increment(&mut self) {
        self.index += 1;
        self.loc.col += 1;
    }

    fn add(&mut self, t: Token) {
        if matches!(t, Token::Newline) {
            self.loc.line += 1;
            self.loc.col = 0;
        }
        let loc = self.loc.clone();
        self.tokens.push(ScannedToken { t, loc });
    }

    fn ident_like(&mut self, make_token: impl FnOnce(String) -> Token) {
        self.consume_all_alphanum();
        let name = self.lexeme();
        self.add(make_token(name))
    }

    fn consume_all_alphanum(&mut self) {
        consume_all!(self, '0'..='9' | 'A'..='Z' | 'a'..='z' | '_');
    }
}

