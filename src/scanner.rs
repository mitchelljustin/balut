use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use std::str::FromStr;

use crate::scanner::ErrorKind::{IllegalChar, IntParseFailed};
use crate::token::{sym_allowed, Location, ScannedToken, Token};
use crate::token::Token::*;
use crate::types::Int;

#[derive(Debug)]
pub struct ScannerError {
    kind: ErrorKind,
    loc: Location,
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let ScannerError { kind, loc } = self;
        write!(f, "{kind:?} at {loc}")
    }
}

impl Error for ScannerError {}

#[derive(Debug)]
pub enum ErrorKind {
    IllegalChar(char),
    IntParseFailed(ParseIntError),
}

const SPACES_PER_INDENT: i32 = 2;

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

pub fn scan(source: &str) -> Result<Vec<ScannedToken>, ScannerError> {
    Scanner::new(&source).scan()
}

impl Scanner {
    fn new(source: &str) -> Scanner {
        let mut source: Vec<char> = source.chars().collect();
        if let Some(&ch) = source.last() {
            if ch != '\n' {
                source.push('\n');
            }
        }
        Scanner {
            source,
            tokens: Default::default(),
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
            '#' =>
                self.comment(),
            first =>
                self.sym(first)?
        }
        Ok(())
    }

    fn comment(&mut self) {
        while !self.is_at_end() && !matches!(self.current(), '\n') {
            self.increment();
        }
    }

    fn sym(&mut self, first: char) -> Result<(), ErrorKind> {
        let one_char = first.to_string();
        if self.is_at_end() {
            let Some(sym) = sym_allowed(&one_char) else {
                return Err(IllegalChar(first));
            };
            self.add(Sym(sym));
            return Ok(());
        }
        let second = self.current();
        let mut two_char = one_char.clone();
        two_char.push(second);
        if let Some(sym) = sym_allowed(&two_char) {
            self.add(Sym(sym));
            self.increment();
            return Ok(());
        }
        if let Some(sym) = sym_allowed(&one_char) {
            self.add(Sym(sym));
            return Ok(());
        }
        return Err(IllegalChar(first));
    }

    fn newline(&mut self) -> Result<(), ErrorKind> {
        self.add(Newline);
        consume_all!(self, ' ');
        let num_spaces = (self.index - self.start - 1) as i32;
        let level = num_spaces / SPACES_PER_INDENT;
        let diff = level - self.indent_level;
        for _ in 0..diff {
            self.add(Indent);
        }
        for _ in diff..0 {
            self.add(Dedent);
        }
        self.indent_level = level;
        self.loc.col = 0;
        self.loc.line += 1;
        Ok(())
    }

    fn lexeme(&self) -> String {
        self.source[self.start..self.index].into_iter().collect()
    }

    fn scan(mut self) -> Result<Vec<ScannedToken>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.index;
            self.scan_token()
                .map_err(|kind| ScannerError { kind, loc: self.loc.clone() })?;
        }
        self.add(EOF);
        return Ok(self.tokens);
    }

    fn number(&mut self) -> Result<(), ErrorKind> {
        consume_all!(self, '0'..='9');
        let lexeme = self.lexeme();
        let value = Int::from_str(&lexeme).map_err(IntParseFailed)?;
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
        c
    }

    fn increment(&mut self) {
        self.index += 1;
        self.loc.col += 1;
    }

    fn add(&mut self, tok: Token) {
        let loc = self.loc.clone();
        self.tokens.push(ScannedToken { tok, loc });
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

