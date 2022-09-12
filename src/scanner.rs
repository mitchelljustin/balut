use std::num::ParseIntError;
use std::str::FromStr;

use crate::scanner::ErrorKind::{IllegalChar, IntParseFailed};
use crate::types::Integer;

#[derive(Debug)]
pub struct ScannerError {
    kind: ErrorKind,
    location: usize,
}

#[derive(Debug)]
pub enum ErrorKind {
    IllegalChar(char),
    MatchFailed { expected: char, actual: char },
    IntParseFailed(ParseIntError),
}

const SPACES_PER_INDENT: i32 = 2;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Nomen(String),
    Punct(&'static str),
    Integer(Integer),
    String(String),
    Newline,
    Indent,
    Dedent,
    EOF,
}

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
    tokens: Vec<Token>,
    index: usize,
    start: usize,
    indent_level: i32,
}

macro consume_all($self:expr, $pat:pat) {
while !$self.is_at_end() && matches!($self.current(), $pat) {
        $self.increment();
    }
}

pub fn scan(source: String) -> Result<Vec<Token>, ScannerError> {
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
                self.ident_like(Token::Ident),
            'A'..='Z' =>
                self.ident_like(Token::Nomen),
            '0'..='9' =>
                self.number()?,
            '"' =>
                self.string()?,
            ' ' => {}
            '\n' =>
                self.newline()?,
            first => {
                let one_char = first.to_string();
                if self.is_at_end() {
                    let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == one_char) else {
                        return Err(IllegalChar(first));
                    };
                    self.add(Token::Punct(punct));
                    return Ok(());
                }
                let second = self.current();
                let mut two_char = one_char.clone();
                two_char.push(second);
                if let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == two_char) {
                    self.add(Token::Punct(punct));
                    self.increment();
                    return Ok(());
                }
                if let Some(punct) = ALLOWED_PUNCT.iter().find(|&&s| s == one_char) {
                    self.add(Token::Punct(punct));
                    return Ok(());
                }
                return Err(IllegalChar(first));
            }
        }
        Ok(())
    }

    fn newline(&mut self) -> Result<(), ErrorKind> {
        consume_all!(self, '\n');
        for _ in 0..(self.index - self.start) {
            self.add(Token::Newline);
        }
        let spaces_start = self.index;
        consume_all!(self, ' ');
        let n_spaces = (self.index - spaces_start) as i32;
        let level = n_spaces / SPACES_PER_INDENT;
        let diff = level - self.indent_level;
        for _ in 0..diff {
            self.add(Token::Indent);
        }
        for _ in diff..0 {
            self.add(Token::Dedent);
        }
        self.indent_level = level;
        Ok(())
    }

    fn lexeme(&self) -> String {
        self.source[self.start..self.index].into_iter().collect()
    }

    fn scan(mut self) -> Result<Vec<Token>, ScannerError> {
        while !self.is_at_end() {
            self.start = self.index;
            self.scan_token()
                .map_err(|kind| ScannerError { kind, location: self.index })?;
        }
        self.add(Token::EOF);
        return Ok(self.tokens);
    }

    fn number(&mut self) -> Result<(), ErrorKind> {
        consume_all!(self, '0'..='9');
        let lexeme = self.lexeme();
        let value = i32::from_str(&lexeme).map_err(IntParseFailed)?;
        self.add(Token::Integer(value));
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
        self.add(Token::String(literal));
        Ok(())
    }

    fn consume_maybe(&mut self, expected: char) -> bool {
        self.consume(expected).is_ok()
    }

    fn consume(&mut self, expected: char) -> Result<(), ErrorKind> {
        let actual = self.current();
        if actual != expected {
            return Err(ErrorKind::MatchFailed { actual, expected });
        }
        self.increment();
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.current();
        self.increment();
        return c;
    }

    fn increment(&mut self) {
        self.index += 1;
    }

    fn add(&mut self, token: Token) {
        self.tokens.push(token)
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

