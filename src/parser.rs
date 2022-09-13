use ErrorKind::MatchExhausted;

use crate::ast::{Literal, Node};
use crate::parser::ErrorKind::{ConsumeFailed, IllegalSyntheticNewline};
use crate::parser::ParserError::ParseError;
use crate::scanner;
use crate::scanner::ScannerError;
use crate::token::{Location, ScannedToken, Token};
use crate::token::Token::*;

#[derive(Debug)]
pub enum ParserError {
    ScannerError(ScannerError),
    ParseError { kind: ErrorKind, loc: Location },
}

#[derive(Debug)]
pub enum ErrorKind {
    ConsumeFailed { expected: &'static str, actual: Token },
    MatchExhausted { rule_name: &'static str, token: Token },
    IllegalSyntheticNewline,
    IllegalEOF,
}

type NodeResult = Result<Node, ErrorKind>;
type TokenResult = Result<Token, ErrorKind>;

pub fn parse_source(source: String) -> Result<Node, ParserError> {
    let tokens = scanner::scan(source).map_err(ParserError::ScannerError)?;
    parse(tokens)
}

pub fn parse(tokens: Vec<ScannedToken>) -> Result<Node, ParserError> {
    Parser::new(tokens).parse()
}

pub struct Parser {
    tokens: Vec<ScannedToken>,
    index: usize,
    synthetic_newline: bool,
}

mod operators {
    const ASSIGNMENT: &[&'static str] = &["=", "=>"];
    const BINARY: &[&'static str] = &["+", "-", "/", "*"];
    const UNARY: &[&'static str] = &["-", "!", "~"];
}


macro consume($self:expr, $pat:pat) {
match $self.current() {
    $pat => $self.increment(),
    _ => return $self.consume_failed(stringify!(pattern: $pat)),
}
}

impl Parser {
    fn new(tokens: Vec<ScannedToken>) -> Parser {
        Parser {
            tokens,
            index: 0,
            synthetic_newline: false,
        }
    }

    fn token_at(&self, offset: isize) -> &ScannedToken {
        let index = (self.index as isize + offset) as usize;
        &self.tokens[index]
    }

    fn peek(&self, offset: isize) -> &Token {
        &self.token_at(offset).t
    }

    fn current(&self) -> &Token {
        self.peek(0)
    }

    fn previous(&self) -> &Token {
        self.peek(-1)
    }

    fn next(&self) -> &Token {
        self.peek(1)
    }

    fn increment(&mut self) {
        self.index += 1;
    }

    fn parse(mut self) -> Result<Node, ParserError> {
        self.sequence(false)
            .map_err(|kind| ParseError {
                kind,
                loc: self.token_at(0).loc.clone(),
            })
    }

    fn match_exhausted(&self, rule_name: &'static str) -> NodeResult {
        Err(MatchExhausted { rule_name, token: self.current().clone() })
    }

    fn consume_failed(&self, expected: &'static str) -> NodeResult {
        Err(ConsumeFailed { expected, actual: self.previous().clone() })
    }

    fn advance(&mut self) -> Token {
        self.increment();
        let tok = self.previous().clone();
        tok
    }

    fn sequence(&mut self, indent: bool) -> NodeResult {
        if indent {
            consume!(self, Newline);
            consume!(self, Indent);
        }
        let mut statements = Vec::new();
        while (indent && !matches!(self.current(), Dedent))
            || (!indent && !matches!(self.current(), EOF)) {
            let statement = self.statement()?;
            statements.push(statement);
        }
        if indent {
            consume!(self, Dedent);
        }
        if self.synthetic_newline {
            return Err(IllegalSyntheticNewline);
        }
        self.synthetic_newline = true;
        Ok(Node::Sequence {
            statements
        })
    }

    fn statement(&mut self) -> NodeResult {
        let expr = self.expression()?;
        if self.synthetic_newline {
            self.synthetic_newline = false;
        } else {
            consume!(self, Newline);
        }
        Ok(expr)
    }

    fn expression(&mut self) -> NodeResult {
        self.phrase()
    }

    fn phrase(&mut self) -> NodeResult {
        let head = self.ident()?;
        let mut terms = vec![head];
        while !matches!(self.current(), Newline | Sym(")")) {
            let term = self.primary()?;
            terms.push(term)
        }
        if matches!(self.current(), Newline) && matches!(self.next(), Indent) {
            let final_seq = self.sequence(true)?;
            terms.push(final_seq);
        }
        Ok(Node::Phrase { terms })
    }

    fn primary(&mut self) -> NodeResult {
        match self.current() {
            Ident(_) => self.ident(),
            IntLit(_) => self.integer(),
            StrLit(_) => self.string(),
            Sym("(") => self.grouping(),
            _ => self.match_exhausted("primary"),
        }
    }


    fn integer(&mut self) -> NodeResult {
        let IntLit(value) = self.advance() else {
            return self.consume_failed("integer");
        };
        Ok(Node::Literal { value: Literal::Integer(value) })
    }

    fn string(&mut self) -> NodeResult {
        let StrLit(value) = self.advance() else {
            return self.consume_failed("string");
        };
        Ok(Node::Literal { value: Literal::String(value) })
    }

    fn ident(&mut self) -> NodeResult {
        let token = self.advance();
        let Ident(name) = token else {
            return self.consume_failed("ident");
        };
        Ok(Node::Ident { name })
    }

    fn grouping(&mut self) -> NodeResult {
        consume!(self, Sym("("));
        let body = Box::new(self.expression()?);
        consume!(self, Sym(")"));
        Ok(Node::Grouping { body })
    }
}