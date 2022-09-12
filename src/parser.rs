use ErrorKind::MatchExhausted;
use crate::ast::{Literal, Node};
use crate::parser::ErrorKind::ConsumeFailed;
use crate::scanner;
use crate::scanner::{ScannerError, Token};

#[derive(Debug)]
pub enum ParserError {
    ScannerError(ScannerError),
    ParseError(ErrorKind),
}

#[derive(Debug)]
pub enum ErrorKind {
    ScanError(ScannerError),
    ConsumeFailed { expected: &'static str, actual: Token },
    MatchExhausted { rule_name: &'static str, token: Token },
    IllegalEOF,
}

type NodeResult = Result<Node, ErrorKind>;
type TokenResult = Result<Token, ErrorKind>;

pub fn parse_source(source: String) -> Result<Node, ParserError> {
    let tokens = scanner::scan(source).map_err(ParserError::ScannerError)?;
    parse(tokens)
}

pub fn parse(tokens: Vec<Token>) -> Result<Node, ParserError> {
    Parser::new(tokens).parse().map_err(ParserError::ParseError)
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

mod operators {
    const ASSIGNMENT: &[&'static str] = &["=", "=>"];
    const BINARY: &[&'static str] = &["+", "-", "/", "*"];
    const UNARY: &[&'static str] = &["-", "!", "~"];
}

macro consume_failed($self:expr, $expected:expr) {
return Err($crate::parser::ErrorKind::ConsumeFailed { actual: $self.current().clone(), expected: $expected })
}

macro consume($self:expr, $pat:pat) {
{
    match $self.current() {
        $pat => {
            $self.increment();
        },
        _ => consume_failed!($self, stringify!($pat)),
    }
}
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            index: 0,
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.index - 1]
    }

    fn next(&self) -> &Token {
        &self.tokens[self.index + 1]
    }

    fn increment(&mut self) {
        self.index += 1;
    }

    fn parse(mut self) -> NodeResult {
        self.sequence(false)
    }

    fn match_exhausted(&self, rule_name: &'static str) -> NodeResult {
        Err(MatchExhausted { rule_name, token: self.current().clone() })
    }

    fn consume_failed(&self, expected: &'static str) -> NodeResult {
        Err(ConsumeFailed { expected, actual: self.previous().clone() })
    }

    fn advance(&mut self) -> Token {
        let token = self.current().clone();
        self.increment();
        token
    }

    fn sequence(&mut self, indent: bool) -> NodeResult {
        if indent {
            consume!(self, Token::Newline);
            consume!(self, Token::Indent);
        }
        let mut statements = Vec::new();
        while (indent && !matches!(self.current(), Token::Dedent)) || (!indent && !matches!(self.current(), Token::EOF)) {
            let statement = self.statement()?;
            statements.push(statement);
        }
        if indent {
            consume!(self, Token::Dedent);
        }
        Ok(Node::Sequence {
            statements
        })
    }

    fn statement(&mut self) -> NodeResult {
        let expr = self.expr()?;
        consume!(self, Token::Newline);
        Ok(expr)
    }

    fn expr(&mut self) -> NodeResult {
        Ok(self.phrase()?)
    }

    fn phrase(&mut self) -> NodeResult {
        let head = self.ident()?;
        let mut terms = vec![head];
        while !matches!(self.current(), Token::Newline) {
            let term = self.primary()?;
            terms.push(term)
        }
        if matches!(self.current(), Token::Newline) && matches!(self.next(), Token::Indent) {
            let final_seq = self.sequence(true)?;
            terms.push(final_seq);
        }
        Ok(Node::Phrase { terms })
    }

    fn primary(&mut self) -> NodeResult {
        match self.current() {
            Token::Ident(_) => self.ident(),
            Token::Integer(_) => self.integer(),
            Token::String(_) => self.string(),
            Token::Punct("(") => self.grouping(),
            _ => self.match_exhausted("primary"),
        }
    }


    fn integer(&mut self) -> NodeResult {
        let Token::Integer(value) = self.advance() else {
            return self.consume_failed("integer");
        };
        Ok(Node::Literal { value: Literal::Integer(value) })
    }

    fn string(&mut self) -> NodeResult {
        let Token::String(value) = self.advance() else {
            return self.consume_failed("string");
        };
        Ok(Node::Literal { value: Literal::String(value) })
    }

    fn ident(&mut self) -> NodeResult {
        let Token::Ident(name) = self.advance() else {
            return self.consume_failed("ident");
        };
        Ok(Node::Ident { name })
    }

    fn grouping(&mut self) -> NodeResult {
        consume!(self, Token::Punct("("));
        let body = Box::new(self.expr()?);
        consume!(self, Token::Punct(")"));
        Ok(Node::Grouping { body })
    }
}