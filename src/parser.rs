use std::error::Error;
use std::fmt::{Display, Formatter};

use ErrorKind::MatchExhausted;

use crate::ast::{Literal, Node};
use crate::parser::ErrorKind::{ConsumeFailed, ExpectedIndent, IllegalFinalSequence, IllegalSyntheticNewline, Unknown};
use crate::parser::ParserError::ParseError;
use crate::scanner;
use crate::scanner::ScannerError;
use crate::token::{Location, ScannedToken, Token};
use crate::token::Token::*;

#[derive(Debug)]
pub enum ParserError {
    ScanError(ScannerError),
    ParseError { kind: ErrorKind, loc: Location, derivation: Derivation },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ScanError(err) =>
                write!(f, "ScanError({err})"),
            ParseError { kind, loc, derivation } =>
                write!(f, "{derivation}\n\n{kind:?} at {loc}")
        }
    }
}

impl Error for ParserError {}

#[derive(Debug)]
pub enum ErrorKind {
    ConsumeFailed { expected: &'static str, actual: Token },
    MatchExhausted { rule: &'static str, token: Token },
    ExpectedIndent,
    IllegalFinalSequence,
    IllegalSyntheticNewline,
    Unknown,
}

type NodeResult = Result<Node, ErrorKind>;

pub fn parse_source(source: &str) -> Result<Node, ParserError> {
    let tokens = scanner::scan(source).map_err(ParserError::ScanError)?;
    parse(tokens)
}

pub fn parse(tokens: Vec<ScannedToken>) -> Result<Node, ParserError> {
    Parser::new(tokens).parse()
}


#[derive(Default, Debug, Clone)]
pub struct Derivation {
    steps: Vec<String>,
}

impl Display for Derivation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (step, report) in self.steps.iter().enumerate() {
            write!(f, "{step:>6} -- {report}\n")?;
        }
        Ok(())
    }
}

pub struct Parser {
    tokens: Vec<ScannedToken>,
    index: usize,
    synthetic_newlines: usize,
    derivation: Derivation,
}


mod patterns {
    pub macro sym_union($($sym:literal)|+) {
    $crate::token::Token::Sym($($sym)|+)
    }

    pub macro assignment() {
    sym_union!["=" | "=>"]
    }

    pub macro binary() {
    sym_union!["+" | "-" | "/" | "*"]
    }

    pub macro unary_low() {
    sym_union!["-"]
    }

    pub macro unary_high() {
    sym_union!["!" | "~" | "&"]
    }

    pub macro whitespace() {
    $crate::token::Token::Newline |
    $crate::token::Token::Dedent |
    $crate::token::Token::Indent
    }

    pub macro phrase_terminator() {
    $crate::token::Token::Newline |
    $crate::token::Token::EOF |
    $crate::token::Token::Dedent |
    sym_union![")"] |
    binary!() |
    assignment!()
    }
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
            synthetic_newlines: 0,
            derivation: Default::default(),
        }
    }


    fn add_step(&mut self, rule: &str, step: String) {
        self.derivation.steps.push(format!(
            "{rule:>10}: {step}"
        ));
    }

    fn entering(&mut self, rule: &str) {
        self.add_step(rule, "entering".to_string());
    }

    fn returning(&mut self, rule: &str, node: &Node) {
        self.add_step(rule, format!("returning '{node}'"))
    }

    fn token_at(&self, offset: isize) -> &ScannedToken {
        let index = (self.index as isize + offset) as usize;
        &self.tokens[index]
    }

    fn peek(&self, offset: isize) -> &Token {
        &self.token_at(offset).tok
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
        let ScannedToken { tok, loc } = &self.tokens[self.index];
        self.derivation.steps.push(format!("            eating {tok:?} at {loc}"));
        self.index += 1;
    }

    fn parse(mut self) -> Result<Node, ParserError> {
        match self.sequence(true) {
            Ok(root) => {
                let derivation = self.derivation;
                println!("Derivation OK: \n{derivation}");
                Ok(root)
            }
            Err(kind) =>
                Err(ParseError {
                    kind,
                    loc: self.token_at(0).loc.clone(),
                    derivation: self.derivation,
                })
        }
    }

    fn match_exhausted(&self, rule: &'static str) -> NodeResult {
        Err(MatchExhausted { rule, token: self.current().clone() })
    }

    fn consume_failed(&self, expected: &'static str) -> NodeResult {
        Err(ConsumeFailed { expected, actual: self.previous().clone() })
    }

    fn advance(&mut self) -> Token {
        self.increment();
        self.previous().clone()
    }

    fn sequence(&mut self, top_level: bool) -> NodeResult {
        self.entering("sequence");
        let mut indent_level = 0;
        if top_level {
            while matches!(self.current(), patterns::whitespace!()) {
                self.add_step("sequence", format!("consuming space after statement"));
                self.increment();
            }
        } else {
            self.add_step("sequence", format!("consuming newline+indent"));
            consume!(self, Newline);
            while let Indent = self.current() {
                indent_level += 1;
                self.add_step("sequence", format!("consuming indent {indent_level}"));
                self.increment();
            }
            if indent_level == 0 {
                return self.consume_failed("indent");
            }
        }
        let mut statements = Vec::new();
        while (!top_level && !matches!(self.current(), Dedent))
            || (top_level && !matches!(self.current(), EOF)) {
            let statement = self.statement()?;
            self.add_step("sequence", format!("appending '{statement}'"));
            statements.push(statement);
            if top_level {
                while matches!(self.current(), patterns::whitespace!()) {
                    self.add_step("sequence", format!("consuming space after statement"));
                    self.increment();
                }
            }
        }
        for i in 1..=indent_level {
            self.add_step("sequence", format!("consuming dedent {i}"));
            consume!(self, Dedent);
        }
        self.synthetic_newlines += 1;
        let node = Node::Sequence { statements };
        self.returning("sequence", &node);
        Ok(node)
    }

    fn statement(&mut self) -> NodeResult {
        self.entering("statement");
        let node = self.assignment()?;
        if self.synthetic_newlines > 0 {
            self.add_step("statement", format!("consuming synthetic newline {}", self.synthetic_newlines));
            self.synthetic_newlines -= 1;
        } else {
            consume!(self, Newline);
        }
        self.returning("statement", &node);
        Ok(node)
    }

    fn assignment(&mut self) -> NodeResult {
        self.entering("assignment");
        let mut node = self.expression()?;
        while matches!(self.current(), patterns::assignment!()) {
            let Sym(operator) = self.advance() else { break; };
            self.add_step("assignment", format!("adding operator '{operator}'"));
            let value = Box::new(self.expression()?);
            node = Node::Assignment {
                target: Box::new(node),
                operator,
                value,
            };
            self.add_step("assignment", format!("collecting '{node}'"));
        }
        Ok(node)
    }

    fn expression(&mut self) -> NodeResult {
        self.unary_low()
    }

    fn unary_low(&mut self) -> NodeResult {
        self.entering("unary_low");
        if matches!(self.current(), patterns::unary_low!()) {
            let Sym(operator) = self.advance() else { return Err(Unknown); };
            let body = Box::new(self.unary_low()?);
            let node = Node::Unary { operator, body };
            self.returning("unary_low", &node);
            return Ok(node);
        }
        let node = self.binary()?;
        self.returning("unary_low", &node);
        Ok(node)
    }

    fn binary(&mut self) -> NodeResult {
        self.entering("binary");
        let mut node = self.phrase()?;

        while matches!(self.current(), patterns::binary!()) {
            let Sym(operator) = self.advance() else { break; }; // should never happen
            self.add_step("binary", format!("adding operator '{operator}'"));
            let rhs = Box::new(self.phrase()?);
            node = Node::Binary {
                lhs: Box::new(node),
                operator,
                rhs,
            };
            self.add_step("binary", format!("collecting '{node}'"));
        }

        self.returning("binary", &node);
        Ok(node)
    }

    fn phrase(&mut self) -> NodeResult {
        self.entering("phrase");
        let head = self.unary_high()?;
        let mut terms = vec![head];
        while !matches!(self.current(), patterns::phrase_terminator!()) {
            let term = self.unary_high()?;
            self.add_step("phrase", format!("adding term '{term}'"));
            terms.push(term);
        }
        if matches!(self.current(), Newline) && matches!(self.next(), Indent) {
            let Node::Sequence { mut statements } = self.sequence(false)? else {
                return Err(Unknown);
            };
            let stmts_string = statements.iter()
                .map(|t| format!("{t}"))
                .collect::<Vec<_>>()
                .join("; ");
            self.add_step("phrase", format!("adding terms from sequence '{stmts_string}'"));
            terms.append(&mut statements);
        }
        let node = match <[Node; 1]>::try_from(terms) {
            Ok([head]) => head,
            Err(terms) => Node::Phrase { terms },
        };
        self.returning("phrase", &node);
        Ok(node)
    }


    fn unary_high(&mut self) -> NodeResult {
        self.entering("unary_high");
        if matches!(self.current(), patterns::unary_high!()) {
            let Sym(operator) = self.advance() else { return Err(Unknown); };
            let body = Box::new(self.unary_high()?);
            let node = Node::Unary { operator, body };
            self.returning("unary_high", &node);
            return Ok(node);
        }
        let node = self.primary()?;
        self.returning("unary_high", &node);
        Ok(node)
    }

    fn primary(&mut self) -> NodeResult {
        self.entering("primary");
        match self.current() {
            Ident(_) => self.ident(),
            IntLit(_) => self.integer(),
            StrLit(_) => self.string(),
            Sym("(") => self.grouping(),
            Nomen(_) => self.path(),
            Newline => {
                if !matches!(self.next(), Indent) {
                    return Err(ExpectedIndent);
                }
                self.sequence(false)
            }
            _ => self.match_exhausted("primary"),
        }
    }


    fn integer(&mut self) -> NodeResult {
        self.entering("integer");
        let IntLit(value) = self.advance() else {
            return self.consume_failed("integer");
        };
        let node = Node::Literal { value: Literal::Int(value) };
        self.returning("integer", &node);
        Ok(node)
    }

    fn string(&mut self) -> NodeResult {
        self.entering("string");
        let StrLit(value) = self.advance() else {
            return self.consume_failed("string");
        };
        let node = Node::Literal { value: Literal::String(value) };
        self.returning("string", &node);
        Ok(node)
    }

    fn ident(&mut self) -> NodeResult {
        self.entering("ident");
        let Ident(name) = self.advance() else {
            return self.consume_failed("ident");
        };
        let node = Node::Ident { name };
        self.returning("ident", &node);
        Ok(node)
    }

    fn grouping(&mut self) -> NodeResult {
        self.entering("grouping");
        consume!(self, Sym("("));
        if matches!(self.current(), Sym(")")) {
            self.increment();
            let node = Node::Nil;
            self.returning("grouping", &node);
            return Ok(node);
        }
        let body = Box::new(self.expression()?);
        consume!(self, Sym(")"));
        let node = Node::Grouping { body };
        self.returning("grouping", &node);
        Ok(node)
    }

    fn nomen(&mut self) -> NodeResult {
        self.entering("nomen");
        let Nomen(name) = self.advance() else {
            return self.consume_failed("nomen");
        };
        let node = Node::Nomen { name };
        self.returning("nomen", &node);
        Ok(node)
    }

    fn path(&mut self) -> NodeResult {
        self.entering("path");
        let head = self.nomen()?;
        let mut components = vec![head];
        while matches!(self.current(), Sym("::")) {
            self.increment();
            let component = self.nomen()?;
            components.push(component);
        }
        let node = match <[Node; 1]>::try_from(components) {
            Ok([nomen]) => nomen,
            Err(components) => Node::Path { components },
        };
        self.returning("path", &node);
        Ok(node)
    }
}