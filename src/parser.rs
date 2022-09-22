use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

use ErrorKind::MatchExhausted;

use crate::ast::{Literal, Node};
use crate::parser::ErrorKind::{ConsumeFailed, ExpectedIndent, IllegalLiteral, Unknown};
use crate::parser::ParserError::ParseError;
use crate::scanner;
use crate::scanner::ScannerError;
use crate::token::{Location, ScannedToken, Token};
use crate::token::Token::*;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ErrorKind {
    ConsumeFailed { expected: &'static str, actual: Token },
    MatchExhausted { rule: &'static str, token: Token },
    IllegalLiteral { reason: &'static str, node: Option<Node> },
    ExpectedIndent,
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
    sym_union![")" | "," | "]"] |
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
        self.add_step(rule, format!("returning '{node}'"));
    }

    fn returning_list(&mut self, rule: &str, nodes: &[Node]) {
        let string = nodes
            .iter()
            .map(|n| format!("'{n}'"))
            .collect::<Vec<_>>()
            .join(", ");
        self.add_step(rule, format!("returning {string}"));
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
        match self.program() {
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

    fn program(&mut self) -> NodeResult {
        self.entering("program");
        self.add_step("program", format!("consuming whitespace before program"));
        while matches!(self.current(), patterns::whitespace!()) {
            self.increment();
        }
        let mut elements = Vec::new();
        let mut should_continue = true;
        while !matches!(self.current(), EOF) {
            if !should_continue {
                return self.consume_failed("newline after statement");
            }
            let statement = self.statement()?;
            self.add_step("program", format!("appending {statement}"));
            elements.push(statement);
            should_continue = matches!(self.current(), Newline);
            if should_continue {
                self.increment();
            }
            self.add_step("program", format!("consuming any whitespace after statement"));
            while matches!(self.current(), patterns::whitespace!()) {
                self.increment();
            }
        }
        let node = Node::Sequence { elements };
        self.returning("program", &node);
        Ok(node)
    }

    fn match_exhausted(&self, rule: &'static str) -> NodeResult {
        Err(MatchExhausted { rule, token: self.current().clone() })
    }

    fn consume_failed<T>(&self, expected: &'static str) -> Result<T, ErrorKind> {
        Err(ConsumeFailed { expected, actual: self.current().clone() })
    }

    fn advance(&mut self) -> Token {
        self.increment();
        self.previous().clone()
    }

    fn sequence(&mut self) -> NodeResult {
        self.entering("sequence");
        let mut indent_level = 0;
        self.add_step("sequence", format!("consuming newline"));
        consume!(self, Newline);
        while let Indent = self.current() {
            indent_level += 1;
            self.add_step("sequence", format!("consuming indent {indent_level}"));
            self.increment();
        }
        if indent_level == 0 {
            return self.consume_failed("indent");
        }
        let mut elements = Vec::new();
        loop {
            let element = self.statement()?;
            self.add_step("sequence", format!("appending '{element}'"));
            elements.push(element);
            match self.current() {
                Newline => {
                    self.add_step("sequence", format!("consuming newline(s) after statement"));
                    self.increment();
                    while let Newline = self.current() {
                        self.increment();
                    }
                }
                Dedent => {
                    break;
                }
                _ => return self.consume_failed("newline or dedent")
            }
        }
        for i in 1..=indent_level {
            self.add_step("sequence", format!("consuming dedent {i}"));
            consume!(self, Dedent);
        }
        consume!(self, Newline);
        let node = Node::Sequence { elements };
        self.returning("sequence", &node);
        Ok(node)
    }

    fn statement(&mut self) -> NodeResult {
        self.assignment()
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
        self.returning("assignment", &node);
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
        let mut elements = self.multi_list(
            |t| matches!(t, patterns::phrase_terminator!()),
        )?;
        terms.append(&mut elements);

        let node = match <[_; 1]>::try_from(terms) {
            Ok([head]) => head,
            Err(terms) => Node::Phrase { terms },
        };
        self.returning("phrase", &node);
        Ok(node)
    }

    fn multi_list(&mut self, should_terminate: impl Fn(&Token) -> bool) -> Result<Vec<Node>, ErrorKind> {
        self.entering("multi_list");
        if matches!(self.current(), Newline) && matches!(self.next(), Indent) {
            self.add_step("multi_list", format!("adding from sequence"));
            let Node::Sequence { elements } = self.sequence()? else {
                return Err(Unknown);
            };
            self.returning_list("multi_list", &elements);
            return Ok(elements);
        }
        let mut elements = Vec::new();
        let mut is_at_end = false;
        while !should_terminate(self.current()) && !is_at_end {
            let element = self.annotation()?;
            self.add_step("multi_list", format!("adding element '{element}'"));
            elements.push(element);
            if matches!(self.current(), Sym(",")) {
                self.increment();
            } else {
                is_at_end = true;
            }
        }
        self.returning_list("multi_list", &elements);
        Ok(elements)
    }

    fn annotation(&mut self) -> NodeResult {
        self.entering("annotation");
        let mut node = self.unary_high()?;
        if matches!(self.current(), Nomen(_)) {
            let ty = self.path()?;
            node = Node::Annotation { value: Box::new(node), ty: Box::new(ty) };
        }
        self.returning("annotation", &node);
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
            Sym("[") => self.array_dict(),
            Nomen(_) => self.path(),
            Newline => {
                if !matches!(self.next(), Indent) {
                    return Err(ExpectedIndent);
                }
                self.sequence()
            }
            _ => self.match_exhausted("primary"),
        }
    }

    fn array_dict(&mut self) -> NodeResult {
        self.entering("array_dict");
        consume!(self, Sym("["));
        if let Sym("=") = self.current() {
            self.increment();
            consume!(self, Sym("]"));
            let node = Node::Literal { value: Literal::Dict(HashMap::new()) };
            self.returning("array_dict", &node);
            return Ok(node);
        }
        let elements = self.multi_list(
            |t| matches!(t, Sym("]"))
        )?;
        consume!(self, Sym("]"));
        let n_dict_items = elements.iter()
            .filter(|el| matches!(el, Node::Assignment { .. }))
            .count();

        let value =
            if n_dict_items == elements.len() {
                let mut dict = HashMap::new();
                for assn in elements {
                    let Node::Assignment { target, value, operator: "=" } = assn else {
                        return Err(IllegalLiteral { reason: "dict entries must be of form [key]=[value]", node: Some(assn) });
                    };
                    let Node::Ident { name } = *target else {
                        return Err(IllegalLiteral { reason: "dict key must be an ident", node: Some(*target) });
                    };
                    if let Node::Assignment { .. } = *value {
                        return Err(IllegalLiteral { reason: "dict value must be an expr", node: Some(*value) });
                    }
                    dict.insert(name, *value);
                }
                Literal::Dict(dict)
            } else if n_dict_items == 0 {
                Literal::Array(elements)
            } else {
                return Err(IllegalLiteral { reason: "illegal dict or array", node: None });
            };
        let node = Node::Literal { value };
        self.returning("array_dict", &node);
        Ok(node)
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
        let node = Node::Path { components };
        self.returning("path", &node);
        Ok(node)
    }
}