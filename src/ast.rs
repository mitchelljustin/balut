use std::fmt::{Display, Formatter, Write};

use crate::types::Integer;

#[derive(Debug)]
pub enum Literal {
    String(String),
    Integer(Integer),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(value) => write!(f, "\"{value}\""),
            Literal::Integer(value) => write!(f, "{value}")
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Literal { value: Literal },
    Phrase { terms: Vec<Node> },
    Grouping { body: Box<Node> },
    Binary { lhs: Box<Node>, operator: &'static str, rhs: Box<Node> },
    Assignment { target: Box<Node>, operator: &'static str, value: Box<Node> },
    Unary { operator: &'static str, rhs: Box<Node> },
    Sequence { statements: Vec<Node> },
    Ident { name: String },
}

fn fmt_nodes(f: &mut Formatter<'_>, nodes: &Vec<Node>, separator: &str, groupers: (char, char)) -> std::fmt::Result {
    f.write_char(groupers.0)?;
    for (i, node) in nodes.iter().enumerate() {
        node.fmt(f)?;
        if i < nodes.len() - 1 {
            f.write_str(separator)?;
        }
    }
    f.write_char(groupers.1)?;
    Ok(())
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Literal { value } =>
                write!(f, "{value}"),
            Node::Phrase { terms } =>
                fmt_nodes(f, terms, " ", ('‹', '›')),
            Node::Ident { name } =>
                write!(f, "{name}"),
            Node::Sequence { statements } =>
                fmt_nodes(f, statements, "; ", ('[', ']')),
            Node::Grouping { body } =>
                write!(f, "({body})"),
            Node::Binary { .. } =>
                Ok(()),
            Node::Assignment { .. } =>
                Ok(()),
            Node::Unary { .. } =>
                Ok(()),
        }
    }
}

