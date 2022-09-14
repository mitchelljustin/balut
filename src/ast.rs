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
    Unary { operator: &'static str, body: Box<Node> },
    Sequence { statements: Vec<Node> },
    Path { components: Vec<Node> },
    Nomen { name: String },
    Ident { name: String },
    Nil,
}

fn fmt_nodes(f: &mut Formatter<'_>, nodes: &[Node], separator: &str, groupers: Option<(char, char)>) -> std::fmt::Result {
    if let Some((left, _)) = groupers {
        f.write_char(left)?;
    }
    for (i, node) in nodes.iter().enumerate() {
        node.fmt(f)?;
        if i < nodes.len() - 1 {
            f.write_str(separator)?;
        }
    }
    if let Some((_, right)) = groupers {
        f.write_char(right)?;
    }

    Ok(())
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Literal { value } =>
                write!(f, "{value}"),
            Node::Phrase { terms } =>
                fmt_nodes(f, terms, " ", Some(('‹', '›'))),
            Node::Ident { name } | Node::Nomen { name } =>
                write!(f, "{name}"),
            Node::Sequence { statements } =>
                fmt_nodes(f, statements, "; ", Some(('[', ']'))),
            Node::Path { components } =>
                fmt_nodes(f, components, "::", None),
            Node::Grouping { body } =>
                write!(f, "({body})"),
            Node::Binary { lhs, operator, rhs } =>
                write!(f, "‹{lhs} {operator} {rhs}›"),
            Node::Assignment { target, operator, value } =>
                write!(f, "{target} {operator} {value}"),
            Node::Unary { body, operator } =>
                write!(f, "{operator}{body}"),
            Node::Nil =>
                write!(f, "()"),
        }
    }
}

