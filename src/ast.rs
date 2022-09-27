use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};

use crate::types::Int;

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Int(Int),
    Array(Vec<Node>),
    Dict(HashMap<String, Node>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(value) =>
                write!(f, "\"{value}\""),
            Literal::Int(value) =>
                write!(f, "{value}"),
            Literal::Array(elements) =>
                fmt_list(f, elements, ", ", Some(('[', ']'))),
            Literal::Dict(dict) =>
                if dict.is_empty() {
                    write!(f, "[:]")
                } else {
                    fmt_list(
                        f,
                        &dict.iter().map(|(k, v)| format!("{k}: {v}")).collect::<Vec<_>>(),
                        ", ",
                        Some(('[', ']')),
                    )
                }
        }
    }
}


#[derive(Debug, Clone)]
pub enum Node {
    Literal { value: Literal },
    Phrase { terms: Vec<Node> },
    Grouping { body: Box<Node> },
    Annotation { value: Box<Node>, ty: Box<Node> },
    Binary { lhs: Box<Node>, operator: &'static str, rhs: Box<Node> },
    Assignment { target: Box<Node>, operator: &'static str, value: Box<Node> },
    Unary { operator: &'static str, body: Box<Node> },
    Sequence { elements: Vec<Node> },
    Access { target: Box<Node>, member: Box<Node> },
    Path { components: Vec<Node> },
    Nomen { name: String },
    Ident { name: String },
    Nil,
}

fn fmt_list(f: &mut Formatter<'_>, list: &[impl Display], separator: &str, groupers: Option<(char, char)>) -> std::fmt::Result {
    if let Some((left, _)) = groupers {
        f.write_char(left)?;
    }
    for (i, item) in list.iter().enumerate() {
        item.fmt(f)?;
        if i < list.len() - 1 {
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
            Node::Phrase { terms } => {
                write!(f, "‹")?;
                if let Some(head) = terms.first() {
                    write!(f, "{head}")?;
                    if terms.len() > 1 {
                        write!(f, " ")?;
                        fmt_list(f, &terms[1..], ", ", None)?;
                    }
                }
                write!(f, "›")?;
                Ok(())
            }
            Node::Ident { name } | Node::Nomen { name } =>
                write!(f, "{name}"),
            Node::Sequence { elements } =>
                fmt_list(f, elements, "; ", Some(('{', '}'))),
            Node::Path { components } =>
                fmt_list(f, components, "::", None),
            Node::Grouping { body } =>
                write!(f, "({body})"),
            Node::Binary { lhs, operator, rhs } =>
                write!(f, "‹{lhs} {operator} {rhs}›"),
            Node::Assignment { target, operator, value } =>
                write!(f, "{target} {operator} {value}"),
            Node::Access { target, member } =>
                write!(f, "{target}.{member}"),
            Node::Unary { body, operator } =>
                write!(f, "{operator}{body}"),
            Node::Nil =>
                write!(f, "()"),
            Node::Annotation { value, ty } =>
                write!(f, "{value} {ty}"),
        }
    }
}

