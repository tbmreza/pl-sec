use std::fmt;

/// A single lexical token produced by the [`crate::lexer::Lexer`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Literals
    Int(i64),
    Bool(bool),
    Ident(String),

    // Keywords
    Let,
    If,
    Else,
    For,
    In,

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Dot,
    DotDot,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    EqEq,
    BangEq,
    Lt,
    Gt,
    AndAnd,
    OrOr,
    Bang,
    Assign,

    // Sentinel
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Int(value) => write!(f, "{value}"),
            Token::Bool(value) => write!(f, "{value}"),
            Token::Ident(name) => f.write_str(name),
            Token::Let => f.write_str("let"),
            Token::If => f.write_str("if"),
            Token::Else => f.write_str("else"),
            Token::For => f.write_str("for"),
            Token::In => f.write_str("in"),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::LBrace => f.write_str("{"),
            Token::RBrace => f.write_str("}"),
            Token::Semicolon => f.write_str(";"),
            Token::Dot => f.write_str("."),
            Token::DotDot => f.write_str(".."),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Star => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            Token::EqEq => f.write_str("=="),
            Token::BangEq => f.write_str("!="),
            Token::Lt => f.write_str("<"),
            Token::Gt => f.write_str(">"),
            Token::AndAnd => f.write_str("&&"),
            Token::OrOr => f.write_str("||"),
            Token::Bang => f.write_str("!"),
            Token::Assign => f.write_str("="),
            Token::Eof => f.write_str("end of input"),
        }
    }
}