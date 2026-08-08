use std::fmt;

use crate::token::Token;

/// A lexical error pointing at the offending byte offset.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub position: usize,
    pub message: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at offset {}: {}", self.position, self.message)
    }
}

impl std::error::Error for LexError {}

/// A hand-rolled lexer that turns a source string into a [`Token`] stream.
///
/// Whitespace is discarded. The keywords are `let`, `if`, `else`, `for` and
/// `in`; `true`/`false` become boolean literals.
pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            tokens: Vec::new(),
        }
    }

    /// Lexes the whole input, consuming the lexer.
    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while let Some(&(_, c)) = self.chars.peek() {
            let token = match c {
                ' ' | '\t' | '\r' | '\n' => {
                    self.chars.next();
                    None
                }
                '0'..='9' => Some(self.lex_number()?),
                'a'..='z' | 'A'..='Z' | '_' => Some(self.lex_identifier()),
                '(' => {
                    self.chars.next();
                    Some(Token::LParen)
                }
                ')' => {
                    self.chars.next();
                    Some(Token::RParen)
                }
                '{' => {
                    self.chars.next();
                    Some(Token::LBrace)
                }
                '}' => {
                    self.chars.next();
                    Some(Token::RBrace)
                }
                ';' => {
                    self.chars.next();
                    Some(Token::Semicolon)
                }
                '+' => {
                    self.chars.next();
                    Some(Token::Plus)
                }
                '-' => {
                    self.chars.next();
                    Some(Token::Minus)
                }
                '*' => {
                    self.chars.next();
                    Some(Token::Star)
                }
                '/' => {
                    self.chars.next();
                    Some(Token::Slash)
                }
                '<' => {
                    self.chars.next();
                    Some(Token::Lt)
                }
                '>' => {
                    self.chars.next();
                    Some(Token::Gt)
                }
                '.' => Some(self.lex_dot()?),
                '=' => Some(self.lex_eq()?),
                '!' => Some(self.lex_bang()?),
                '&' => Some(self.lex_and()?),
                '|' => Some(self.lex_or()?),
                other => {
                    let (position, _) = self.chars.next().expect("peeked char");
                    return Err(LexError {
                        position,
                        message: format!("unexpected character `{other}`"),
                    });
                }
            };

            if let Some(token) = token {
                self.tokens.push(token);
            }
        }

        self.tokens.push(Token::Eof);
        Ok(self.tokens)
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let mut digits = String::new();
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_ascii_digit() {
                digits.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        match digits.parse::<i64>() {
            Ok(value) => Ok(Token::Int(value)),
            Err(_) => Err(LexError {
                position: self.chars.peek().map_or(0, |&(position, _)| position),
                message: format!("integer literal `{digits}` is out of range"),
            }),
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let mut name = String::new();
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                name.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        match name.as_str() {
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "in" => Token::In,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Ident(name),
        }
    }

    fn lex_dot(&mut self) -> Result<Token, LexError> {
        let (position, _) = self.chars.next().expect("peeked char"); // consume '.'
        match self.chars.peek() {
            Some(&(_, '.')) => {
                self.chars.next();
                Ok(Token::DotDot)
            }
            _ => Err(LexError {
                position,
                message: "expected `..`".into(),
            }),
        }
    }

    fn lex_eq(&mut self) -> Result<Token, LexError> {
        self.chars.next(); // consume '='
        match self.chars.peek() {
            Some(&(_, '=')) => {
                self.chars.next();
                Ok(Token::EqEq)
            }
            _ => Ok(Token::Assign),
        }
    }

    fn lex_bang(&mut self) -> Result<Token, LexError> {
        self.chars.next(); // consume '!'
        match self.chars.peek() {
            Some(&(_, '=')) => {
                self.chars.next();
                Ok(Token::BangEq)
            }
            _ => Ok(Token::Bang),
        }
    }

    fn lex_and(&mut self) -> Result<Token, LexError> {
        let (position, _) = self.chars.next().expect("peeked char"); // consume '&'
        match self.chars.peek() {
            Some(&(_, '&')) => {
                self.chars.next();
                Ok(Token::AndAnd)
            }
            _ => Err(LexError {
                position,
                message: "expected `&&`".into(),
            }),
        }
    }

    fn lex_or(&mut self) -> Result<Token, LexError> {
        let (position, _) = self.chars.next().expect("peeked char"); // consume '|'
        match self.chars.peek() {
            Some(&(_, '|')) => {
                self.chars.next();
                Ok(Token::OrOr)
            }
            _ => Err(LexError {
                position,
                message: "expected `||`".into(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(src: &str) -> Vec<Token> {
        Lexer::new(src).tokenize().unwrap()
    }

    #[test]
    fn lexes_mixed_tokens() {
        assert_eq!(
            lex("let x = 10;"),
            vec![
                Token::Let,
                Token::Ident("x".into()),
                Token::Assign,
                Token::Int(10),
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn lexes_operators() {
        assert_eq!(
            lex("a > == < ! = != && || .."),
            vec![
                Token::Ident("a".into()),
                Token::Gt,
                Token::EqEq,
                Token::Lt,
                Token::Bang,
                Token::Assign,
                Token::BangEq,
                Token::AndAnd,
                Token::OrOr,
                Token::DotDot,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn lexes_keywords_and_booleans() {
        assert_eq!(
            lex("for i in true false"),
            vec![
                Token::For,
                Token::Ident("i".into()),
                Token::In,
                Token::Bool(true),
                Token::Bool(false),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn reports_unexpected_characters() {
        let err = Lexer::new("x = 1 @ 2").tokenize().unwrap_err();
        assert!(err.message.contains('@'));
    }

    #[test]
    fn rejects_lone_ampersands() {
        let err = Lexer::new("a & b").tokenize().unwrap_err();
        assert!(err.message.contains("&&"));
    }
}