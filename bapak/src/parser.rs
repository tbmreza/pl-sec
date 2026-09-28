use std::fmt;

use crate::ast::{BinOp, Block, Expr, Lit, Stmt, UnOp};
use crate::token::Token;

/// Binding power of the unary operators `-` and `!`. Right-associative, so the
/// operand is parsed with the full prefix binding power.
const PREFIX_BP: u8 = 15;

/// A syntax error with the token index at which it was detected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub position: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at token {}: {}", self.position, self.message)
    }
}

impl std::error::Error for ParseError {}

/// Result type returned by all parsing entry points.
pub type PResult<T> = Result<T, ParseError>;

/// A recursive-descent parser that mixes statement parsing (recursive
/// descent) with expression parsing (Pratt / top-down operator precedence).
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Builds a parser over a complete token stream. `Eof` is appended if the
    /// lexer did not produce one.
    pub fn new(mut tokens: Vec<Token>) -> Self {
        if tokens.last() != Some(&Token::Eof) {
            tokens.push(Token::Eof);
        }
        Parser { tokens, pos: 0 }
    }

    // --- Token cursor ------------------------------------------------------

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    /// Advances the cursor, never moving past the `Eof` sentinel.
    fn bump(&mut self) {
        if self.pos + 1 < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn at(&self, token: &Token) -> bool {
        self.peek() == token
    }

    fn eat(&mut self, token: &Token) -> bool {
        if self.at(token) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: &Token, what: &str) -> PResult<()> {
        if self.eat(token) {
            Ok(())
        } else {
            Err(self.unexpected(what))
        }
    }

    fn unexpected(&self, what: &str) -> ParseError {
        ParseError {
            position: self.pos,
            message: format!("expected {what}, found `{}`", self.peek()),
        }
    }

    // --- Statements --------------------------------------------------------

    /// `Program := Stmt*`
    pub fn parse_program(&mut self) -> PResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while !self.at(&Token::Eof) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    /// `Stmt := Let | If | For | ExprStmt | Block`
    fn parse_stmt(&mut self) -> PResult<Stmt> {
        match self.peek() {
            Token::Let => {
                self.bump();
                let name = self.parse_ident()?;
                self.expect(&Token::Assign, "`=` between the name and its value")?;
                let value = self.parse_expr(0)?;
                self.expect(&Token::Semicolon, "`;` after `let` declaration")?;
                Ok(Stmt::Let(name, value))
            }
            Token::If => {
                self.bump();
                let condition = self.parse_expr(0)?;
                let then_branch = self.parse_block()?;
                let else_branch = if self.eat(&Token::Else) {
                    Some(self.parse_block()?)
                } else {
                    None
                };
                Ok(Stmt::If(condition, then_branch, else_branch))
            }
            Token::For => {
                self.bump();
                let name = self.parse_ident()?;
                self.expect(&Token::In, "`in` in a `for` loop")?;
                let from = self.parse_expr(0)?;
                self.expect(&Token::DotDot, "`..` between the range bounds")?;
                let to = self.parse_expr(0)?;
                let body = self.parse_block()?;
                Ok(Stmt::For(name, from, to, body))
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                Ok(Stmt::Block(block))
            }
            _ => {
                let expr = self.parse_expr(0)?;
                self.expect(&Token::Semicolon, "`;` after the expression")?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    /// `Block := '{' Stmt* '}'`
    fn parse_block(&mut self) -> PResult<Block> {
        self.expect(&Token::LBrace, "`{`")?;
        let mut body = Vec::new();
        while !self.at(&Token::RBrace) {
            if self.at(&Token::Eof) {
                return Err(self.unexpected("`}` to close the `{` block"));
            }
            body.push(self.parse_stmt()?);
        }
        self.bump(); // consume '}'
        Ok(Block(body))
    }

    fn parse_ident(&mut self) -> PResult<String> {
        match self.peek() {
            Token::Ident(name) => {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            _ => Err(self.unexpected("an identifier")),
        }
    }

    // --- Expressions (Pratt) ----------------------------------------------

    /// Parses an expression with the classic Pratt loop.
    ///
    /// `min_bp` is the minimum binding power that an infix operator must
    /// have for it to be absorbed into the current expression. The `0` call
    /// site therefore parses a whole expression; recursive calls with the
    /// right binding power encode associativity.
    fn parse_expr(&mut self, min_bp: u8) -> PResult<Expr> {
        let mut lhs = self.parse_prefix_expr()?;

        while let Some((left_bp, right_bp)) = binding_power(self.peek()) {
            if left_bp < min_bp {
                break;
            }

            let op_token = self.peek().clone();
            self.bump();
            let rhs = self.parse_expr(right_bp)?;

            lhs = match op_token {
                Token::Assign => {
                    let target = match lhs {
                        Expr::Identifier(name) => name,
                        other => {
                            return Err(ParseError {
                                position: self.pos.saturating_sub(1),
                                message: format!(
                                    "cannot assign to `{other}` — left of `=` must be an identifier"
                                ),
                            });
                        }
                    };
                    Expr::Assign(target, Box::new(rhs))
                }
                _ => {
                    let op = binary_op(&op_token).expect("operator has a binding power");
                    Expr::Binary(op, Box::new(lhs), Box::new(rhs))
                }
            };
        }

        Ok(lhs)
    }

    /// Handles the token in prefix position: literals, identifiers, unary
    /// operators and transparently-elided grouping parentheses.
    fn parse_prefix_expr(&mut self) -> PResult<Expr> {
        match self.peek() {
            Token::Int(value) => {
                let value = *value;
                self.bump();
                Ok(Expr::Literal(Lit::Int(value)))
            }
            Token::Bool(value) => {
                let value = *value;
                self.bump();
                Ok(Expr::Literal(Lit::Bool(value)))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.bump();
                Ok(Expr::Identifier(name))
            }
            Token::Minus => {
                self.bump();
                let operand = self.parse_expr(PREFIX_BP)?;
                Ok(Expr::Unary(UnOp::Neg, Box::new(operand)))
            }
            Token::Bang => {
                self.bump();
                let operand = self.parse_expr(PREFIX_BP)?;
                Ok(Expr::Unary(UnOp::Not, Box::new(operand)))
            }
            Token::LParen => {
                self.bump();
                let inner = self.parse_expr(0)?;
                self.expect(&Token::RParen, "`)` to close the `(`")?;
                Ok(inner)
            }
            _ => Err(self.unexpected("an expression")),
        }
    }
}

/// Explicit `(left_bp, right_bp)` for every infix operator, ordered from
/// loosest to tightest. For left-associative operators `right_bp > left_bp`;
/// the pairs need not be dense.
fn binding_power(tok: &Token) -> Option<(u8, u8)> {
    Some(match tok {
        // Assignment, right-associative.
        Token::Assign => (1, 2),
        // Logical OR, left-associative.
        Token::OrOr => (3, 4),
        // Logical AND, left-associative.
        Token::AndAnd => (5, 6),
        // Equality, left-associative.
        Token::EqEq | Token::BangEq => (7, 8),
        // Comparison, left-associative.
        Token::Lt | Token::Gt => (9, 10),
        // Additive, left-associative.
        Token::Plus | Token::Minus => (11, 12),
        // Multiplicative, left-associative.
        Token::Star | Token::Slash => (13, 14),
        _ => return None,
    })
}

fn binary_op(tok: &Token) -> Option<BinOp> {
    Some(match tok {
        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Star => BinOp::Mul,
        Token::Slash => BinOp::Div,
        Token::EqEq => BinOp::Eq,
        Token::BangEq => BinOp::Ne,
        Token::Lt => BinOp::Lt,
        Token::Gt => BinOp::Gt,
        Token::AndAnd => BinOp::And,
        Token::OrOr => BinOp::Or,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_program(src: &str) -> Vec<Stmt> {
        let tokens = Lexer::new(src).tokenize().unwrap();
        Parser::new(tokens).parse_program().unwrap()
    }

    fn parse_expr(src: &str) -> Expr {
        let mut stmts = parse_program(src);
        match stmts.len() {
            1 => match stmts.remove(0) {
                Stmt::Expr(expr) => expr,
                other => panic!("expected expression statement, got {other:?}"),
            },
            n => panic!("expected one statement, got {n}"),
        }
    }

    #[test]
    fn precedence_add_mul() {
        assert_eq!(parse_expr("1 + 2 * 3;").to_string(), "(1 + (2 * 3))");
        assert_eq!(parse_expr("1 * 2 + 3;").to_string(), "((1 * 2) + 3)");
    }

    #[test]
    fn additive_is_left_associative() {
        assert_eq!(parse_expr("1 - 2 - 3;").to_string(), "((1 - 2) - 3)");
        assert_eq!(parse_expr("1 + 2 + 3;").to_string(), "((1 + 2) + 3)");
    }

    #[test]
    fn multiplicative_is_left_associative() {
        assert_eq!(parse_expr("8 / 2 / 2;").to_string(), "((8 / 2) / 2)");
    }

    #[test]
    fn logical_and_or() {
        assert_eq!(parse_expr("a || b && c;").to_string(), "(a || (b && c))");
        assert_eq!(parse_expr("a && b || c;").to_string(), "((a && b) || c)");
        assert_eq!(parse_expr("a && b && c;").to_string(), "((a && b) && c)");
    }

    #[test]
    fn comparison_and_equality() {
        assert_eq!(parse_expr("1 < 2 == true;").to_string(), "((1 < 2) == true)");
        assert_eq!(parse_expr("a == b != c;").to_string(), "((a == b) != c)");
    }

    #[test]
    fn unary_binds_tightly() {
        assert_eq!(parse_expr("-a * b;").to_string(), "((-a) * b)");
        assert_eq!(parse_expr("-a - b;").to_string(), "((-a) - b)");
        assert_eq!(parse_expr("!a && b;").to_string(), "((!a) && b)");
        assert_eq!(parse_expr("--a;").to_string(), "(-(-a))");
    }

    #[test]
    fn grouping_overrides_precedence() {
        assert_eq!(parse_expr("(1 + 2) * 3;").to_string(), "((1 + 2) * 3)");
        assert_eq!(parse_expr("- (a + b);").to_string(), "(-(a + b))");
    }

    #[test]
    fn assignment_expression() {
        assert_eq!(parse_expr("x = 1 + 2;").to_string(), "x = (1 + 2)");
    }

    #[test]
    fn rejects_assignment_to_non_identifier() {
        let src = "(1 + 2) = 3;";
        let tokens = Lexer::new(src).tokenize().unwrap();
        let err = Parser::new(tokens).parse_program().unwrap_err();
        assert!(err.message.contains("cannot assign"));
    }

    #[test]
    fn parses_let_statement() {
        let mut stmts = parse_program("let x = 1 + 2 * 3;");
        assert_eq!(stmts.len(), 1);
        assert_eq!(
            stmts.remove(0),
            Stmt::Let("x".into(), Expr::Binary(
                BinOp::Add,
                Box::new(Expr::Literal(Lit::Int(1))),
                Box::new(Expr::Binary(
                    BinOp::Mul,
                    Box::new(Expr::Literal(Lit::Int(2))),
                    Box::new(Expr::Literal(Lit::Int(3))),
                )),
            ))
        );
    }

    #[test]
    fn parses_if_with_and_without_else() {
        let stmts = parse_program("if a && b { c = 1; } else { c = 2; }");
        let Stmt::If(cond, then_branch, else_branch) = &stmts[0] else {
            panic!("expected if statement");
        };
        assert_eq!(cond.to_string(), "(a && b)");
        assert_eq!(then_branch.len(), 1);
        assert!(else_branch.is_some());
    }

    #[test]
    fn parses_for_loop() {
        let stmts = parse_program("for i in 0..n { x = x + i; }");
        let Stmt::For(name, from, to, _) = &stmts[0] else {
            panic!("expected for statement");
        };
        assert_eq!(name, "i");
        assert_eq!(from.to_string(), "0");
        assert_eq!(to.to_string(), "n");
    }

    #[test]
    fn round_trips_pretty_printing() {
        let src = "let a = 1 + 2;\nif a > 0 { a = -a; } else { a = 0; }\nfor i in 1..3 { a = a + i; }";
        let stmts = parse_program(src);
        assert_eq!(parse_program(&stmts.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n")), stmts);
    }

    #[test]
    fn reports_missing_semicolons() {
        let src = "let x = 1\nlet y = 2;";
        let tokens = Lexer::new(src).tokenize().unwrap();
        let err = Parser::new(tokens).parse_program().unwrap_err();
        assert!(err.message.contains('`'));
    }
}