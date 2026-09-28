use std::fmt;

/// A literal value appearing in the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

/// Prefix (unary) operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

/// Infix (binary) operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    And,
    Or,
}

/// An expression produced by the Pratt parser.
///
/// Grouping (`( expr )`) is not represented as a node: parentheses only
/// adjust precedence and are transparent to the tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Identifier(String),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Assign(String, Box<Expr>),
}

/// A braced list of statements: `{ stmt* }`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Stmt>);

/// A statement produced by the recursive-descent statement parser.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    If(Expr, Block, Option<Block>),
    For(String, Expr, Expr, Block),
    Expr(Expr),
    Block(Block),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(value) => write!(f, "{value}"),
            Lit::Bool(value) => write!(f, "{value}"),
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnOp::Neg => f.write_str("-"),
            UnOp::Not => f.write_str("!"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => f.write_str("+"),
            BinOp::Sub => f.write_str("-"),
            BinOp::Mul => f.write_str("*"),
            BinOp::Div => f.write_str("/"),
            BinOp::Eq => f.write_str("=="),
            BinOp::Ne => f.write_str("!="),
            BinOp::Lt => f.write_str("<"),
            BinOp::Gt => f.write_str(">"),
            BinOp::And => f.write_str("&&"),
            BinOp::Or => f.write_str("||"),
        }
    }
}

/// Renders an expression as a fully parenthesised S-expression, so every
/// grammatical ambiguity is removed and precedence decisions are visible.
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{lit}"),
            Expr::Identifier(name) => f.write_str(name),
            Expr::Unary(op, operand) => write!(f, "({op}{operand})"),
            Expr::Binary(op, lhs, rhs) => write!(f, "({lhs} {op} {rhs})"),
            Expr::Assign(target, value) => write!(f, "{target} = {value}"),
        }
    }
}

/// Renders a statement as indented, round-trippable source.
impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_stmt(f, self, 0)
    }
}

impl Block {
    /// Number of statements in the block.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<Stmt>> for Block {
    fn from(stmts: Vec<Stmt>) -> Self {
        Block(stmts)
    }
}

fn write_stmt(
    f: &mut fmt::Formatter<'_>,
    stmt: &Stmt,
    depth: usize,
) -> fmt::Result {
    let pad = "  ".repeat(depth);

    match stmt {
        Stmt::Let(name, value) => {
            write!(f, "{pad}let {name} = {value};")
        }
        Stmt::Expr(expr) => write!(f, "{pad}{expr};"),
        Stmt::If(cond, then_branch, else_branch) => {
            write!(f, "{pad}if {cond} ")?;
            write_block(f, then_branch, depth)?;
            if let Some(else_branch) = else_branch {
                write!(f, " else ")?;
                write_block(f, else_branch, depth)?;
            }
            Ok(())
        }
        Stmt::For(name, from, to, body) => {
            write!(f, "{pad}for {name} in {from}..{to} ")?;
            write_block(f, body, depth)
        }
        Stmt::Block(block) => write_block(f, block, depth),
    }
}

fn write_block(
    f: &mut fmt::Formatter<'_>,
    block: &Block,
    depth: usize,
) -> fmt::Result {
    if block.is_empty() {
        return f.write_str("{}");
    }

    writeln!(f, "{{")?;
    for stmt in &block.0 {
        write_stmt(f, stmt, depth + 1)?;
        writeln!(f)?;
    }
    write!(f, "{}}}", "  ".repeat(depth))
}