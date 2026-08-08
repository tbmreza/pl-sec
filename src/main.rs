use bapak::lexer::Lexer;
use bapak::parser::Parser;

/// A small program exercising literals, `let`, assignment, `if`/`else` and a
/// `for` loop, plus most levels of the precedence table.
const SOURCE: &str = r#"let N = 10;
let x = 0;
let flag = true;

if flag && N > 3 {
    x = x + 2 * N - 1 / 3;
} else {
    x = -1;
}

for i in 1..N {
    x = x + i * i;
}

{
    x = !flag * x;
}
"#;

fn main() {
    println!("=== Source ===\n{SOURCE}\n");

    let tokens = match Lexer::new(SOURCE).tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("lex error: {e}");
            std::process::exit(1);
        }
    };

    println!("=== Token stream ===");
    for (i, tok) in tokens.iter().enumerate() {
        println!("{i:>2}: {tok}");
    }

    println!();
    let mut parser = Parser::new(tokens);
    match parser.parse_program() {
        Ok(stmts) => {
            println!("=== AST (pretty-printed) ===");
            for stmt in &stmts {
                println!("{stmt}");
                println!();
            }
        }
        Err(e) => {
            eprintln!("parse error: {e}");
            std::process::exit(1);
        }
    }
}