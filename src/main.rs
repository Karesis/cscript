//! src/main.rs
//! 这个文件现在是一个非常简单的二进制驱动程序。
//! 它的唯一职责就是处理命令行参数、读取文件，然后调用我们的编译器库。

// 使用我们自己的库！请将 "cscript_ref" 替换成你的包名。
use cscript_ref::{
    parser::parse,
    lexer::lex
};

fn main() {
    let source_code = r#"
        main() -> int {
            const my_var: int = 10;
            return 0;
        }
    "#;

    let tokens = lex(source_code).0;
    let ast = parse(source_code.len(), tokens).0;
    println!("{:?}", ast);
}