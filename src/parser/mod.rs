// src/parser/mod.rs

pub mod ast;
pub mod parsers; 

use crate::lexer::{self, Token, Span};
use crate::parser::ast::Program;
use crate::parser::parsers::program_parser;
use chumsky::prelude::*;
use chumsky::input::Stream;

/// 解析源代码字符串的顶层入口函数
pub fn parse_source<'a>(source_code: &'a str) -> (Option<Program>, Vec<Rich<'a, Token, Span>>) {
    // 1. 创建词法分析器迭代器
    let lex_iter = lexer::lexer(source_code);

    // 2. [FIXED] 使用 for 循环立即处理所有 token 和错误
    // 这种“急切”的执行方式可以完美解决所有权和生命周期问题。
    let mut tokens = Vec::new();
    let mut lex_errs = Vec::new();
    for result in lex_iter {
        match result {
            Ok(token_span) => tokens.push(token_span),
            Err((lex_err, span)) => {
                lex_errs.push(Rich::custom(span, lex_err.to_string()));
            }
        }
    }

    // 3. 配置正确的 Stream
    let eoi = source_code.len()..source_code.len();
    // 现在我们可以安全地使用收集好的 tokens Vec 来创建流
    let stream = Stream::from_iter(tokens)
        .map(eoi, |(token, span)| (token, span));

    // 4. 语法分析
    let (ast, mut parse_errs) = program_parser()
        .parse(stream)
        .into_output_errors();

    // 5. 合并词法和语法错误
    lex_errs.append(&mut parse_errs);

    (ast, lex_errs)
}

#[cfg(test)]
mod tests {
    use super::parse_source;

    #[test]
    fn test_simple_function() {
        let src = "int main() { return 0; }";
        let (ast, errs) = parse_source(src);

        let error_messages: Vec<_> = errs.iter().map(|e| e).collect();
        assert!(error_messages.is_empty(), "Parsing failed with errors: {:?}", error_messages);
        
        assert!(ast.is_some(), "AST should not be None");
    }

    #[test]
    fn test_syntax_error() {
        let src = "int main() { return 0 "; 
        let (ast, errs) = parse_source(src);
        
        assert!(!errs.is_empty(), "Should have reported parsing errors");
    }
}