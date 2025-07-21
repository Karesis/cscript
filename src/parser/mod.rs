// src/parser/mod.rs

// 声明子模块
pub mod ast;
mod parsers;

// 引入所有需要的模块
use crate::diagnostics::codes::E0100_SYNTAX_ERROR;
use crate::diagnostics::{Diagnostic, DiagnosticBag, Label};
use crate::lexer::{self, Span, Token};
use crate::parser::ast::Program;
use chumsky::input::{Stream, Input};
use chumsky::Parser;

// 从内部实现模块中导入解析器构建函数
use parsers::program_parser;

/// 这是 parser 模块唯一的公共入口函数。
pub fn parse(source: &str, diagnostics: &mut DiagnosticBag) -> Option<Program> {
    // --- 1. 词法分析 ---
    let tokens = lexer::lex(source, diagnostics);

    // --- 2. 提前退出检查 ---
    if diagnostics.has_errors() {
        return None;
    }

    // --- 3. 语法分析 ---
    let eoi_span = source.len()..source.len();
    let token_stream = Stream::from_iter(tokens)
        .map(eoi_span, |(token, span)| (token, span));
    
    let parser = program_parser();

    let (ast, parse_errors) = parser.parse(token_stream).into_output_errors();

    // --- 4. 报告语法错误 ---
    for error in parse_errors {
        let expected_str = if error.expected().len() == 0 {
            "something else".to_string()
        } else {
            error
                .expected()
                .map(|expected| expected.to_string())
                .collect::<Vec<_>>()
                .join(" or ")
        };

        let message = format!(
            "Expected {}, but found {}",
            expected_str,
            // [CORRECTED] The closure now correctly takes a reference to a Token (`tok`)
            // and formats it directly, without trying to access a non-existent field `.0`.
            error.found().map_or("end of input".to_string(), |tok| format!("`{:?}`", tok))
        );

        let diagnostic = Diagnostic::error(&E0100_SYNTAX_ERROR, Label::new(error.span().clone(), message))
            .with_note("Please check the syntax and ensure it conforms to the CScript language rules.");
        
        diagnostics.report(diagnostic);
    }

    // --- 5. 返回最终结果 ---
    if diagnostics.has_errors() {
        None
    } else {
        ast
    }
}
