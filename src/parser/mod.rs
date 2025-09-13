//! src/parser/mod.rs
//!
//! 这个模块是你编译器语法分析阶段的公共接口。
//! 它的职责是接收一个 Token 流，并将其转换为一个抽象语法树 (AST)。

// 声明子模块。`pub mod ast` 使 AST 定义可以被编译器其他部分访问。
pub mod ast;
mod parsers;
//测试模块
#[cfg(test)]
mod test;

use crate::reporter::{CompilerError, ParserError};
use crate::lexer::Token;
use crate::parser::ast::Program;
use crate::utils::Span;
use chumsky::input::{Stream, Input};
use chumsky::Parser;
// 从内部实现模块中导入解析器构建函数
use parsers::program_parser;

/// 这是 parser 模块唯一的公共入口函数。
///
/// 它是一个纯函数，职责非常单一：
/// - 输入: 源代码的总长度 (用于 EOI span) 和一个 Token 向量。
/// - 输出: 一个元组，包含可选的 Program AST 和一个潜在的语法错误向量。
///
/// 它不关心词法分析或最终的错误报告，只专注于语法分析这一件事。
pub fn parse(
    source_len: usize,
    tokens: Vec<(Token, Span)>,
) -> (Option<Program>, Vec<CompilerError>) {

    let mut errors: Vec<CompilerError> = Vec::new();

    // --- 1. 创建 Token 流 ---
    // `chumsky` 需要一个 Token 流 (Stream) 作为输入。
    // 还需要提供一个文件末尾 (End of Input) 的 span，以便在代码意外结束时报告错误。
    let eoi_span = Span::new(source_len, source_len);
    let token_stream = Stream::from_iter(tokens)
        .map(eoi_span, |(token, span)| (token, span.into()));

    // --- 2. 获取并运行解析器 ---
    let parser = program_parser();
    let (ast, parse_errors) = parser.parse(token_stream).into_output_errors();

    // --- 3. 将 chumsky 的错误转换为我们自己的结构化错误 ---
    // 这一步是连接 chumsky 和我们 miette 诊断系统的关键。
    for error in parse_errors {
        // 格式化“实际找到的 (found)” Token，使其更易读。
        let found = error
            .found()
            .map_or("文件末尾".to_string(), |tok| {
                // 使用 Token 的 Debug 实现来显示它
                format!("`{}`", tok)
            });

        // 格式化“期望的 (expected)” Token 列表。
        let expected = if error.expected().len() == 0 {
            "其他东西".to_string()
        } else {
            error
                .expected()
                .map(|expected_pattern| expected_pattern.to_string())
                .collect::<Vec<_>>()
                .join(" 或 ")
        };

        // 创建我们自定义的、结构化的 ParserError。
        let parser_error = ParserError::UnexpectedToken {
            expected,
            found,
            span: error.span().clone().into(),
        };

        // `.into()` 会利用 #[from] 宏自动将 ParserError 包装成 CompilerError::Parsing
        errors.push(parser_error.into());
    }

    // --- 4. 返回结果 ---
    (ast, errors)
}