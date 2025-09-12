//! 这个模块是整个编译器错误处理系统的核心。
//! 它使用 `thiserror` 和 `miette` 来定义所有结构化的诊断信息。

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// 顶层的编译器错误枚举。
/// 所有编译阶段（词法、语法、类型检查等）的错误都会被包含在这里。
#[derive(Debug, Error, Diagnostic)]
pub enum CompilerError {
    /// 词法分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexical(#[from] LexerError),

    /// 语法分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Parsing(#[from] ParserError),
    // --- 在这里可以添加未来的错误类型 ---
    // #[diagnostic(transparent)]
    // Parsing(#[from] ParserError),
    // #[diagnostic(transparent)]
    // TypeChecking(#[from] TypeError),
}

/// 词法分析器可能产生的所有错误的集合
#[derive(Debug, Error, Diagnostic)]
pub enum LexerError {
    #[error("无法识别的字符: '{unrecognized_char}'")]
    #[diagnostic(
        code(E0001),
        help("这个字符在语言中不是一个有效的符号。请检查是否有拼写错误或多余的字符。")
    )]
    UnrecognizedToken {
        unrecognized_char: char,
        #[label("这个字符无法被识别")]
        span: SourceSpan,
    },
    // --- 在这里可以添加未来更具体的词法错误 ---
    // 例如: 未闭合的字符串字面量
    // #[error("未闭合的字符串字面量")]
    // #[diagnostic(code(E0002))]
    // UnterminatedString {
    //     #[label("这个字符串从这里开始，但没有找到结束的引号")]
    //     span: SourceSpan,
    // },
}

/// 语法分析器可能产生的所有错误的集合。
#[derive(Debug, Error, Diagnostic)]
pub enum ParserError {
    #[error("语法错误: 期望 {expected}, 但找到了 {found}")]
    #[diagnostic(
        code(E0100),
        help("请检查语法并确保其符合语言规则。")
    )]
    UnexpectedToken {
        expected: String,
        found: String,
        #[label("在这里")]
        span: SourceSpan,
    },
}