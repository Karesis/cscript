//! 这个模块是整个编译器错误处理系统的核心。
//! 它使用 `thiserror` 和 `miette` 来定义所有结构化的诊断信息。

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use crate::parser::ast::Ident;

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

    /// 语义分析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Semantic(#[from] SemanticError),
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

/// 语义分析器可能产生的所有错误的集合。
#[derive(Debug, Error, Diagnostic)]
pub enum SemanticError {
    #[error("找不到名为 '{name}' 的变量")]
    #[diagnostic(
        code(E0201),
        help("请检查变量名是否拼写正确，或者它是否在当前作用域内声明。")
    )]
    UndefinedVariable {
        name: String,
        #[label("在此作用域内未定义")]
        span: SourceSpan,
    },

    #[error("类型不匹配: 期望类型 '{expected}', 但找到了 '{found}'")]
    #[diagnostic(
        code(E0202),
        help("如果需要，可以尝试使用显式类型转换。")
    )]
    TypeMismatch {
        expected: String,
        found: String,
        #[label("这个表达式的类型是 '{found}', 但期望的类型是 '{expected}'")]
        span: SourceSpan,
    },

    #[error("重复定义: 符号 '{name}' 已经被定义")]
    #[diagnostic(code(E0203))]
    Redefinition {
        name: String,
        #[label("'{name}' 在这里被重复定义")]
        span: SourceSpan,
        #[label("之前的定义在这里")]
        original_span: SourceSpan,
    },
    
    #[error("无效的赋值目标")]
    #[diagnostic(
        code(E0204),
        help("只有变量、数组元素或结构体成员等（统称为左值 L-values）可以被赋值。")
    )]
    InvalidAssignmentTarget {
        #[label("这里不能作为赋值的目标")]
        span: SourceSpan,
    },
    
    #[error("'{keyword}' 只能在循环内部使用")]
    #[diagnostic(code(E0205))]
    ControlFlowOutsideLoop {
        keyword: String, // "break" or "continue"
        #[label("此 '{keyword}' 语句不在任何循环中")]
        span: SourceSpan,
    },

    #[error("函数调用参数数量不匹配: 期望 {expected} 个, 但提供了 {found} 个")]
    #[diagnostic(code(E0206))]
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
        #[label("函数调用提供了 {found} 个参数")]
        span: SourceSpan,
        #[label("函数定义在这里，需要 {expected} 个参数")]
        definition_span: SourceSpan,
    },

    #[error("找不到类型 '{type_name}'")]
    #[diagnostic(code(E0207))]
    TypeNotFound {
        type_name: Ident,
        #[label("在此作用域内找不到这个类型")]
        span: SourceSpan, // miette 会从 Ident 中自动获取 span
    },

    #[error("符号 '{name}' 不是一个结构体类型")]
    #[diagnostic(code(E0208))]
    SymbolIsNotAStruct {
        name: Ident,
        #[label("期望这里是一个结构体类型，但它不是")]
        span: SourceSpan, // miette 会从 Ident 中自动获取 span
    },
    
    #[error("无效的字面量格式: 无法将 '{content}' 解析为 {kind}")]
    #[diagnostic(code(E0209))]
    InvalidLiteralFormat {
        kind: String, // e.g., "i64" or "f64"
        content: String,
        #[label("此字面量格式无效")]
        span: SourceSpan,
    },

    #[error("整数字面量溢出，无法存入目标类型 '{target_type}'")]
    #[diagnostic(code(E0210))]
    IntegerOverflow {
        target_type: String, // 使用 String 以便显示
        #[label("这个数字对于 '{target_type}' 来说太大了")]
        span: SourceSpan,
    },

    #[error("期望一个值，但找到了类型 '{found}'")]
    #[diagnostic(code(E0211), help("类型名称本身不能作为值使用。"))]
    ExpectedValueFoundType {
        found: String,
        #[label("这是一个类型，不是一个值")]
        span: SourceSpan,
    },

    #[error("期望一个可赋值的左值")]
    #[diagnostic(code(E0212), help("只有变量、解引用的指针或结构体字段等才能被赋值。"))]
    InvalidLValue {
        #[label("这里不能被赋值")]
        span: SourceSpan,
    },

    #[error("不能对常量进行赋值")]
    #[diagnostic(code(E0213))]
    AssignmentToConst {
        #[label("此绑定被声明为常量，不能被修改")]
        span: SourceSpan,
    },

    #[error("符号 '{name}' 不是一个函数")]
    #[diagnostic(code(E0214))]
    NotAFunction {
        name: String,
        #[label("期望这是一个函数，但它不是")]
        span: SourceSpan,
    },

    #[error("在结构体类型 '{struct_type}' 中找不到字段 '{field_name}'")]
    #[diagnostic(code(E0215))]
    FieldNotFound {
        field_name: String,
        struct_type: String,
        #[label("这个字段不存在")]
        span: SourceSpan,
    },

    #[error("对非结构体类型进行成员访问")]
    #[diagnostic(code(E0216))]
    MemberAccessOnNonStruct {
        #[label("这个表达式的类型不是结构体，无法访问其成员")]
        span: SourceSpan,
    },
    
    #[error("结构体初始化时的字段数量不匹配：期望 {expected} 个，实际提供了 {found} 个")]
    #[diagnostic(code(E0217))]
    InvalidFieldCount {
        expected: usize,
        found: usize,
        #[label("此处提供了 {found} 个字段")]
        span: SourceSpan,
    },
}