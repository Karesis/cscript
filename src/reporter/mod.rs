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

    /// 模块解析阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    Resolver(#[from] ResolverError),

    /// 代码生成阶段的错误
    #[error(transparent)]
    #[diagnostic(transparent)]
    CodeGen(#[from] CodeGenError),
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

/// 代码生成阶段可能产生的所有错误的集合。
#[derive(Debug, Error, Diagnostic)]
pub enum CodeGenError {
    #[error("LLVM 模块验证失败: {message}")]
    #[diagnostic(
        code(E0300),
        help("这是一个编译器自身的 Bug，通常意味着生成的 LLVM IR 不合法。请向编译器开发者报告此问题。")
    )]
    LLVMVerificationFailed {
        message: String,
    },

    #[error("内部代码生成错误: {message}")]
    #[diagnostic(
        code(E0301),
        help("这是一个编译器自身的 Bug，发生在尝试将 HIR 转换为 LLVM IR 的过程中。请报告此问题。")
    )]
    InternalError {
        message: String,
        #[label("{message}")]
        span: SourceSpan,
    },

    #[error("全局变量的初始化表达式必须是常量")]
    #[diagnostic(
        code(E0302),
        help("全局变量在编译时就必须确定其值，因此不能使用函数调用或其他变量作为其初始值。")
    )]
    NonConstantGlobalInitializer {
        #[label("此表达式不是一个常量")]
        span: SourceSpan,
    },
}

/// 模块解析器可能产生的所有错误的集合。
#[derive(Debug, Error, Diagnostic)]
pub enum ResolverError {
    #[error("无法读取文件: {path}")]
    #[diagnostic(
        code(E0401), // 新的错误码
        help("请检查文件是否存在以及程序是否有读取权限。详细错误: {io_error}")
    )]
    FileReadError {
        path: String,
        io_error: String,
        #[label("尝试读取这个文件时失败")]
        span: SourceSpan, // 这个 span 会指向导致读取的 `use` 语句
    },

    #[error("无法规范化文件路径: {path}")] // <-- 确认这个变体存在
    #[diagnostic(
        code(E0402),
        help("这通常由无效的符号链接或权限问题引起。详细错误: {io_error}")
    )]
    CanonicalizationError {
        path: String,
        io_error: String,
        #[label("在处理这个路径时出错")]
        span: SourceSpan,
    },

    #[error("找不到模块: 无法解析路径 '{path}'")]
    #[diagnostic(
        code(E0404), // 借用 HTTP 404 的灵感 :)
        help("请检查路径是否正确，以及文件是否存在。")
    )]
    ModuleNotFound {
        path: String,
        #[label("在这条 `use` 语句中引用的模块找不到")]
        span: SourceSpan,
    },
    // 我们将在这里根据需要添加更多错误...
}