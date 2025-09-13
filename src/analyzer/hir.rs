//! src/analyzer/hir.rs
//!
//! 定义了编译器的高级中间表示（High-level Intermediate Representation, HIR）。
//! HIR 是在语义分析（analyzer）阶段由 AST（抽象语法树）降级（lower）而来的。
//! 与 AST 相比，HIR 的每个节点都经过了完整的语义检查和类型注解，
//! 它包含了代码生成阶段所需的所有信息，例如：
//! 1. 每个表达式的最终推导类型 (`resolved_type`)。
//! 2. 每个变量的确切存储位置（全局变量或栈偏移量 `Storage`）。
//! 3. 变量和函数的使用点直接链接到它们的声明 (`Arc<VarDecl>`)。

use crate::analyzer::types::SemanticType;
use crate::utils::Span;
use crate::parser::ast::{BinaryOp, Ident, UnaryOp};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// 一个可以精确表示 HIR 中整数值的枚举。
/// 它可以无损地存储所有宽度不超过 128 位的有符号或无符号整数。
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum HirIntValue {
    Signed(i128),
    Unsigned(u128),
}

/// 代表一个经过类型检查和解析的字面量值。
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(HirIntValue),
    String(String),
    Bool(bool),
    Float(f64),
}

/// 为 LiteralValue 实现 Hash trait，以便在需要哈希的集合中使用（例如，常量折叠）。
///
/// **注意:** f64 类型本身不实现 Hash，因为它包含 NaN（Not a Number）值，
/// 而 `NaN != NaN`。此实现通过处理其比特位表示来绕过此限制，
/// 确保了哈希行为的一致性。
impl Hash for LiteralValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            LiteralValue::Integer(i) => i.hash(state),
            LiteralValue::String(s) => s.hash(state),
            LiteralValue::Bool(b) => b.hash(state),
            LiteralValue::Float(f) => {
                // 特殊处理-0.0, 0.0, 和 NaN，以确保它们有稳定的哈希值
                if *f == 0.0 { // 同时捕捉 +0.0 和 -0.0
                    0.0f64.to_bits().hash(state);
                } else if f.is_nan() {
                    // 为所有 NaN 值使用一个固定的、规范的比特模式进行哈希
                    0x7ff8000000000001u64.hash(state);
                } else {
                    f.to_bits().hash(state);
                }
            }
        }
    }
}
impl Eq for LiteralValue {}


// --- 核心“注解”信息 ---

/// 表示变量的存储位置。这是 analyzer 添加的关键语义信息之一。
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Storage {
    /// 全局变量，存储在静态数据区。
    Global { name: String },
    /// 局部变量，存储在函数调用栈上，`offset` 是相对于栈基址指针（base pointer）的偏移量。
    Local { offset: i32 },
}

// --- HIR 节点定义 ---

/// HIR 的根节点，代表一个完整的、经过分析的 CScript 程序。
#[derive(Debug)]
pub struct Program {
    /// 程序中定义的所有函数。
    pub functions: Vec<Function>,
    /// 程序中定义的所有全局变量。
    pub globals: Vec<Arc<VarDecl>>,
    /// 通过 `extern` 声明的外部函数。
    pub extern_functions: Vec<Arc<FunctionDecl>>,
}

/// 代表一个函数声明（没有函数体），主要用于 `extern` 函数。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDecl {
    /// 函数名。
    pub name: Ident,
    /// 参数类型列表。
    pub params: Vec<SemanticType>,
    /// 返回值类型。
    pub return_type: SemanticType,
    /// 是否为可变参数函数（例如 `printf`）。
    pub is_variadic: bool,
}

/// 代表一个完整的函数定义（包含函数体）。
#[derive(Debug)]
pub struct Function {
    /// 函数名。
    pub name: Ident,
    /// 参数列表。每个参数都是一个完整的变量声明。
    pub params: Vec<Arc<VarDecl>>,
    /// 返回值类型。
    pub return_type: SemanticType,
    /// 函数体，是一个语句块。
    pub body: Block,
}

/// 代表一个语句块，它定义了一个词法作用域。
#[derive(Debug)]
pub struct Block {
    /// 块内包含的语句列表。
    pub stmts: Vec<Statement>,
}

/// 代表一种语句。语句是执行某些操作但不产生值的代码单元。
#[derive(Debug)]
pub enum Statement {
    /// 嵌套的语句块。
    Block(Block),
    /// 变量声明语句。
    VarDecl(Arc<VarDecl>),
    /// 表达式语句（例如，一个函数调用 `foo();`）。
    Expr(Expression),
    /// if-else 条件语句。
    If {
        condition: Expression,
        then_branch: Block,
        else_branch: Option<Block>,
        span: Span,
    },
    /// while 循环语句。
    While {
        condition: Expression,
        body: Block,
        span: Span,
    },
    /// return 语句。
    Return {
        /// 返回值表达式，对于 `void` 函数则为 `None`。
        value: Option<Expression>,
        span: Span,
    },
    /// break 语句。
    Break(Span),
    /// continue 语句。
    Continue(Span),
}

/// 代表一个完整的、经过语义分析的变量声明。
/// 它被 `Arc` 包裹，因为它可以被多个地方共享引用（声明处和所有使用处）。
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VarDecl {
    /// 变量名。
    pub name: Ident,
    /// 变量的语义类型。
    pub var_type: SemanticType,
    /// 是否为常量。
    pub is_const: bool,
    /// 分析后确定的存储位置。
    pub storage: Storage,
    /// 可选的初始化表达式，也已经过完整分析。
    pub initializer: Option<Expression>,
}

/// 代表一个表达式。表达式是任何可以被求值并产生一个值的代码片段。
#[derive(Debug, Clone)]
pub struct Expression {
    /// 表达式的具体种类。
    pub kind: ExprKind,
    /// 表达式在源代码中的位置，用于错误报告。
    pub span: Span,
    /// **核心注解**: 此表达式经过类型检查后解析出的最终语义类型。
    pub resolved_type: SemanticType,
}

/// 表达式的比较仅基于其 `kind`，忽略 `span` 和 `resolved_type`。
/// 这对于某些优化（如常量折叠）或结构性比较很有用。
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
impl Eq for Expression {}
impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}


/// 表达式的具体种类。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// 字面量，例如 `123`, `"hello"`, `true`。
    Literal(LiteralValue),
    /// 变量引用。
    /// **关键区别**: 这里不再是 AST 中的 `Ident`，而是一个指向其 `VarDecl` 的共享指针。
    /// 这意味着变量的每次使用都直接链接到它的完整声明信息。
    Variable(Arc<VarDecl>),
    /// 结构体成员访问，例如 `my_struct.field`。
    MemberAccess {
        /// `my_struct` 部分的表达式。
        expression: Box<Expression>,
        /// `field` 部分的标识符。
        member: Ident,
    },
    /// 结构体字面量实例化，例如 `MyStruct { field: value }`。
    StructLiteral {
        /// 结构体的完整类型信息。
        struct_type: Arc<SemanticType>,
        /// 每个字段的名称和其对应的初始化表达式。
        fields: Vec<(Ident, Expression)>,
    },
    /// 一元运算，例如 `-x` 或 `!y`。
    UnaryOp {
        op: UnaryOp,
        right: Box<Expression>,
    },
    /// 二元运算，例如 `a + b`。
    BinaryOp {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// 赋值运算，例如 `a = b`。
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// 函数调用。
    FunctionCall {
        name: Ident,
        args: Vec<Expression>,
    },
    /// 取地址运算，例如 `&x`。
    AddressOf(Box<Expression>),
    /// 解引用运算，例如 `*p`。
    Dereference(Box<Expression>),
}