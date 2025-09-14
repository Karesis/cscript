use std::hash::Hash;
use std::fmt;
use crate::utils::Span;

/// AST 的根节点，代表整个源文件。
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<GlobalItem>,
    pub span: Span,
}

/// 程序中的顶层项，例如函数或全局变量。
#[derive(Debug, Clone)]
pub enum GlobalItem {
    Use(UseDecl), // 用于use声明
    Extern(ExternBlock), 
    Struct(StructDef),
    Function(FunctionDef),
    VarDecl(VarDecl), // 用于全局变量
}

// --- 顶层结构 --- //

/// 代表一个 `extern "ABI" { ... }` 块。
#[derive(Debug, Clone)]
pub struct ExternBlock {
    // 应用程序二进制接口（ABI）字符串，例如 "C"。
    pub abi: String,
    pub declarations: Vec<FunctionDecl>,
    pub span: Span,
}
/// 代表一个完整的 `struct` 定义。
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Ident,
    // 结构体包含一个字段声明的向量（名称: 类型）。
    pub fields: Vec<(Ident, Type)>, 
    pub span: Span,
}

/// 代表一个完整的函数定义。
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub return_type: Type,
    pub name: Ident,
    pub params: Vec<(Type, Ident)>,
    pub body: Block,
    pub span: Span,
}

/// 代表一个没有函数体的函数声明，目前只用于 `extern` 块中。
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Ident,
    pub params: Vec<(Type, Ident)>,
    pub return_type: Type,
    // 一个标志，用于指示函数是否是可变参数的（接受 `...` 参数）。
    pub is_variadic: bool,
    pub span: Span,
}

/// 一个语句块，由 `{ ... }` 包围。
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

// --- 模块系统 --- //

/// 代表一个 `use` 声明，例如 `use root::io::File;`
#[derive(Debug, Clone)]
pub struct UseDecl {
    /// 导入的路径，例如 `root::io`
    pub path: UsePath,
    /// 导入的目标，例如 `*` 或 `{File, Dir}`
    pub target: UseTree,
    pub span: Span,
}

/// 代表 `use` 语句中的路径部分
#[derive(Debug, Clone)]
pub struct UsePath {
    /// 路径的类型（是否以 root 或 super 开头）
    pub kind: PathKind,
    /// 路径的各个分段
    pub segments: Vec<Ident>,
    pub span: Span,
}

/// 路径的类型
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathKind {
    /// 从项目根开始，例如 `root::...`
    Root,
    /// 从父模块开始，例如 `super::...`
    Super,
    /// 相对路径，例如 `my_module::...`
    Relative,
}

/// 代表 `use` 路径后导入的目标
#[derive(Debug, Clone)]
pub enum UseTree {
    /// 简单导入，没有 `*` 或 `{}`，例如 `use path;`
    Simple,
    /// 通配符导入，例如 `use path::*;`
    Glob,
    /// 嵌套列表导入，例如 `use path::{item1, item2 as alias};`
    Nested {
        items: Vec<NestedUseItem>,
        span: Span,
    },
}

/// 嵌套 `use` 列表中的单个项
#[derive(Debug, Clone)]
pub enum NestedUseItem {
    /// 一个简单的项，例如 `File`
    Simple {
        name: Ident,
    },
    /// 一个重命名的项，例如 `Response as Resp`
    Renamed {
        original: Ident,
        new_name: Ident,
    },
}

// --- 语句 --- //

/// 语言中任何有效的语句。
#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block),
    VarDecl(VarDecl),
    Expr(Expression),
    If {
        condition: Expression,
        then_branch: Box<Block>,
        else_branch: Option<Box<Block>>,
        span: Span,
    },
    While {
        condition: Expression,
        body: Box<Block>,
        span: Span,
    },
    Return {
        value: Option<Expression>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
}

/// 变量声明。
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub is_const: bool,
    pub var_type: Type,
    pub name: Ident,
    pub init: Option<Expression>,
    pub span: Span,
}

// --- 表达式 --- //

/// 任何可以求值为一个值的有效表达式。
#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
}

/// 代表表达式的具体种类。
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// 字面量，例如 `123`, `"hello"`, `true`。
    Literal(LiteralValue),

    /// 变量引用，例如 `x`。
    Variable(Ident),

    /// 聚合字面量，用于初始化结构体等，例如 `{ 1, add(2, 3) }`。
    AggregateLiteral {
        values: Vec<Expression>,
    },

    /// 一元运算，例如 `-x` 或 `!flag`。
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

    /// 赋值操作，例如 `x = y`。
    Assignment {
        left: Box<Expression>, 
        right: Box<Expression>,
    },

    /// 函数调用，例如 `my_func(a, b)`。
    FunctionCall {
        name: Ident,
        args: Vec<Expression>,
    },

    /// 成员访问，用于访问结构体的字段，例如 `my_struct.field`。
    MemberAccess {
        expression: Box<Expression>,
        member: Ident,
    },
}

/// 代表一元运算符。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp { 
    /// 逻辑非 `!`，例如 `!true`。
    Not,
    /// 取负 `-`，例如 `-5`。
    Negate,
    /// 取地址 `&`，例如 `&x`。
    AddressOf,
    /// 解引用 `*`，例如 `*ptr`。
    Dereference,
}

/// 代表二元运算符。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp { 
    /// 加法 `+`
    Add, 
    /// 减法 `-`
    Subtract, 
    /// 乘法 `*`
    Multiply, 
    /// 除法 `/`
    Divide, 
    /// 取模 `%`
    Modulo, 
    /// 等于 `==`
    Eq, 
    /// 不等于 `!=`
    NotEq, 
    /// 小于 `<`
    Lt, 
    /// 大于 `>`
    Gt, 
    /// 小于等于 `<=`
    Lte, 
    /// 大于等于 `>=`
    Gte, 
    /// 逻辑与 `&&`
    And, 
    /// 逻辑或 `||`
    Or, 
}

/// 代表解析后的字面量的值。
#[derive(Debug, Clone, PartialEq, Eq, Hash)] 
pub enum LiteralValue {
    /// 整数字面量，例如 `42`。
    Integer(String),
    /// 浮点数字面量，例如 `3.14`。
    Float(String),
    /// 字符串字面量，例如 `"hello"`。
    String(String),
    /// 布尔值字面量，`true` 或 `false`。
    Bool(bool),
}

// --- 类型和标识符 --- //

/// 代表源代码中的类型注解。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // --- 传统与基础类型 ---

    /// 通用整数类型，在语义分析阶段通常解析为 `i32`。
    Int,
    /// 字符类型，例如 `'a'`。
    Char,
    /// 布尔类型，`true` 或 `false`。
    Bool,
    /// 空类型 `void`，通常用于表示函数没有返回值。
    Void,

    // --- 精确宽度的整数类型 ---

    /// 8位有符号整数。
    I8,
    /// 16位有符号整数。
    I16,
    /// 32位有符号整数。
    I32,
    /// 64位有符号整数。
    I64,
    /// 8位无符号整数。
    U8,
    /// 16位无符号整数。
    U16,
    /// 32位无符号整数。
    U32,
    /// 64位无符号整数。
    U64,

    // --- 浮点数类型 ---

    /// 32位浮点数。
    F32,
    /// 64位浮点数。
    F64,

    // --- 复合类型 ---

    /// 结构体类型，由其名称标识。
    Struct(Ident),
    /// 指针类型，指向另一种类型 T，语法如 `*T`。
    Ptr(Box<Type>),
}

// 为Type实现Display trait，用于reporter的thiserror报错
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            // 这里会直接调用我们上面为 Ident 实现的 Display
            Type::Struct(ident) => write!(f, "{}", ident),
            // 递归地调用 Display 来处理指针的基类型
            Type::Ptr(base) => write!(f, "*{}", base),
        }
    }
}

/// 一个标识符，例如变量名、函数名或结构体名。
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Ident {
    /// 标识符的字符串名称，例如 "my_variable"。
    pub name: String,
    /// 标识符在源代码中的位置范围（Span），用于错误报告。
    pub span: Span,
}

// 为Ident实现Display trait，用于reporter的thiserror报错
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 当需要将 Ident 显示为字符串时，我们只关心它的名字。
        write!(f, "{}", self.name)
    }
}