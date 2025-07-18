use crate::diagnostic::Span;
use crate::lexer::{Literal, Operator};

// --- 1. 核心 AST 节点与标识符 ---

/// 所有 AST 节点的通用包装，包含了节点本身和其在源代码中的位置。
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
}
impl<T> Node<T> {
    /// 将一个节点的内部类型 `T` 映射（转换）为类型 `U`，同时保持 `span` 不变。
    ///
    /// 这是一个非常实用的辅助函数，它允许在解析过程中，将一个具体的
    /// AST 节点（如 `Node<ReturnStatement>`）无缝地转换为一个更通用的
    /// AST 节点（如 `Node<Statement>`），而无需手动重建整个 `Node` 结构。
    pub fn map<U, F>(self, f: F) -> Node<U>
    where
        // `F` 是一个只执行一次的闭包（或函数指针），它接收类型 `T` 并返回类型 `U`。
        F: FnOnce(T) -> U,
    {
        Node {
            // 调用闭包 f，对 `kind` 进行转换
            kind: f(self.kind),
            // 保持原来的 `span` 不变
            span: self.span,
        }
    }
}

/// 一个标识符，例如变量名或函数名。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

// --- 2. 程序顶层结构 ---

/// AST 的根节点，代表一个完整的 CScript 源文件。
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    /// 顶层声明（函数和全局变量）的列表。
    pub declarations: Vec<Node<Declaration>>,
}

/// C 程序中的一个顶层声明。
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDefinition),
    Variable(VariableDeclaration),
}

// --- 3. 类型 ---

/// 类型规范，例如 `int`, `char*`, `void`。
#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    /// 命名类型，如 `int`, `char`, `bool`, `void`。
    Named(Identifier),
    /// 指向另一个类型的指针，例如 `*int`。
    Pointer(Box<Node<TypeSpec>>),
}

// --- 4. 定义与声明 ---

/// 函数的定义。
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub params: Vec<Node<Parameter>>,
    pub return_type: Node<TypeSpec>,
    pub body: Node<BlockStatement>,
}

/// 函数定义中的单个参数。
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub type_spec: Node<TypeSpec>,
}

/// 变量声明（可以是全局或局部的）。
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_spec: Node<TypeSpec>,
    /// 变量的可选初始值。
    pub initializer: Option<Node<Expression>>,
}

// --- 5. 语句 ---

/// 语句。C 是一门面向语句的语言。
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// 仅由表达式构成的语句，例如 `my_func();` 或 `x = 1;`。
    Expression(Node<Expression>),
    /// 变量声明语句，例如 `int x = 10;`。
    VariableDeclaration(VariableDeclaration),
    /// `if` (或 `if-else`) 语句。
    If(IfStatement),
    /// `while` 循环语句。
    While(WhileStatement),
    /// `return` 语句。
    Return(ReturnStatement),
    /// 由 `{ ... }` 包围的代码块语句。
    Block(BlockStatement),
}

/// An `if` statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Node<Expression>,
    pub then_block: Box<Node<Statement>>,
    pub else_branch: Option<Box<Node<Statement>>>,
}

/// A `while` loop statement.
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Node<Expression>,
    pub body: Box<Node<Statement>>,
}

/// A `return` statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    /// 要返回的值，对于 `return;` 则为 `void`。
    pub value: Option<Node<Expression>>,
}

/// 代码块语句，也称为复合语句。
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Node<Statement>>,
}

// --- 6. 表达式 ---

/// 表达式，其运算后会产生一个值。
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Assignment(AssignmentExpression),
    Call(CallExpression),
}

/// 一元运算，例如 `-x`, `!flag`, `*ptr`。
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: Node<Operator>,
    pub operand: Box<Node<Expression>>,
}

/// 二元运算，例如 `a + b`。
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Node<Expression>>,
    pub operator: Node<Operator>,
    pub right: Box<Node<Expression>>,
}

/// 赋值运算，例如 `x = y`。
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub lvalue: Box<Node<Expression>>,
    pub rvalue: Box<Node<Expression>>,
}

/// 函数调用。
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Node<Expression>>,
    pub arguments: Vec<Node<Expression>>,
}