use crate::lexer::Span;
use std::hash::{Hasher, Hash};

//================================================================//
// 1. Top-Level Items (The Program)
//================================================================//

/// The root of the AST, representing a whole source file.
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<GlobalItem>,
    pub span: Span,
}

/// A top-level item in a program, e.g., a function or global variable.
#[derive(Debug, Clone)]
pub enum GlobalItem {
    Function(FunctionDef),
    VarDecl(VarDecl), // For global variables
}

/// A complete function definition.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub return_type: Type,
    pub name: Ident,
    pub params: Vec<(Type, Ident)>,
    pub body: Block,
    pub span: Span,
}

/// A block of statements, enclosed in `{ ... }`.
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

//================================================================//
// 2. Statements
//================================================================//

/// Any valid statement in the language.
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

/// A variable declaration. Can be a statement or part of a for-loop.
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub is_const: bool,
    pub var_type: Type,
    pub name: Ident,
    pub init: Option<Expression>,
    pub span: Span,
}


//================================================================//
// 3. Expressions
//================================================================//

/// Any valid expression that evaluates to a value.
#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(LiteralValue),
    Variable(Ident),
    UnaryOp {
        op: UnaryOp,
        right: Box<Expression>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment {
        left: Box<Expression>, 
        right: Box<Expression>,
    },
    FunctionCall {
        name: Ident,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp { 
    Not, 
    Negate, 
    AddressOf, 
    Dereference 
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp { 
    Add, 
    Subtract, 
    Multiply, 
    Divide, 
    Modulo, 
    Eq, 
    NotEq, 
    Lt, 
    Gt, 
    Lte, 
    Gte, 
    And, 
    Or 
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)] 
pub enum LiteralValue {
    Integer(String),
    Float(String),
    String(String),
    Bool(bool),
}

//================================================================//
// 4. Types and Identifiers
//================================================================//

/// A type annotation in the source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // 传统类型
    Int, // 保留为 i32 的别名
    Char,
    Bool,
    Void,

    // [NEW] 整数类型
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,

    // [NEW] 浮点数类型
    F32,
    F64,

    // 指针类型
    Ptr(Box<Type>),
}

/// An identifier, like a variable or function name.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}