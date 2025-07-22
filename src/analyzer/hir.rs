// src/analyzer/hir.rs

use crate::parser::ast::{self, UnaryOp, BinaryOp, Ident};
use crate::lexer::Span;
use crate::analyzer::types::SemanticType; 
use std::sync::Arc;
use std::hash::{Hash, Hasher}; 

// =======================================================================
// 核心“注解”信息
// =======================================================================

/// 代表一个变量的存储位置
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Storage {
    Global { name: String },
    Local { offset: i32 }, 
}

// =======================================================================
// HIR 节点定义
// =======================================================================

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: Vec<Arc<VarDecl>>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<Arc<VarDecl>>,
    pub return_type: SemanticType, // [FIXED]
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Block),
    VarDecl(Arc<VarDecl>),
    Expr(Expression),
    If {
        condition: Expression,
        then_branch: Block,
        else_branch: Option<Block>,
        span: Span, // [ADDED]
    },
    While {
        condition: Expression,
        body: Block,
        span: Span, // [ADDED]
    },
    Return { // [UPDATED] Changed to a struct to hold the span
        value: Option<Expression>,
        span: Span,
    },
    Break(Span),      // [UPDATED] Break and Continue can also carry their span
    Continue(Span),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VarDecl {
    pub name: Ident,
    pub var_type: SemanticType, // [FIXED]
    pub is_const: bool,
    pub storage: Storage,
    pub initializer: Option<Expression>,
}
// 手动实现 PartialEq, Eq, Hash
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind // 暂时只比较 kind，可以根据需要扩展
    }
}
impl Eq for Expression {}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state); // 暂时只 hash kind
    }
}


#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
    pub resolved_type: SemanticType, // [FIXED]
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ExprKind {
    Literal(ast::LiteralValue),
    Variable(Arc<VarDecl>), 
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
    AddressOf(Box<Expression>),
    Dereference(Box<Expression>),
}