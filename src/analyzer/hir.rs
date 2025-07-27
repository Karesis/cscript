// src/analyzer/hir.rs

use crate::analyzer::types::SemanticType;
use crate::lexer::Span;
use crate::parser::ast::{BinaryOp, Ident, UnaryOp};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    String(String),
    Bool(bool),
    Float(f64), // 它现在正确地存储 f64
}

impl Hash for LiteralValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            LiteralValue::Integer(i) => i.hash(state),
            LiteralValue::String(s) => s.hash(state),
            LiteralValue::Bool(b) => b.hash(state),
            LiteralValue::Float(f) => {
                if *f == 0.0 {
                    0.0f64.to_bits().hash(state);
                } else if f.is_nan() {
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
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Storage {
    Global { name: String },
    Local { offset: i32 },
}

// --- HIR 节点定义 ---
#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: Vec<Arc<VarDecl>>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<Arc<VarDecl>>,
    pub return_type: SemanticType,
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
        span: Span,
    },
    While {
        condition: Expression,
        body: Block,
        span: Span,
    },
    Return {
        value: Option<Expression>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VarDecl {
    pub name: Ident,
    pub var_type: SemanticType,
    pub is_const: bool,
    pub storage: Storage,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub span: Span,
    pub resolved_type: SemanticType,
}

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


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Literal(LiteralValue),
    Variable(Arc<VarDecl>),
    MemberAccess {
        expression: Box<Expression>,
        member: Ident,
    },
    StructLiteral {
        // 存储 struct 的完整类型信息
        struct_type: Arc<SemanticType>,
        // 存储每个字段的名字和已经过完整分析的、类型正确的初始化表达式
        fields: Vec<(Ident, Expression)>,
    },
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
