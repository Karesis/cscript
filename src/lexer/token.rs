use crate::diagnostic::Span;
use std::fmt::{Display, Formatter, Result};

/// 主体 Token 定义，包含其种类和在源代码中的位置。
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// CScript 语言中所有可能的词法单元。
#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    /// 文件结束标志。
    Eof,

    /// 标识符。
    Identifier(String),

    /// 字面量。
    Literal(Literal),

    /// 关键字。
    Keyword(Keyword),

    /// 操作符。
    Operator(Operator),

    /// 分隔符与标点。
    Punctuation(Punctuation),
}

impl TokenKind {
    /// 一个用于错误报告的简单字符串表示。
    pub fn to_string_for_error(&self) -> String {
        match self {
            TokenKind::Eof => "end of file".to_string(),
            TokenKind::Identifier(_) => "an identifier".to_string(),
            TokenKind::Literal(l) => match l {
                Literal::Integer(_) => "an integer literal".to_string(),
                Literal::Char(_) => "a character literal".to_string(),
                Literal::String(_) => "a string literal".to_string(),
                Literal::Boolean(_) => "a boolean literal".to_string(),
            },
            TokenKind::Keyword(k) => format!("keyword `{}`", k),
            TokenKind::Operator(o) => format!("operator `{}`", o),
            TokenKind::Punctuation(p) => format!("punctuation `{}`", p),
        }
    }
}

/// CScript `c0` 语言中的关键字。
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    // 内置类型
    Int,
    Char,
    Void,
    Bool,

    // 控制流
    If,
    Else,
    While,
    Return,
}

impl Keyword {
    /// 尝试将一个字符串切片转换为一个关键字。
    pub fn lookup(s: &str) -> Option<Keyword> {
        match s {
            "int" => Some(Keyword::Int),
            "char" => Some(Keyword::Char),
            "void" => Some(Keyword::Void),
            "bool" => Some(Keyword::Bool),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "return" => Some(Keyword::Return),
            _ => None,
        }
    }
}
impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Keyword::Int => "int",
            Keyword::Char => "char",
            Keyword::Void => "void",
            Keyword::Bool => "bool",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::Return => "return",
        };
        write!(f, "{}", s)
    }
}

/// 字面量。
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    /// 整型字面量，直接存储为 i64。
    Integer(i64),

    /// 字符型字面量。
    Char(char),

    /// 字符串字面量。
    String(String),

    /// 布尔字面量
    Boolean(bool),
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

/// 运算符。
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    // Arithmetic
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Logical
    Not,    // !
    AndAnd, // &&
    OrOr,   // ||

    // Comparison
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    Gt,    // >
    LtEq,  // <=
    GtEq,  // >=

    // Assignment & Address
    Assign, // =
    And,    // & (Address-of)
}
impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Star => "*",
            Operator::Slash => "/",
            Operator::Percent => "%",
            Operator::Not => "!",
            Operator::AndAnd => "&&",
            Operator::OrOr => "||",
            Operator::Eq => "==",
            Operator::NotEq => "!=",
            Operator::Lt => "<",
            Operator::Gt => ">",
            Operator::LtEq => "<=",
            Operator::GtEq => ">=",
            Operator::Assign => "=",
            Operator::And => "&",
        };
        write!(f, "{}", s)
    }
}

/// 分隔符与标点。
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuation {
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [ (Kept for future array support)
    RBracket, // ] (Kept for future array support)
    Comma,    // ,
    Semicolon, // ;
}
impl Display for Punctuation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let s = match self {
            Punctuation::LParen => "(",
            Punctuation::RParen => ")",
            Punctuation::LBrace => "{",
            Punctuation::RBrace => "}",
            Punctuation::LBracket => "[",
            Punctuation::RBracket => "]",
            Punctuation::Comma => ",",
            Punctuation::Semicolon => ";",
        };
        write!(f, "{}", s)
    }
}