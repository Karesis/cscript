use logos::Logos;
use std::fmt;
use crate::diagnostics::{Diagnostic, DiagnosticBag, Label};
use crate::diagnostics::codes::E0000_UNRECOGNIZED_CHAR;

pub type Span = std::ops::Range<usize>;

// 字符串字面量的辅助解析函数
/// 这个函数处理字符串字面量，去除首尾引号并处理转义字符。
fn lex_string_literal(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    // 去掉首尾的引号 "..."
    let inner = &slice[1..slice.len() - 1];
    
    let mut s = String::with_capacity(inner.len());
    let mut chars = inner.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // 处理转义字符
            match chars.next() {
                Some('n') => s.push('\n'),
                Some('t') => s.push('\t'),
                Some('r') => s.push('\r'),
                Some('\\') => s.push('\\'),
                Some('"') => s.push('"'),
                // 如果遇到无法识别的转义序列，可以选择报告错误或按原样保留
                // 这里我们选择按原样保留，更复杂的错误处理可以在语义分析阶段进行
                Some(other) => {
                    s.push('\\');
                    s.push(other);
                }
                // 字符串以'\'结尾，这是一个格式错误，但我们这里暂时忽略
                None => return None, 
            }
        } else {
            s.push(c);
        }
    }
    Some(s)
}

#[derive(Logos, Debug, PartialEq, Clone)]
// 当 logos 遇到无法识别的字符时，它会直接返回那个字符作为错误。
#[logos(error = char)]
#[logos(skip r"[ \t\r\n\f]+")] // 跳过空白
#[logos(skip r"//[^\n]*")]      // 跳过单行注释
#[logos(skip r"/\*([^*]|\*[^/])*\*/")] // 跳过块注释
pub enum Token {
    // Keywords
    #[token("int")]
    Int,
    #[token("char")]
    Char,
    #[token("void")]
    Void,
    #[token("bool")]
    Bool,
    #[token("const")]
    Const,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,

    // 有符号整数家族 (i-family)
    #[token("i8")]
    I8,
    #[token("i16")]
    I16,
    #[token("i32")]
    I32,
    #[token("i64")]
    I64,

    // 无符号整数家族 (u-family)
    #[token("u8")]
    U8,
    #[token("u16")]
    U16,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,

    // 浮点数家族 (f-family)
    #[token("f32")]
    F32,
    #[token("f64")]
    F64,

    // Literals
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool), 

    // [MODIFIED] 新增了对浮点数字面量的解析。
    // logos 会优先尝试匹配这个更具体的规则，然后再尝试匹配整数。
    #[regex("[0-9]+\\.[0-9]+", |lex| lex.slice().to_string())]
    Float(String),
        
    // [REFACTORED] 直接在词法分析阶段将数字字符串解析为 i64。
    // 如果解析失败（例如，数字太大溢出），返回 None，logos 会将其视为一个错误。
    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    Integer(String),

    // [REFACTORED] 使用辅助函数来处理字符串字面量。
    #[regex(r#""([^"\\]|\\.)*""#, lex_string_literal)]
    String(String),

    // Identifier
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Assign,
    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("&")]
    Ampersand,
    #[token(".")]
    Dot,
    #[token(":")]      
    Colon,
    #[token("->")]     
    Arrow,
    
    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
}

// Implement Display for Token to make it easier to print them out during debugging.
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// 对源代码进行词法分析，返回一个 Token 向量，并将所有词法错误报告给 DiagnosticBag。
pub fn lex(source: &str, diagnostics: &mut DiagnosticBag) -> Vec<(Token, Span)> {
    let mut tokens = Vec::new();
    
    // 使用 logos 生成的词法分析器，并获取每个 token 的位置 (span)
    let lexer = Token::lexer(source).spanned();

    for (result, span) in lexer {
        match result {
            // 如果成功解析出一个 Token
            Ok(token) => tokens.push((token, span)),
            
            // 如果遇到错误 (一个无法识别的字符)
            Err(unrecognized_char) => {
                // [MODIFIED] 使用新的、基于错误码的规范化方式来创建诊断信息。
                // 1. 我们直接引用 E0000_UNRECOGNIZED_CHAR 这个集中的错误定义。
                // 2. 默认的错误消息 ("Unrecognized character") 和错误码 ("E0000") 都来自 ErrorCode。
                // 3. 我们只提供一个带有具体上下文的 Label。
                let diagnostic = Diagnostic::error(
                    &E0000_UNRECOGNIZED_CHAR,
                    Label::new(span, format!("This character '{}' is not valid in CScript", unrecognized_char)),
                );
                
                // 向“诊断背包”报告这个错误
                diagnostics.report(diagnostic);
            }
        }
    }
    
    tokens
}
