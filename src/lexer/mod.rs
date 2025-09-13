// 导入logos分词库
use logos::Logos;
// 导入错误处理模组
use crate::reporter::{CompilerError, LexerError};
// 导入定位处理
use crate::utils::Span;
use std::fmt;

// 声明单元测试模块
#[cfg(test)]
mod test;

// logos 解析时需要使用的错误类型
#[derive(Debug, Default, Clone, PartialEq)]
pub enum LexingError {
    /// 使用 `#[default]` 来指定当 logos 需要创建一个默认错误实例时
    /// 应该使用哪个变体。
    #[default]
    InvalidToken,
}

/// 词素定义
#[derive(Logos, Debug, PartialEq, Clone)]
// 当 logos 遇到无法识别的字符时，它会报告一个"LexingError"。
#[logos(error = LexingError)]
// 跳过空白
#[logos(skip r"[ \t\r\n\f]+")] 
// 跳过单行注释
#[logos(skip r"//[^\n]*")]      
// 跳过块注释
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    
    // 关键字 
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
    #[token("struct")]
    Struct,
    #[token("extern")]
    Extern,

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

    // 布尔字面量
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool), 

    // 浮点数字面量
    // logos 会优先尝试匹配这个更具体的规则，然后再尝试匹配整数。
    // 从而和下面的整数匹配区分
    #[regex("[0-9]+\\.[0-9]+", |lex| lex.slice().to_string())]
    Float(String),
        
    // 整数字面量
    // 和上面一样，先解析为str
    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    Integer(String),

    // 字符串字面量
    // 使用辅助函数来处理字符串字面量。
    #[regex(r#""([^"\\]|\\.)*""#, lex_string_literal)]
    String(String),

    // 标识符
    // 函数名，变量名等
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // 运算符号
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

    // 连接符号
    #[token(".")]
    Dot,
    #[token(":")]      
    Colon,
    #[token("->")]     
    Arrow,
    #[token("...")]
    Ellipsis,
    
    // 分割符号
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

/// 字符串字面量的辅助解析函数
// 这个函数处理字符串字面量，去除首尾引号并处理转义字符。
fn lex_string_literal(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    // 去掉首尾的引号 "..."
    let inner = &slice[1..slice.len() - 1];
    
    // 预先分配空间，从而优化性能
    let mut s = String::with_capacity(inner.len()); // 区别于常用的String::new()
    // 创建迭代器，方便迭代解析
    let mut chars = inner.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // 处理转义字符
            match chars.next() {

                // 识别换行
                Some('n') => s.push('\n'),
                // 识别制表符
                Some('t') => s.push('\t'),
                // 识别回车符号
                Some('r') => s.push('\r'),
                // 识别反斜杠
                Some('\\') => s.push('\\'),
                // 识别双引号
                Some('"') => s.push('"'),
                // 如果遇到无法识别的转义序列，选择按原样保留
                // 更复杂的错误处理将在在语义分析阶段进行
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

/// 对源代码进行词法分析，返回一个 Token 向量，并将所有词法错误报告给 DiagnosticBag。
/// 对源代码进行词法分析。
pub fn lex(source: &str) -> (Vec<(Token, Span)>, Vec<CompilerError>) {
    let mut tokens = Vec::new();
    let mut errors: Vec<CompilerError> = Vec::new();

    // lexer.spanned() 现在会产生 (Result<Token, LexingError>, Range<usize>)
    let lexer = Token::lexer(source).spanned();

    for (result, span) in lexer {
        match result {
            // 正常的 Token
            Ok(token) => tokens.push((token, span.into())),

            // logos 遇到了一个错误
            Err(_lexing_error) => {
                // 尽管得到了一个 LexingError，但它本身不包含无效字符的信息。
                // 需要从 `source` 和 `span` 中提取这些信息。
                let slice = &source[span.clone()];
                let unrecognized_char = slice.chars().next().unwrap_or_default();
                
                let error = LexerError::UnrecognizedToken {
                    unrecognized_char,
                    span: span.into(),
                };
                errors.push(error.into());
            }
        }
    }

    (tokens, errors)
}