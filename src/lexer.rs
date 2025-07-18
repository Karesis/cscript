use logos::Logos;
use std::fmt;

pub type Span = std::ops::Range<usize>;


#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\r\n\f]+")] // skip whitespace
#[logos(skip r"//[^\n]*")]     // skip single-line comments
#[logos(skip r"/\*([^*]|\*[^/])*\*/")] // skip multi-line comments
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
    
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool), 

    // Literals
    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    Integer(String),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
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

// Custom error type for the lexer.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct LexingError;

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexing Error: Unrecognized token")
    }
}

// Implement Display for Token to make it easier to print them out during debugging.
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// A helper function to create a lexer instance and generate a stream of tokens.
// This is the primary interface our parser will use.
pub fn lexer(
    source: &str,
) -> impl Iterator<Item = Result<(Token, Span), (LexingError, Span)>> + '_ {
    Token::lexer(source)
        .spanned()
        .map(|(result, span)| match result {
            Ok(token) => Ok((token, span)),
            Err(e) => Err((e, span)),
        })
}