// 主词法分析器模块。
// 包含 Lexer 结构体和负责将源代码转换为 Token 流的所有逻辑。

use super::token::*;
use crate::diagnostic::*;
use std::iter::Peekable;
use std::str::Chars;

// --- 1. Lexer 定义 ---

/// 词法分析器，负责将源代码转换为 Token 流。
/// The lexer, responsible for turning source code into a stream of tokens.
pub struct Lexer<'a> {
    /// 源代码的引用。
    /// A reference to the source code string.
    source: &'a str,
    /// 诊断信息收集器的可变引用，用于报告错误。
    /// A mutable reference to the diagnostic bag for error reporting.
    diagnostics: &'a mut DiagnosticBag,
    /// 可窥视（peekable）的字符迭代器，是词法分析的核心。
    /// A peekable character iterator, core to the lexer's functionality.
    chars: Peekable<Chars<'a>>,
    /// 当前正在扫描的 Token 的起始位置（字节索引）。
    /// The starting byte position of the token currently being scanned.
    start_pos: usize,
    /// 迭代器在源代码中的当前位置（以字节为单位）。
    /// The current byte position of the iterator in the source code.
    current_pos: usize,
}

impl<'a> Lexer<'a> {
    /// 创建一个新的 Lexer 实例。
    /// Creates a new Lexer instance.
    pub fn new(source: &'a str, diagnostics: &'a mut DiagnosticBag) -> Self {
        Lexer {
            source,
            diagnostics,
            chars: source.chars().peekable(),
            start_pos: 0,
            current_pos: 0,
        }
    }

    /// 消费 Lexer 并扫描所有 Token 直到文件末尾。
    /// Consumes the Lexer and scans all tokens until the end of the file.
    pub fn scan_all_tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.scan_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }
}

// --- 2. Core Character Logic ---
// --- 2. 底层字符操作逻辑 ---

/// 定义了 Lexer 的核心功能：底层字符操作和位置追踪。
/// Defines the core functionality of the Lexer: low-level character operations and position tracking.
trait LexerCore {
    /// 返回当前字符，但不消费它。
    /// Returns the current character without consuming it.
    fn current_char(&mut self) -> char;
    /// 返回下一个字符（前瞻），但不消费它。
    /// Peeks at the next character without consuming it.
    fn peek_char(&mut self) -> char;
    /// 消费当前字符并前进一个位置。
    /// Consumes the current character and advances the position.
    fn advance(&mut self) -> char;
    /// 检查是否已到达源代码的末尾。
    /// Checks if the end of the source has been reached.
    fn is_at_end(&mut self) -> bool;
    /// 使用给定的类型和当前的扫描范围创建一个 Token。
    /// Creates a token with the given kind and the current span.
    fn make_token(&mut self, kind: TokenKind) -> Token;
    /// 检查当前字符是否与预期字符匹配。如果匹配，则消费该字符并返回 `true`。
    /// Checks if the current character matches an expected one. If so, consumes it and returns `true`.
    fn match_char(&mut self, expected: char) -> bool;
}

impl<'a> LexerCore for Lexer<'a> {
    fn current_char(&mut self) -> char {
        self.chars.peek().cloned().unwrap_or('\0')
    }

    fn peek_char(&mut self) -> char {
        let mut it = self.chars.clone();
        it.next(); // Skip current char
        it.next().unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        let c = self.chars.next().unwrap_or('\0');
        if c != '\0' {
            self.current_pos += c.len_utf8();
        }
        c
    }

    fn is_at_end(&mut self) -> bool {
        self.current_char() == '\0'
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        let span = Span::new(self.start_pos, self.current_pos);
        Token { kind, span }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.current_char() != expected {
            false
        } else {
            self.advance();
            true
        }
    }
}

// --- 3. Main Token Scanning Logic ---
// --- 3. 主要词法识别逻辑 ---

/// 定义了高级的 Token 识别逻辑。
/// Defines the high-level token recognition logic.
trait TokenScanner {
    /// 扫描并返回下一个 Token。
    /// Scans and returns the next single token.
    fn scan_token(&mut self) -> Token;

    // --- 各类 Token 的扫描辅助函数 ---
    // --- Helper methods for scanning different token types ---
    
    /// 跳过所有空白字符和注释。
    /// Skips all whitespace and comments.
    fn skip_whitespace_and_comments(&mut self);
    /// 扫描标识符或关键字。
    /// Scans an identifier or a keyword.
    fn scan_identifier(&mut self) -> TokenKind;
    /// 扫描数字字面量（支持十进制、八进制、十六进制）。
    /// Scans a number literal (supports decimal, octal, hexadecimal).
    fn scan_number(&mut self) -> TokenKind;
    /// 扫描字符串字面量。
    /// Scans a string literal.
    fn scan_string(&mut self) -> TokenKind;
    /// 扫描字符字面量。
    /// Scans a character literal.
    fn scan_char_literal(&mut self) -> TokenKind;
    /// 扫描转义序列。
    /// Scans an escape sequence.
    fn scan_escape_sequence(&mut self, in_string: bool) -> char;
}

impl<'a> TokenScanner for Lexer<'a> {
    /// 主分发函数，根据当前字符决定调用哪个具体的扫描函数。
    /// The main dispatch function that decides which specific scanning function to call based on the current character.
    fn scan_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        self.start_pos = self.current_pos;

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        let c = self.advance();
        
        let kind = match c {
            // 单字符标点 (Single-character punctuation)
            '(' => TokenKind::Punctuation(Punctuation::LParen),
            ')' => TokenKind::Punctuation(Punctuation::RParen),
            '{' => TokenKind::Punctuation(Punctuation::LBrace),
            '}' => TokenKind::Punctuation(Punctuation::RBrace),
            '[' => TokenKind::Punctuation(Punctuation::LBracket),
            ']' => TokenKind::Punctuation(Punctuation::RBracket),
            ',' => TokenKind::Punctuation(Punctuation::Comma),
            ';' => TokenKind::Punctuation(Punctuation::Semicolon),
            
            // 单字符或双字符操作符 (Single or double-character operators)
            '+' => TokenKind::Operator(Operator::Plus),
            '*' => TokenKind::Operator(Operator::Star),
            '/' => TokenKind::Operator(Operator::Slash),
            '%' => TokenKind::Operator(Operator::Percent),
            '-' => TokenKind::Operator(Operator::Minus),
            '=' => if self.match_char('=') { TokenKind::Operator(Operator::Eq) } else { TokenKind::Operator(Operator::Assign) },
            '!' => if self.match_char('=') { TokenKind::Operator(Operator::NotEq) } else { TokenKind::Operator(Operator::Not) },
            '<' => if self.match_char('=') { TokenKind::Operator(Operator::LtEq) } else { TokenKind::Operator(Operator::Lt) },
            '>' => if self.match_char('=') { TokenKind::Operator(Operator::GtEq) } else { TokenKind::Operator(Operator::Gt) },
            '&' => if self.match_char('&') { TokenKind::Operator(Operator::AndAnd) } else { TokenKind::Operator(Operator::And) },
            '|' => if self.match_char('|') { TokenKind::Operator(Operator::OrOr) } else { 
                let span = Span::new(self.start_pos, self.current_pos);
                let label = Label::new(span, "Bitwise-OR `|` is not supported in c0");
                self.diagnostics.report_error_with_code("Unsupported operator", ErrorCode::E0002, label);
                return self.scan_token(); // 报告错误后，递归扫描下一个有效的 Token
            },

            // 字面量和标识符 (Literals and identifiers)
            '\'' => self.scan_char_literal(),
            '"' => self.scan_string(),
            c if c.is_ascii_digit() => self.scan_number(),
            c if c.is_alphabetic() || c == '_' => self.scan_identifier(),
            
            // 未知字符 (Unknown character)
            _ => {
                let span = Span::new(self.start_pos, self.current_pos);
                let label = Label::new(span, "this character is not recognized");
                self.diagnostics.report_error_with_code(format!("Unknown character '{}'", c), ErrorCode::E0001, label);
                return self.scan_token();
            }
        };

        self.make_token(kind)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.current_char() {
                // 标准 C 空白符 (Standard C whitespace)
                ' ' | '\r' | '\t' | '\n' | '\x0B' | '\x0C' => { self.advance(); },
                '/' => {
                    // 单行注释 (Single-line comment)
                    if self.peek_char() == '/' {
                        self.advance(); // consume '/'
                        while self.current_char() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    // 多行注释 (Multi-line comment)
                    } else if self.peek_char() == '*' {
                        let comment_start = self.current_pos;
                        self.advance(); // consume '/'
                        self.advance(); // consume '*'
                        
                        while !(self.current_char() == '*' && self.peek_char() == '/') {
                            if self.is_at_end() {
                                let span = Span::new(comment_start, self.current_pos);
                                let label = Label::new(span, "this comment was never closed");
                                self.diagnostics.report_error_with_code("Unclosed multi-line comment", ErrorCode::E0003, label);
                                break;
                            }
                            self.advance();
                        }
                        
                        if !self.is_at_end() {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'
                        }
                    } else {
                        break; // 是一个除法运算符，不是注释 (It's a division operator, not a comment)
                    }
                },
                _ => break,
            }
        }
    }

    fn scan_identifier(&mut self) -> TokenKind {
        while self.current_char().is_alphanumeric() || self.current_char() == '_' {
            self.advance();
        }
        let text = &self.source[self.start_pos..self.current_pos];
        match text {
            // 先检查是否为布尔字面量
            "true" => TokenKind::Literal(Literal::Boolean(true)),
            "false" => TokenKind::Literal(Literal::Boolean(false)),
            // 如果不是，再检查是否是关键字
            _ => Keyword::lookup(text)
                .map(TokenKind::Keyword)
                .unwrap_or_else(|| TokenKind::Identifier(text.to_string())), // 最后才认为是普通标识符
        }
    }
    
    fn scan_number(&mut self) -> TokenKind {
        let text_slice = &self.source[self.start_pos..];
        let is_hex = text_slice.starts_with("0x") || text_slice.starts_with("0X");

        if is_hex {
            self.advance(); // 0
            self.advance(); // x
            let hex_start = self.current_pos;
            while self.current_char().is_ascii_hexdigit() {
                self.advance();
            }
            if self.current_pos == hex_start {
                 let span = Span::new(self.start_pos, self.current_pos);
                 let label = Label::new(span, "expected at least one hex digit after `0x`");
                 self.diagnostics.report_error_with_code("Invalid hex literal", ErrorCode::E0015, label);
            }
        } else {
            while self.current_char().is_ascii_digit() {
                self.advance();
            }
        }

        let text = &self.source[self.start_pos..self.current_pos];
        let radix = if is_hex { 16 } else if text.starts_with('0') && text.len() > 1 { 8 } else { 10 };
        
        let value_str = if radix == 16 { &text[2..] } else { text };
        
        match i64::from_str_radix(value_str, radix) {
            Ok(val) => TokenKind::Literal(Literal::Integer(val)),
            Err(_) => {
                let span = Span::new(self.start_pos, self.current_pos);
                let label = Label::new(span, "this number is too large to fit in a 64-bit integer");
                self.diagnostics.report_error_with_code("Invalid integer literal", ErrorCode::E0016, label);
                TokenKind::Literal(Literal::Integer(0)) // 错误时返回一个虚拟值 (Return a dummy value on error)
            }
        }
    }

    fn scan_string(&mut self) -> TokenKind {
        let mut value = String::new();
        while self.current_char() != '"' && !self.is_at_end() {
            let ch = if self.current_char() == '\\' { self.scan_escape_sequence(true) } else { self.advance() };
            value.push(ch);
        }

        if self.is_at_end() {
            let span = Span::new(self.start_pos, self.current_pos);
            let label = Label::new(span, "string starts here but is never closed");
            self.diagnostics.report_error_with_code("Unclosed string literal", ErrorCode::E0004, label);
        } else {
            self.advance(); // 消费结束的双引号 (consume the closing `"`)
        }
        
        TokenKind::Literal(Literal::String(value))
    }
    
    fn scan_char_literal(&mut self) -> TokenKind {
        let value = if self.current_char() == '\\' { self.scan_escape_sequence(false) } else { self.advance() };

        if !self.match_char('\'') {
             let span = Span::new(self.start_pos, self.current_pos);
             let label = Label::new(span, "expected a closing single quote `'`");
             self.diagnostics.report_error_with_code("Unclosed or multi-character literal", ErrorCode::E0005, label);
             // 尝试恢复：跳过直到找到下一个单引号或空白符
             // Attempt to recover: skip until the next quote or whitespace
             while !self.is_at_end() && self.current_char() != '\'' && !self.current_char().is_whitespace() { self.advance(); }
             if self.current_char() == '\'' { self.advance(); }
        }
        
        TokenKind::Literal(Literal::Char(value))
    }

    fn scan_escape_sequence(&mut self, in_string: bool) -> char {
        self.advance(); // consume `\`
        let c = self.advance();
        match c {
            'n' => '\n', 'r' => '\r', 't' => '\t', '\\' => '\\', '0' => '\0',
            '\'' if !in_string => '\'',
            '"' if in_string => '"',
            // C 风格十六进制转义 (C-style hex escape \xHH)
            'x' => {
                let mut hex_code = String::new();
                for _ in 0..2 {
                    if self.current_char().is_ascii_hexdigit() {
                        hex_code.push(self.advance());
                    } else { break; }
                }

                if hex_code.is_empty() {
                    let span = Span::new(self.current_pos - 2, self.current_pos);
                    let label = Label::new(span, "expected at least one hex digit after `\\x`");
                    self.diagnostics.report_error_with_code("Incomplete hex escape sequence", ErrorCode::E0007, label);
                    // 返回一个替换字符以表示错误
                    // Return a replacement character to signify an error
                    std::char::REPLACEMENT_CHARACTER 
                } else {
                    let value = u8::from_str_radix(&hex_code, 16).unwrap();
                    value as char
                }
            }
            _ => {
                let span = Span::new(self.current_pos - 2, self.current_pos);
                let label = Label::new(span, format!("`\\{}` is not a valid escape sequence", c));
                self.diagnostics.report_error_with_code("Unknown escape sequence", ErrorCode::E0006, label);
                // 在 C 中，未知转义通常会被忽略，这里直接返回该字符
                // In C, unknown escapes are often ignored; here we just return the character itself.
                c 
            }
        }
    }
}