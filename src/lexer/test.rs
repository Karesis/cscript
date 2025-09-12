// 导入父模块（也就是 lexer 模块）的所有内容
use super::*; 
use miette::SourceSpan;

/// 一个辅助函数，用于简化测试。
/// 它接收源代码，调用 lexer，然后只返回 Token 的向量（忽略它们的 span）。
/// 这样做是因为在大多数测试中，我们更关心 Token 的类型和顺序，而不是它们在代码中的确切位置。
fn lex_just_tokens(source: &str) -> Vec<Token> {
    let (tokens, _errors) = lex(source);
    tokens.into_iter().map(|(token, _span)| token).collect()
}

// --- 成功案例 (Happy Path) ---
// 测试词法分析器能否正确识别各种有效的 Token。

#[test]
fn test_keywords_and_identifiers() {
    let source = "int main_function = 10;";
    let expected_tokens = vec![
        Token::Int,
        Token::Ident("main_function".to_string()),
        Token::Assign,
        Token::Integer("10".to_string()),
        Token::Semicolon,
    ];
    assert_eq!(lex_just_tokens(source), expected_tokens);
}

#[test]
fn test_all_operators() {
    let source = "+ - * / % = == != < > <= >= && || ! & . : -> ...";
    let expected_tokens = vec![
        Token::Plus, Token::Minus, Token::Star, Token::Slash, Token::Percent,
        Token::Assign, Token::Eq, Token::NotEq, Token::Lt, Token::Gt,
        Token::Lte, Token::Gte, Token::And, Token::Or, Token::Not,
        Token::Ampersand, Token::Dot, Token::Colon, Token::Arrow, Token::Ellipsis,
    ];
    assert_eq!(lex_just_tokens(source), expected_tokens);
}

#[test]
fn test_comments_and_whitespace_are_skipped() {
    let source = r#"
        // 这是一个单行注释
        int /* 这是一个
                块注释 */ main;
    "#;
    let expected_tokens = vec![
        Token::Int,
        Token::Ident("main".to_string()),
        Token::Semicolon,
    ];
    assert_eq!(lex_just_tokens(source), expected_tokens);
}

// --- 失败案例 (Sad Path) ---
// 测试词法分析器能否在遇到无效输入时，正确地**产生**我们定义的错误。

#[test]
fn test_unrecognized_character() {
    // 这个源代码里包含一个 logos 无法识别的字符 '#'
    let source = "int a = #;";
    
    // 调用完整的 lex 函数，因为这次我们关心的是 errors
    let (_tokens, errors) = lex(source);

    // 1. 确认我们只收到了一个错误
    assert_eq!(errors.len(), 1, "Expected exactly one error.");

    // 2. 深入检查这个错误的类型和内容
    // 我们使用 `if let` 来安全地解构错误，确保它就是我们期望的那个变体。
    // 这种方式比 `#[derive(PartialEq)]` 更灵活、更健壮。
    if let Some(CompilerError::Lexical(LexerError::UnrecognizedToken { unrecognized_char, span })) = errors.get(0) {
        // 确认错误的具体内容是否正确
        assert_eq!(*unrecognized_char, '#', "The unrecognized character should be '#'.");
        // 确认错误发生的位置是否正确
        assert_eq!(*span, SourceSpan::from(8..9), "The span of the error is incorrect.");
    } else {
        // 如果 `if let` 匹配失败，说明错误类型不对，我们让测试失败并打印出收到的错误。
        panic!("Expected an UnrecognizedToken error, but got a different kind of error: {:?}", errors.get(0));
    }
}

#[test]
fn test_multiple_unrecognized_characters() {
    let source = "let x = @ + $;";
    let (_tokens, errors) = lex(source);

    assert_eq!(errors.len(), 2, "Expected two errors.");

    // 检查第一个错误
    if let Some(CompilerError::Lexical(LexerError::UnrecognizedToken { unrecognized_char, .. })) = errors.get(0) {
        assert_eq!(*unrecognized_char, '@');
    } else {
        panic!("First error was not UnrecognizedToken");
    }

    // 检查第二个错误
    if let Some(CompilerError::Lexical(LexerError::UnrecognizedToken { unrecognized_char, .. })) = errors.get(1) {
        assert_eq!(*unrecognized_char, '$');
    } else {
        panic!("Second error was not UnrecognizedToken");
    }
}