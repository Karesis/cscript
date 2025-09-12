use super::ast::{GlobalItem, Program, Statement, Type, VarDecl};
use super::*; // 导入父模块（parser）的所有公共项，主要是 `parse` 函数
use crate::reporter::{CompilerError, ParserError};
use crate::lexer;

/// 辅助函数，用于测试成功解析的场景。
/// 它会执行词法和语法分析，如果出现任何错误就会 panic，否则返回 AST。
/// 这使得成功路径的测试代码非常简洁。
fn parse_source_ok(source: &str) -> Program {
    let (tokens, lex_errors) = lexer::lex(source);
    assert!(lex_errors.is_empty(), "Lexer failed unexpectedly for source: {}", source);

    let (ast, parse_errors) = parse(source.len(), tokens);
    assert!(parse_errors.is_empty(), "Parser failed unexpectedly for source: {}", source);

    ast.expect("Parser returned no AST despite reporting no errors.")
}

/// 辅助函数，用于测试解析失败的场景。
/// 它执行完整的词法和语法分析，并返回所有发现的错误。
fn parse_source_fail(source: &str) -> Vec<CompilerError> {
    let (tokens, mut errors) = lexer::lex(source);
    let (_ast, parse_errors) = parse(source.len(), tokens);
    errors.extend(parse_errors);
    errors
}

// --- 成功路径测试 (Happy Path) ---

#[test]
fn test_simple_function_definition() {
    let source = r#"
        main() -> int {
            return 0;
        }
    "#;
    let ast = parse_source_ok(source);

    // 断言 AST 的基本结构是正确的
    assert_eq!(ast.items.len(), 1, "Expected one global item.");
    
    // 使用 matches! 宏来简洁地检查 enum 变体和内部数据
    matches!(
        &ast.items[0],
        GlobalItem::Function(func_def) if
            func_def.name.name == "main" &&
            func_def.params.is_empty() &&
            func_def.return_type == Type::I32 &&
            func_def.body.stmts.len() == 1
    );
}

#[test]
fn test_variable_declaration_and_struct() {
    let source = r#"
        struct Point {
            x: int,
            y: int,
        }

        main() -> void {
            const p: Point = { 1, 2 };
        }
    "#;
    let ast = parse_source_ok(source);

    assert_eq!(ast.items.len(), 2);
    
    // 检查结构体定义
    matches!(&ast.items[0], GlobalItem::Struct(s) if s.name.name == "Point" && s.fields.len() == 2);
    
    // 检查函数定义和内部的变量声明
    if let GlobalItem::Function(func) = &ast.items[1] {
        assert_eq!(func.body.stmts.len(), 1);
        matches!(&func.body.stmts[0], Statement::VarDecl(VarDecl { is_const: true, .. }));
    } else {
        panic!("Second item was not a function definition.");
    }
}

#[test]
fn test_extern_block() {
    let source = r#"
        extern "C" {
            printf(fmt: *char, ...) -> int;
        }
    "#;
    let ast = parse_source_ok(source);
    assert_eq!(ast.items.len(), 1);
    matches!(&ast.items[0], GlobalItem::Extern(e) if e.abi == "C" && e.declarations[0].is_variadic);
}

// --- 失败路径测试 (Sad Path) ---

#[test]
fn test_missing_semicolon() {
    let source = r#"
        main() -> int {
            return 0 // <-- 缺少分号
        }
    "#;
    let errors = parse_source_fail(source);

    assert_eq!(errors.len(), 1, "Expected exactly one parsing error.");

    // 深入检查错误的类型和内容
    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        // 我们期望解析器在这里想要一个分号
        assert!(expected.contains("';'"), "Expected error to mention ';'.");
        // 实际找到的是函数体的右大括号
        assert!(found.contains("'}'"), "Found token should be '}}'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken, but got: {:?}", errors.get(0));
    }
}

#[test]
fn test_unexpected_token_instead_of_expression() {
    let source = "main() -> int { return if; }"; // 'if' 不能作为返回值
    let errors = parse_source_fail(source);

    assert_eq!(errors.len(), 1);

    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        // 解析器期望一个表达式，但找到了 'if' 关键字
        assert!(expected.contains("expression"), "Expected an expression.");
        assert!(found.contains("'if'"), "Found token should be 'if'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken, but got: {:?}", errors.get(0));
    }
}

#[test]
fn test_incomplete_struct_definition() {
    let source = "struct Point { x: }"; // 类型缺失
    let errors = parse_source_fail(source);

    assert_eq!(errors.len(), 1);

    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        // 在 ':' 之后，解析器期望一个类型
        assert!(expected.contains("type"), "Expected a type.");
        // 实际找到的是 '}'
        assert!(found.contains("'}'"), "Found token should be '}}'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken, but got: {:?}", errors.get(0));
    }
}