use super::ast::*;
use super::*; // 导入父模块（parser）的所有公共项，主要是 `parse` 函数
use crate::reporter::{CompilerError, ParserError};
use crate::lexer;

// --- 测试辅助函数 ---

/// 辅助函数，用于测试源代码能够被**成功**解析的场景。
///
/// 它会执行完整的词法和语法分析流程。如果在此过程中遇到任何错误，
/// 测试会立即 panic 并打印出详细的错误报告，这能帮助我们快速定位
/// 那些本应成功但意外失败的用例。
/// 如果一切顺利，它会返回最终生成的 AST (`Program`)。
fn parse_source_ok(source: &str) -> Program {
    let (tokens, lex_errors) = lexer::lex(source);
    assert!(
        lex_errors.is_empty(),
        "Lexer failed unexpectedly for source:\n---\n{}\n---",
        source
    );

    let (ast, parse_errors) = parse(source.len(), tokens);
    if !parse_errors.is_empty() {
        // 在 panic 之前，使用 miette 打印出所有格式化好的错误报告，方便调试。
        for error in parse_errors {
            eprintln!("{:?}", miette::Report::new(error));
        }
        panic!(
            "Parser failed unexpectedly for source:\n---\n{}\n---",
            source
        );
    }

    ast.expect("Parser returned no AST despite reporting no errors.")
}

/// 辅助函数，用于测试源代码**不能**被成功解析的场景。
///
/// 它同样会执行完整的词法和语法分析，但它会收集并返回所有
/// 在此过程中发现的错误 (`CompilerError` 的向量)。测试代码可以
/// 随后对这些返回的错误进行详细的断言。
fn parse_source_fail(source: &str) -> Vec<CompilerError> {
    let (tokens, mut errors) = lexer::lex(source);
    let (_ast, parse_errors) = parse(source.len(), tokens);
    errors.extend(parse_errors);
    errors
}

// --- 成功路径测试 (Happy Path) ---

/// 测试最基础的、带返回值的 `main` 函数定义。
#[test]
fn test_simple_function_definition() {
    let source = r#"
        main() -> int {
            return 0;
        }
    "#;
    let ast = parse_source_ok(source);
    assert_eq!(ast.items.len(), 1);
    matches!(&ast.items[0], GlobalItem::Function(func_def) if func_def.name.name == "main");
}

/// 测试顶层的结构体定义，以及在函数内部使用该结构体进行常量声明。
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
    matches!(&ast.items[0], GlobalItem::Struct(s) if s.name.name == "Point");
}

/// 测试 `extern` 块，特别是带有指针类型和变长参数的函数声明。
#[test]
fn test_extern_block() {
    let source = r#"
        extern "C" {
            printf(fmt: char*, ...) -> int;
        }
    "#;
    let ast = parse_source_ok(source);
    assert_eq!(ast.items.len(), 1);
    matches!(&ast.items[0], GlobalItem::Extern(e) if e.abi == "C" && e.declarations[0].is_variadic);
}

/// 新增测试：验证 `if-else` 语句能被正确解析。
#[test]
fn test_if_else_statement() {
    let source = r#"
        main() -> int {
            if (true) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;
    let ast = parse_source_ok(source);
    let func = match &ast.items[0] {
        GlobalItem::Function(f) => f,
        _ => panic!("Expected function"),
    };
    matches!(&func.body.stmts[0], Statement::If { else_branch: Some(_), .. });
}

/// 新增测试：验证 `while` 循环能被正确解析。
#[test]
fn test_while_loop() {
    let source = r#"
        main() -> void {
            while (true) {
                // loop forever
            }
        }
    "#;
    let ast = parse_source_ok(source);
    let func = match &ast.items[0] {
        GlobalItem::Function(f) => f,
        _ => panic!("Expected function"),
    };
    matches!(&func.body.stmts[0], Statement::While { .. });
}

/// 新增测试：验证复杂的二元运算符优先级（乘法优先于加法）。
#[test]
fn test_operator_precedence() {
    let source = r#"
        main() -> void {
            a: int = 1 + 2 * 3; // should be parsed as 1 + (2 * 3)
        }
    "#;
    let ast = parse_source_ok(source);
    let func = match &ast.items[0] { GlobalItem::Function(f) => f, _ => panic!() };
    let stmt = match &func.body.stmts[0] { Statement::Expr(e) => e, _ => panic!() };
    // 断言顶层操作是赋值
    if let ExprKind::Assignment { right, .. } = &stmt.kind {
        // 断言赋值的右侧是一个二元运算
        if let ExprKind::BinaryOp { op, right: mul_expr, ..} = &right.kind {
            // 断言这个二元运算是加法
            assert_eq!(*op, BinaryOp::Add);
            // 断言加法的右侧是另一个二元运算（乘法）
            matches!(&mul_expr.kind, ExprKind::BinaryOp { op: BinaryOp::Multiply, .. });
        } else {
            panic!("Expected a binary operation on the right side of assignment.");
        }
    } else {
        panic!("Expected an assignment expression.");
    }
}

// --- 失败路径测试 (Sad Path) ---

/// 测试 `return` 语句后缺少分号的情况。
#[test]
fn test_missing_semicolon() {
    let source = r#"
        main() -> int {
            return 0
        }
    "#;
    let errors = parse_source_fail(source);
    assert_eq!(errors.len(), 1);
    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        // chumsky 会报告期望一个 "Semicolon" (Debug 名)，因为 "return 0" 是一个合法的表达式开头，
        // 后面可以跟分号（表达式语句），也可以跟其他运算符。
        assert!(expected.contains("Semicolon"), "Expected error to mention 'Semicolon'.");
        assert!(found.contains("RBrace"), "Found token should be 'RBrace'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken");
    }
}

/// 测试在需要表达式的地方提供了一个关键字。
#[test]
fn test_unexpected_token_instead_of_expression() {
    let source = r#"
        main() -> int {
            return if;
        }
    "#;
    let errors = parse_source_fail(source);
    assert_eq!(errors.len(), 1);
    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        assert!(expected.contains("expression"), "Expected an expression.");
        assert!(found.contains("If"), "Found token should be 'If'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken");
    }
}

/// 测试结构体字段定义不完整（缺少类型）。
#[test]
fn test_incomplete_struct_definition() {
    let source = r#"
        struct Point {
            x:
        }
    "#;
    let errors = parse_source_fail(source);
    assert_eq!(errors.len(), 1);
    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        assert!(expected.contains("type"), "Expected a type.");
        assert!(found.contains("RBrace"), "Found token should be 'RBrace'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken");
    }
}

/// 新增测试：验证括号不匹配的错误。
#[test]
fn test_mismatched_parenthesis() {
    let source = r#"
        main() -> int {
            return (0;
        }
    "#;
    let errors = parse_source_fail(source);
    assert_eq!(errors.len(), 1);
    if let Some(CompilerError::Parsing(ParserError::UnexpectedToken { expected, found, .. })) = errors.get(0) {
        assert!(expected.contains("RParen"), "Expected a closing parenthesis 'RParen'.");
        assert!(found.contains("Semicolon"), "Found token should be 'Semicolon'.");
    } else {
        panic!("Expected a ParserError::UnexpectedToken");
    }
}

// 测试顶层变量/全局变量
#[test]
fn test_top_level_variable_declaration() {
    let source = "g_my_var: int = 2 + 3;";
    let ast = parse_source_ok(source);
    assert_eq!(ast.items.len(), 1);
    matches!(&ast.items[0], GlobalItem::VarDecl(decl) if decl.name.name == "g_my_var");
}