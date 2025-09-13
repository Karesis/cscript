// In src/codegen/test.rs

use super::*;
use crate::analyzer;
use crate::lexer; // [ASSUMPTION] 假设你的 lexer 模块在 crate::lexer
use crate::parser;

// --- Test Harness (无需修改) ---

/// 一个完整的辅助函数，用于简化端到端测试。
///
/// 它接收源代码字符串，完成“词法分析 -> 解析 -> 语义分析 -> 代码生成”的完整流程，
/// 并返回最终的 LLVM IR 字符串。
///
/// 如果过程中任何一步失败，它会 panic 并显示错误，这在测试中是可接受的。
fn codegen_test_harness(source: &str) -> String {
    // 1. 词法分析 (Lexing)
    // [NOTE] 我们假设你的 lexer API 是 lexer::lex(source) -> (Option<Vec<Token>>, Vec<Error>)
    let (tokens, lexer_errors) = lexer::lex(source);
    if !lexer_errors.is_empty() {
        panic!("Lexing produced errors: {:?}", lexer_errors);
    }
    
    // 2. 解析 (Parsing)
    let (ast_program, parser_errors) = parser::parse(source.len(), tokens);
    if !parser_errors.is_empty() {
        panic!("Parsing failed: {:?}", parser_errors);
    }
    let ast_program = ast_program.expect("Parsing succeeded but produced no AST");

    // 3. 语义分析 (Analyzing)
    let analyzer = analyzer::Analyzer::new();
    let hir_program = match analyzer.analyze(&ast_program) {
        Ok(hir) => hir,
        Err(errors) => panic!("Analysis failed: {:?}", errors),
    };

    // 4. 代码生成 (Codegen)
    match codegen(&hir_program) {
        Ok(ir) => ir,
        Err(errors) => panic!("CodeGen failed: {:?}", errors),
    }
}


// --- Test Cases (使用正确的 CScript 语法进行修正) ---

#[test]
fn test_return_integer_literal() {
    // [FIXED] 移除了 'fn' 关键字，使用正确的 CScript 函数语法。
    let source = "test_main() -> int { return 42; }";
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_return_integer_literal ---\n{}\n", llvm_ir);

    assert!(
        llvm_ir.contains("define i32 @test_main()"),
        "Expected function signature 'define i32 @test_main()'"
    );
    assert!(
        llvm_ir.contains("ret i32 42"),
        "Expected 'ret i32 42'"
    );
}

#[test]
fn test_binary_expression() {
    let source = "test_add() -> int { return 10 + 5; }";
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_binary_expression ---\n{}\n", llvm_ir);

    assert!(llvm_ir.contains("define i32 @test_add()"));

    // [FIXED] 断言函数直接返回了计算结果 15，而不是检查 add 指令。
    // 这使得测试对常量折叠等优化是健壮的。
    assert!(
        llvm_ir.contains("ret i32 15"),
        "Expected to return the constant-folded value 15"
    );
}

#[test]
fn test_local_variable() {
    let source = r#"
        test_vars() -> int {
            x: int = 100;
            y: int = x + 20;
            return y;
        }
    "#;
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_local_variable ---\n{}\n", llvm_ir);

    // 1. 检查两个局部变量的栈分配 (`alloca`)
    assert_eq!(llvm_ir.matches("alloca i32").count(), 2, "Expected two stack allocations for local variables");
    // 2. 检查 x 的初始化存储
    assert!(
        llvm_ir.contains("store i32 100, ptr"),
        "Expected to store initial value 100"
    );
    // 3. 检查为了计算 y 而从 x 加载值
    assert!(
        llvm_ir.contains("load i32, ptr"),
        "Expected to load value from a variable"
    );
    
    // 4. [FIXED] 检查 y 的计算。我们只断言存在一个 i32 类型的加法，
    //    并且它的操作数是一个寄存器（%），而不关心 `nsw` 标志。
    assert!(
        llvm_ir.contains("add i32 %"), // <--- 移除了 "nsw"
        "Expected add instruction for `y`'s initialization"
    );
    
    // 5. 检查最终的返回值
    assert!(
        llvm_ir.contains("ret i32 %"),
        "Expected to return the value of `y`"
    );
}

#[test]
fn test_global_variable() {
    // [FIXED] 移除了 'fn' 和 'let' 关键字。
    let source = r#"
        my_global: int = 77;

        test_global() -> int {
            return my_global;
        }
    "#;
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_global_variable ---\n{}\n", llvm_ir);

    // 1. 检查全局变量的定义
    assert!(
        llvm_ir.contains("@my_global = global i32 77"),
        "Expected global variable definition"
    );
    // 2. 检查函数中对全局变量的加载
    assert!(
        llvm_ir.contains("load i32, ptr @my_global"),
        "Expected to load from global variable"
    );
    // 3. 检查返回值
    assert!(
        llvm_ir.contains("ret i32 %"),
        "Expected to return the loaded global value"
    );
}