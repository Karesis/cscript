// In src/codegen/test.rs

use super::*; // 导入父模块 (codegen) 的所有内容
use crate::analyzer;
use crate::parser;
use crate::lexer; // [ASSUMPTION] 假设你的 lexer 模块在 crate::lexer

// --- Test Harness ---

/// 一个完整的辅助函数，用于简化端到端测试。
///
/// 它接收源代码字符串，完成“词法分析 -> 解析 -> 语义分析 -> 代码生成”的完整流程，
/// 并返回最终的 LLVM IR 字符串。
///
/// 如果过程中任何一步失败，它会 panic 并显示错误，这在测试中是可接受的。
fn codegen_test_harness(source: &str) -> String {
    // 1. 词法分析 (Lexing)
    let (tokens, lexer_errors) = lexer::lex(source);
    if !lexer_errors.is_empty() {
        panic!("Lexing failed: {:?}", lexer_errors);
    }

    // 2. 解析 (Parsing)
    let (ast_program, parser_errors) = parser::parse(source.len(), tokens);
    if !parser_errors.is_empty() {
        panic!("Parsing failed: {:?}", parser_errors);
    }
    let ast_program = ast_program.expect("Parsing succeeded but produced no AST");

    // 3. 语义分析 (Analyzing)
    // 根据你的 analyzer API，我们需要创建一个实例然后调用 analyze
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


// --- Test Cases ---

#[test]
fn test_return_integer_literal() {
    // 测试一个最简单的函数，它只返回一个整数
    let source = "fn test_main() -> int { return 42; }";
    let llvm_ir = codegen_test_harness(source);

    // 打印 IR 以便调试
    println!("--- IR for test_return_integer_literal ---\n{}\n", llvm_ir);

    // 检查函数签名是否正确。CScript 的 `int` 应该对应 LLVM 的 `i32`
    assert!(
        llvm_ir.contains("define i32 @test_main()"),
        "Expected function signature 'define i32 @test_main()'"
    );

    // 检查函数体中是否有正确的返回指令
    assert!(
        llvm_ir.contains("ret i32 42"),
        "Expected 'ret i32 42'"
    );
}

#[test]
fn test_binary_expression() {
    // 测试包含二元运算的返回语句
    let source = "fn test_add() -> int { return 10 + 5; }";
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_binary_expression ---\n{}\n", llvm_ir);

    // 检查函数签名
    assert!(llvm_ir.contains("define i32 @test_add()"));

    // 检查 LLVM IR 是否包含加法指令。
    // `nsw` 表示 "No Signed Wrap"，是 LLVM 对有符号整数加法的一个常见标记。
    assert!(
        llvm_ir.contains("add nsw i32 10, 5"),
        "Expected 'add nsw i32 10, 5'"
    );

    // 检查最终的返回指令
    assert!(
        llvm_ir.contains("ret i32"),
        "Expected a return instruction for i32"
    );
}

#[test]
fn test_local_variable() {
    // 测试局部变量的声明、赋值和使用
    let source = r#"
        fn test_vars() -> int {
            let x: int = 100;
            let y: int = x + 20;
            return y;
        }
    "#;
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_local_variable ---\n{}\n", llvm_ir);

    // 1. 检查是否为局部变量 'x' 和 'y' 在栈上分配了空间
    //    `alloca` 是 LLVM 中用于在栈上分配内存的指令。
    //    我们期望有两句 alloca i32
    assert_eq!(llvm_ir.matches("alloca i32").count(), 2, "Expected two stack allocations for local variables");

    // 2. 检查是否将初始值 100 存储到了 'x' 的内存位置
    //    `store i32 100, ptr %...`
    assert!(
        llvm_ir.contains("store i32 100, ptr"),
        "Expected to store initial value 100"
    );

    // 3. 检查是否从 'x' 的内存位置加载了值以用于计算 'y'
    //    `load i32, ptr %...`
    assert!(
        llvm_ir.contains("load i32, ptr"),
        "Expected to load value from a variable"
    );

    // 4. 检查 `y` 的计算
    assert!(
        llvm_ir.contains("add nsw i32 %"),
        "Expected add instruction for `y`'s initialization"
    );

    // 5. 检查是否返回了最终计算出的 `y` 的值
    assert!(
        llvm_ir.contains("ret i32 %"),
        "Expected to return the value of `y`"
    );
}

#[test]
fn test_global_variable() {
    // 测试全局变量的定义和在函数中的使用
    let source = r#"
        let my_global: int = 77;

        fn test_global() -> int {
            return my_global;
        }
    "#;
    let llvm_ir = codegen_test_harness(source);

    println!("--- IR for test_global_variable ---\n{}\n", llvm_ir);

    // 1. 检查全局变量的定义
    //    全局变量在 LLVM IR 中以 '@' 开头。
    //    `@my_global = global i32 77`
    assert!(
        llvm_ir.contains("@my_global = global i32 77"),
        "Expected global variable definition"
    );

    // 2. 检查函数中是否加载了全局变量的值
    //    `load i32, ptr @my_global`
    assert!(
        llvm_ir.contains("load i32, ptr @my_global"),
        "Expected to load from global variable"
    );

    // 3. 检查是否返回了加载的值
    assert!(
        llvm_ir.contains("ret i32 %"),
        "Expected to return the loaded global value"
    );
}