// tests/codegen.rs

// 引入我们编译器库的主入口
use cscript::{parser, analyzer, codegen};
use std::process::{Command, Stdio};
use std::io::Write;

/// 端到端测试的辅助函数
fn run_e2e_test(source: &str) -> Result<i32, String> {
    // 1. 语法分析
    let (ast, parse_errs) = parser::parse_source(source);
    if !parse_errs.is_empty() {
        return Err(format!("Parsing failed: {:?}", parse_errs));
    }
    let ast = ast.unwrap();

    // 2. 语义分析
    let mut analyzer = analyzer::main::Analyzer::new();
    let hir = match analyzer.analyze(&ast) {
        Ok(hir) => hir,
        Err(errs) => return Err(format!("Analysis failed: {:?}", errs)),
    };

    // 3. 代码生成
    let llvm_ir = match codegen::codegen(&hir) {
        Ok(ir) => ir,
        Err(err) => return Err(format!("CodeGen failed: {:?}", err)),
    };

    // 4. 使用 Clang 编译 LLVM IR 为可执行文件
    let mut clang_child = Command::new("clang")
        .arg("-x")
        .arg("ir")
        .arg("-") // 从标准输入读取
        .arg("-o")
        .arg("test_output") // 输出文件名
        .stdin(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to spawn clang: {}", e))?;

    clang_child.stdin.as_mut().unwrap().write_all(llvm_ir.as_bytes()).unwrap();
    
    let clang_status = clang_child.wait().map_err(|e| format!("Clang failed to run: {}", e))?;
    if !clang_status.success() {
        return Err("Clang compilation failed".to_string());
    }

    // 5. 运行生成的可执行文件并获取退出码
    let run_status = Command::new("./test_output")
        .status()
        .map_err(|e| format!("Failed to run compiled output: {}", e))?;

    // 清理生成的文件
    let _ = std::fs::remove_file("test_output");

    Ok(run_status.code().unwrap_or(-1))
}

#[test]
fn test_e2e_simple_return() {
    let source = "int main() { return 42; }";
    let exit_code = run_e2e_test(source).expect("E2E test failed");
    assert_eq!(exit_code, 42);
}

#[test]
fn test_e2e_local_variables_and_arithmetic() {
    let source = r#"
        int main() {
            int x = 10;
            int y = 5;
            int z = (x * 2) + (y - 3); // (10 * 2) + (5 - 3) = 20 + 2 = 22
            return z;
        }
    "#;
    let exit_code = run_e2e_test(source).expect("E2E test failed");
    assert_eq!(exit_code, 22);
}

#[test]
fn test_e2e_if_else_statement() {
    let source = r#"
        int main() {
            int x = 100;
            if (x > 50) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;
    let exit_code = run_e2e_test(source).expect("E2E test failed");
    assert_eq!(exit_code, 1);
}

#[test]
fn test_e2e_while_loop() {
    let source = r#"
        int main() {
            int i = 0;
            int sum = 0;
            while (i < 5) { // 循环 0, 1, 2, 3, 4
                sum = sum + i;
                i = i + 1;
            }
            return sum; // 0 + 1 + 2 + 3 + 4 = 10
        }
    "#;
    let exit_code = run_e2e_test(source).expect("E2E test failed");
    assert_eq!(exit_code, 10);
}

#[test]
fn test_e2e_function_call_with_params() {
    let source = r#"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            return add(15, 27); // 42
        }
    "#;
    let exit_code = run_e2e_test(source).expect("E2E test failed");
    assert_eq!(exit_code, 42);
}