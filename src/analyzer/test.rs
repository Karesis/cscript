// In src/analyzer/tests.rs

use crate::analyzer::{self, hir};
use crate::lexer::lex;
use crate::parser::parse;
use crate::reporter::{CompilerError, SemanticError};

// 一个通用的设置函数，负责词法和语法分析
fn setup(src: &str) -> Result<crate::parser::ast::Program, Vec<CompilerError>> {
    // 假设 `lex` 和 `parse` 函数位于一个名为 `compiler` 或 `crate` 的顶层模块
    // 如果不是，请相应地调整路径
    let (tokens, lex_errors) = lex(src);

    // 测试的“前置条件”：词法分析必须完全正确
    if !lex_errors.is_empty() {
        return Err(lex_errors);
    }

    let (ast, parse_errors) = parse(src.len(), tokens);

    // 测试的“前置条件”：语法分析也必须完全正确
    if !parse_errors.is_empty() {
        return Err(parse_errors);
    }

    // 只有在没有词法和语法错误，并且成功生成了 AST 的情况下，setup 才算成功
    match ast {
        Some(program) => Ok(program),
        None => {
            // 这种情况理论上不应发生：如果没有错误，就应该有 AST。
            // 但为了健壮性，我们在这里 panic，因为它表示 parser 内部逻辑有问题。
            panic!("Parsing succeeded with no errors, but no AST was produced.");
        }
    }
}

/// 辅助函数：用于测试应该成功通过分析的代码。
/// 如果分析失败，它会 panic 并显示所有错误，让测试失败。
/// 如果成功，它返回生成的 hir::Program 供我们检查。
fn analyze_ok(src: &str) -> hir::Program {
    let ast = match setup(src) {
        Ok(program) => program,
        Err(errs) => panic!("Test setup failed (parsing errors): {:?}", errs),
    };

    let analyzer = analyzer::Analyzer::new();
    match analyzer.analyze(&ast) {
        Ok(hir) => hir,
        Err(errs) => panic!("Analysis failed unexpectedly: {:?}", errs),
    }
}

/// 辅助函数：用于测试应该产生语义错误的代码。
/// 如果分析成功，它会 panic，让测试失败。
/// 如果失败，它返回收集到的错误列表供我们检查。
fn analyze_err(src: &str) -> Vec<CompilerError> {
    let ast = match setup(src) {
        Ok(program) => program,
        Err(errs) => panic!("Test setup failed (parsing errors): {:?}", errs),
    };

    let analyzer = analyzer::Analyzer::new();
    match analyzer.analyze(&ast) {
        Ok(_) => panic!("Analysis succeeded unexpectedly for source:\n{}", src),
        Err(errs) => errs,
    }
}

// In src/analyzer/tests.rs

#[cfg(test)]
mod expressions {
    use super::*;
    use crate::analyzer::types::SemanticType;

    #[test]
    fn test_global_addition_expression_type() {
        // 测试：全局变量的加法表达式，类型应为 i32
        let src = "result: i32 = 2 + 3;";
        let hir = analyze_ok(src);

        assert_eq!(hir.globals.len(), 1);
        let var_decl = &hir.globals[0];
        let initializer = var_decl.initializer.as_ref().unwrap();
        
        assert_eq!(initializer.resolved_type, SemanticType::i32());
    }

    #[test]
    fn test_comparison_expression_type() {
        // 测试：比较运算符的表达式，类型应为 bool
        let src = "result: bool = 10 > 5;";
        let hir = analyze_ok(src);

        let var_decl = &hir.globals[0];
        let initializer = var_decl.initializer.as_ref().unwrap();

        assert_eq!(initializer.resolved_type, SemanticType::Bool);
    }

    #[test]
    fn test_boolean_logic_expression_type() {
        // 测试：逻辑运算符的表达式，类型应为 bool
        let src = "result: bool = true && false;";
        let hir = analyze_ok(src);

        let var_decl = &hir.globals[0];
        let initializer = var_decl.initializer.as_ref().unwrap();

        assert_eq!(initializer.resolved_type, SemanticType::Bool);
    }
}

#[cfg(test)]
mod statements {
    use super::*;
    use crate::analyzer::types::SemanticType;

    #[test]
    fn test_valid_if_statement() {
        // 测试：条件为 bool 的 if 语句应该能成功分析
        let src = "main() -> void { if (true) {} }";
        // analyze_ok 如果不 panic 就代表测试通过
        let _ = analyze_ok(src);
    }

    #[test]
    fn test_valid_while_loop() {
        // 测试：条件为 bool 的 while 语句应该能成功分析
        let src = "main() -> void { while (1 < 2) {} }";
        let _ = analyze_ok(src);
    }
    
    #[test]
    fn test_valid_return_statement() {
        // 测试：函数内返回正确类型的值
        let src = "get_num() -> i32 { return 42; }";
        let hir = analyze_ok(src);

        let func = &hir.functions[0];
        let body_stmts = &func.body.stmts;

        // [MODIFIED] 修正这里的模式匹配
        let return_value = match &body_stmts[0] {
            // 使用 { } 来匹配 struct variant，并直接提取 value 字段
            hir::Statement::Return { value, .. } => value,
            _ => panic!("Expected a return statement"), // 也修正了这里的 panic 信息
        };
        
        // `return_value` 现在是 Option<&hir::Expression> 类型
        let initializer = return_value.as_ref().expect("Return statement should have a value");
        
        // 确保返回值 42 的类型被正确推导为 i32
        assert_eq!(initializer.resolved_type, SemanticType::i32());
    }
}

#[cfg(test)]
mod scopes {
    use super::*;

    #[test]
    fn test_variable_shadowing_is_allowed() {
        // 测试：在内层作用域定义同名变量（shadowing）是合法的
        let src = r#"
            main() -> void {
                x: i32 = 10;
                {
                    x: bool = true; // 新的 x，隐藏了外层的 x
                }
                // 这里的 x 仍然是 i32
            }
        "#;
        // 只要分析不报错，就认为 shadowing 的基本逻辑是正确的
        let _ = analyze_ok(src);
    }
}

#[cfg(test)]
mod errors {
    use super::*;

    #[test]
    fn test_error_type_mismatch_in_binary_op() {
        // 测试：二元表达式类型不匹配
        let src = "x: i32 = 2 + true;";
        let errors = analyze_err(src);

        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            CompilerError::Semantic(SemanticError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn test_error_undefined_variable() {
        // 测试：使用未定义的变量
        let src = "x: i32 = y;"; // y 未定义
        let errors = analyze_err(src);

        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            CompilerError::Semantic(SemanticError::UndefinedVariable { name, .. }) if name == "y"
        ));
    }

    #[test]
    fn test_error_redefinition_in_same_scope() {
        // 测试：在同一作用域内重复定义变量
        let src = "main() -> void { x: i32 = 1; x: bool = true; }";
        let errors = analyze_err(src);

        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            CompilerError::Semantic(SemanticError::Redefinition { name, .. }) if name == "x"
        ));
    }

    #[test]
    fn test_error_if_condition_not_bool() {
        // 测试：if 语句的条件不是 bool 类型
        let src = "main() -> void { if (123) {} }";
        let errors = analyze_err(src);

        assert_eq!(errors.len(), 1);
        let err = &errors[0];
        assert!(matches!(
            err,
            CompilerError::Semantic(SemanticError::TypeMismatch { .. })
        ));
        if let CompilerError::Semantic(SemanticError::TypeMismatch{ expected, found, ..}) = err {
            assert_eq!(expected, "bool");
            assert_eq!(found, "integer");
        }
    }

    #[test]
    fn test_error_assignment_to_const() {
        // 测试：对常量进行赋值
        // 注意：这需要您的 parser 支持 `const` 关键字
        let src = "main() -> void { const x: i32 = 10; x = 20; }";
        let errors = analyze_err(src);
        
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            CompilerError::Semantic(SemanticError::AssignmentToConst { .. })
        ));
    }

    #[test]
    fn test_error_break_outside_loop() {
        // 测试：在循环外部使用 break
        let src = "main() -> void { break; }";
        let errors = analyze_err(src);
        
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            CompilerError::Semantic(SemanticError::ControlFlowOutsideLoop { keyword, .. }) if keyword == "break"
        ));
    }
}

#[test]
fn test_initializer_difference() { // 可以创建一个新测试
    // [EXPERIMENT 3] 只将初始值从 100 改为 2 + 3
    let src = "g_my_var: i32 = 2 + 3;"; 
    let _ = analyze_ok(src); // 我们只关心它是否 panic
}