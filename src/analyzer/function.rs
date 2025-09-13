// In src/analyzer/functions.rs

use super::{hir, statement};
use crate::analyzer::symbols::SymbolInfo;
use crate::parser::ast;
use crate::reporter::SemanticError; 
use std::sync::Arc;

/// [REFACTORED]
/// 在 PASS 2 中，降级所有函数定义。
/// 此函数会尝试降级所有函数，并收集所有遇到的错误，而不是在第一个错误处就停止。
pub(super) fn pass2_lower_functions(
    analyzer: &mut super::Analyzer,
    program: &ast::Program,
) -> Vec<hir::Function> {
    let mut hir_functions = Vec::new();

    for item in &program.items {
        if let ast::GlobalItem::Function(func_def) = item {
            // 使用 match 处理 Result，实现错误恢复
            match lower_function_definition(analyzer, func_def) {
                Ok(hir_func) => hir_functions.push(hir_func),
                Err(err) => analyzer.diagnostics.push(err.into()),
            }
        }
    }
    hir_functions
}

/// [REFACTORED]
/// 在 PASS 2 中，收集所有 extern 函数的 HIR 声明。
pub(super) fn pass2_collect_extern_declarations(
    analyzer: &mut super::Analyzer,
    program: &ast::Program,
) -> Vec<Arc<hir::FunctionDecl>> {
    let mut hir_extern_functions = Vec::new();
    for item in &program.items {
        if let ast::GlobalItem::Extern(extern_block) = item {
            for func_decl in &extern_block.declarations {
                // 在符号表中查找 Pass 1 中创建的权威声明
                if let Some(SymbolInfo::Function { decl }) =
                    analyzer.symbol_table.lookup_symbol(&func_decl.name)
                {
                    hir_extern_functions.push(decl.clone());
                } else {
                    // 这是一个内部错误：如果 Pass 1 成功，这里应该总能找到符号
                    // 可以在这里添加一个 internal error 的诊断信息
                }
            }
        }
    }
    hir_extern_functions
}

/// [REFACTORED]
/// 降级一个独立的函数定义，包括其参数和函数体。
/// 返回 Result 以便进行错误传播。
fn lower_function_definition(
    analyzer: &mut super::Analyzer,
    func_def: &ast::FunctionDef,
) -> Result<hir::Function, SemanticError> {
    // 从符号表中获取 Pass 1 创建的权威函数声明
    let func_decl = match analyzer.symbol_table.lookup_symbol(&func_def.name) {
        Some(SymbolInfo::Function { decl }) => decl.clone(),
        _ => unreachable!("Internal error: Function decl should exist from Pass 1"),
    };

    // --- 状态管理：设置当前函数的上下文 ---
    // 保存旧状态，以便在函数结束时恢复，这对于处理嵌套函数（如果支持）或仅仅保持状态清洁至关重要。
    let original_return_type =
        std::mem::replace(&mut analyzer.current_function_return_type, Some(func_decl.return_type.clone()));

    // [MODIFIED] 在进入函数作用域前，为新函数开始一个新的栈帧
    analyzer.frame_manager.begin_frame();
    analyzer.symbol_table.enter_scope();

    let mut hir_params = Vec::new();
    // 遍历 AST 中的参数定义，但使用 Pass 1 中解析好的权威类型
    for ((_param_ast_type, param_name), param_semantic_type) in
        func_def.params.iter().zip(func_decl.params.iter())
    {
        // 调用 frame_manager 来分配偏移量
        let offset = analyzer.frame_manager.allocate(param_semantic_type);
        let storage = hir::Storage::Local { offset }; 

        let decl = Arc::new(hir::VarDecl {
            name: param_name.clone(),
            var_type: param_semantic_type.clone(),
            is_const: false,
            storage, // 使用新计算出的 storage
            initializer: None,
        });

        analyzer.symbol_table.add_symbol(param_name, SymbolInfo::Variable { decl: decl.clone() })?;
        hir_params.push(decl);
    }

    // 创建用于降级函数体的上下文
    let mut body_ctx = analyzer.create_ctx(None);
    let body = statement::lower_block(&func_def.body, &mut body_ctx)?;

    // --- 状态恢复 ---
    analyzer.symbol_table.exit_scope();
    analyzer.current_function_return_type = original_return_type;

    Ok(hir::Function {
        name: func_def.name.clone(),
        params: hir_params,
        return_type: func_decl.return_type.clone(),
        body,
    })
}