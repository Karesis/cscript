// In src/analyzer/functions.rs

use super::{hir, AnalysisContext, Lower, semantic_error::SemanticError, symbols::SymbolInfo, types};
use crate::parser::ast;
use std::sync::Arc;
use super::statement;

/// 在 PASS 2 中，降级所有函数定义。
pub(super) fn pass2_lower_functions(analyzer: &mut super::Analyzer, program: &ast::Program) -> Option<Vec<hir::Function>> {
    program.items.iter().filter_map(|item| {
        if let ast::GlobalItem::Function(func_def) = item {
            Some(lower_function_definition(analyzer, func_def))
        } else {
            None
        }
    }).collect()
}

/// 在 PASS 2 中，收集所有 extern 函数的 HIR 声明。
pub(super) fn pass2_collect_extern_declarations(analyzer: &mut super::Analyzer, program: &ast::Program) -> Vec<Arc<hir::FunctionDecl>> {
    let mut hir_extern_functions = Vec::new();
    for item in &program.items {
        if let ast::GlobalItem::Extern(extern_block) = item {
            for func_decl in &extern_block.declarations {
                let return_type = types::resolve_ast_type(&func_decl.return_type, &analyzer.symbol_table, analyzer.diagnostics);
                let params = func_decl.params.iter().map(|(ty, _)| types::resolve_ast_type(ty, &analyzer.symbol_table, analyzer.diagnostics)).collect();
                let hir_decl = Arc::new(hir::FunctionDecl {
                    name: func_decl.name.clone(), params, return_type, is_variadic: func_decl.is_variadic,
                });
                hir_extern_functions.push(hir_decl);
            }
        }
    }
    hir_extern_functions
}

/// [MOVED] 从旧 Analyzer 中移出的 visit_function 逻辑
fn lower_function_definition(analyzer: &mut super::Analyzer, func_def: &ast::FunctionDef) -> Option<hir::Function> {
    let return_type = types::resolve_ast_type(&func_def.return_type, &analyzer.symbol_table, analyzer.diagnostics);
    analyzer.current_function_return_type = Some(return_type.clone());
    // analyzer.current_stack_offset = 0; // 栈偏移计算需要重新设计

    analyzer.symbol_table.enter_scope();
    
    let mut hir_params = Vec::new();
    for (param_type, param_name) in &func_def.params {
        let resolved_type = types::resolve_ast_type(param_type, &analyzer.symbol_table, analyzer.diagnostics);
        let decl = Arc::new(hir::VarDecl {
            name: param_name.clone(),
            var_type: resolved_type, is_const: false,
            storage: hir::Storage::Local { offset: 0 }, // 栈偏移计算需要重新设计
            initializer: None,
        });
        if analyzer.symbol_table.add_symbol(param_name, SymbolInfo::Variable { decl: decl.clone() }).is_err() {
            analyzer.diagnostics.report(SemanticError::SymbolAlreadyExists(param_name.clone()).into());
        }
        hir_params.push(decl);
    }
    
    // [FIX #1] create_ctx 只接受一个参数
    let mut body_ctx = analyzer.create_ctx(None);
    // [FIX #2] 调用正确的 statement::lower_block 辅助函数
    let body = statement::lower_block(&func_def.body, &mut body_ctx)?;

    analyzer.symbol_table.exit_scope();
    analyzer.current_function_return_type = None;

    Some(hir::Function {
        name: func_def.name.clone(),
        params: hir_params,
        return_type,
        body, // body 现在是 hir::Block 类型，无需 unwrap
    })
}
