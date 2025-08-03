// In src/analyzer/globals.rs

use super::{hir, AnalysisContext, Lower, semantic_error::SemanticError, symbols::SymbolInfo, types};
use crate::parser::ast;
use std::sync::Arc;
use std::collections::HashMap;

/// 执行 PASS 1: 遍历顶层物品，收集所有全局符号（函数、结构体、全局变量）的声明。
pub(super) fn pass1_collect_symbols(analyzer: &mut super::Analyzer, program: &ast::Program) -> Option<()> {
    for item in &program.items {
        match item {
            ast::GlobalItem::Extern(extern_block) => {
                for func_decl in &extern_block.declarations {
                    let ret_type = types::resolve_ast_type(&func_decl.return_type, &analyzer.symbol_table, analyzer.diagnostics);
                    let params_type: Vec<_> = func_decl.params.iter().map(|p| types::resolve_ast_type(&p.0, &analyzer.symbol_table, analyzer.diagnostics)).collect();
                    
                    let symbol_info = SymbolInfo::Function {
                        return_type: ret_type,
                        params: func_decl.params.iter().zip(params_type).map(|((_, ident), ty)| (ty, ident.name.clone())).collect(),
                        is_variadic: func_decl.is_variadic,
                    };
                    
                    if analyzer.symbol_table.add_symbol(&func_decl.name, symbol_info).is_err() {
                        analyzer.diagnostics.report(SemanticError::SymbolAlreadyExists(func_decl.name.clone()).into());
                    }
                }
            }
            ast::GlobalItem::Struct(struct_def) => {
                let mut resolved_fields = Vec::new();
                let mut offsets = HashMap::new();
                let mut current_offset = 0;

                for (field_name, field_ast_type) in &struct_def.fields {
                    let field_type = types::resolve_ast_type(field_ast_type, &analyzer.symbol_table, analyzer.diagnostics);
                    let field_size = field_type.size_of();
                    offsets.insert(field_name.name.clone(), current_offset);
                    resolved_fields.push((field_name.clone(), Arc::new(field_type)));
                    current_offset += field_size;
                }
                
                let struct_type = super::types::SemanticType::Struct {
                    name: struct_def.name.clone(),
                    fields: resolved_fields,
                    size: current_offset,
                    offsets,
                };

                if analyzer.symbol_table.add_symbol(&struct_def.name, SymbolInfo::Type { ty: struct_type }).is_err() {
                    analyzer.diagnostics.report(SemanticError::SymbolAlreadyExists(struct_def.name.clone()).into());
                }
            }
            ast::GlobalItem::Function(func_def) => {
                let ret_type = types::resolve_ast_type(&func_def.return_type, &analyzer.symbol_table, analyzer.diagnostics);
                let params_type: Vec<_> = func_def.params.iter().map(|p| types::resolve_ast_type(&p.0, &analyzer.symbol_table, analyzer.diagnostics)).collect();
                let symbol_info = SymbolInfo::Function {
                    return_type: ret_type,
                    params: func_def.params.iter().zip(params_type).map(|((_, ident), ty)| (ty, ident.name.clone())).collect(),
                    is_variadic: false,
                };
                if analyzer.symbol_table.add_symbol(&func_def.name, symbol_info).is_err() {
                    analyzer.diagnostics.report(SemanticError::SymbolAlreadyExists(func_def.name.clone()).into());
                }
            }
            ast::GlobalItem::VarDecl(var_decl) => {
                let var_type = types::resolve_ast_type(&var_decl.var_type, &analyzer.symbol_table, analyzer.diagnostics);
                let hir_decl = Arc::new(hir::VarDecl {
                    name: var_decl.name.clone(), var_type, is_const: var_decl.is_const,
                    storage: hir::Storage::Global { name: var_decl.name.name.clone() },
                    initializer: None,
                });
                if analyzer.symbol_table.add_symbol(&var_decl.name, SymbolInfo::Variable { decl: hir_decl }).is_err() {
                    analyzer.diagnostics.report(SemanticError::SymbolAlreadyExists(var_decl.name.clone()).into());
                }
            }
        }
    }
    Some(())
}

/// 在 PASS 2 中，分析全局变量的初始化表达式。
pub(super) fn pass2_lower_global_initializers(analyzer: &mut super::Analyzer, program: &ast::Program) -> Option<Vec<Arc<hir::VarDecl>>> {
    let mut hir_globals = Vec::new();
    for item in &program.items {
        if let ast::GlobalItem::VarDecl(var_decl) = item {
            if let Some(SymbolInfo::Variable { decl }) = analyzer.symbol_table.lookup_symbol(&var_decl.name) {
                let mut decl_data = (**decl).clone();
                if let Some(init) = &var_decl.init {
                    // [FIX] 删除了多余的 analyzer.diagnostics 参数
                    let mut init_ctx = analyzer.create_ctx(Some(&decl_data.var_type));
                    if let Some(hir_init) = init.lower(&mut init_ctx) {
                        decl_data.initializer = Some(hir_init);
                    }
                }
                let final_decl_arc = Arc::new(decl_data);
                hir_globals.push(final_decl_arc.clone());
                analyzer.symbol_table.update_symbol(&var_decl.name, SymbolInfo::Variable { decl: final_decl_arc });
            }
        }
    }
    Some(hir_globals)
}
