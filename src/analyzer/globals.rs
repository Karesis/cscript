// In src/analyzer/globals.rs

use super::{hir, AnalysisContext, Lower, symbols::SymbolInfo, types};
use crate::parser::ast;
use crate::reporter::{CompilerError, SemanticError}; 
use std::collections::HashMap;
use std::sync::Arc;

/// 执行 PASS 1: 遍历顶层项，收集所有全局符号（函数、结构体、全局变量）的声明。
/// 这个过程会收集遇到的所有错误，但不会因单个错误而中止。
pub(super) fn pass1_collect_symbols(analyzer: &mut super::Analyzer, program: &ast::Program) {
    for item in &program.items {
        // 使用 match 来处理 Result，以便在出错时可以继续循环
        let result = match item {
            ast::GlobalItem::Extern(extern_block) => {
                collect_extern_function_symbols(analyzer, extern_block)
            }
            ast::GlobalItem::Struct(struct_def) => {
                collect_struct_symbol(analyzer, struct_def)
            }
            ast::GlobalItem::Function(func_def) => {
                collect_function_symbol(analyzer, func_def)
            }
            ast::GlobalItem::VarDecl(var_decl) => {
                collect_global_variable_symbol(analyzer, var_decl)
            }
        };

        if let Err(err) = result {
            analyzer.diagnostics.push(err.into());
        }
    }
}

// [NEW] 将每个全局项的符号收集逻辑拆分为独立的辅助函数，使代码更清晰
fn collect_extern_function_symbols(
    analyzer: &mut super::Analyzer,
    extern_block: &ast::ExternBlock,
) -> Result<(), SemanticError> {
    for func_decl in &extern_block.declarations {
        let return_type = types::resolve_ast_type(&func_decl.return_type, &analyzer.symbol_table)?;

        // 使用 collect::<Result<...>> 来优雅地处理多个参数类型的解析
        let params = func_decl
            .params
            .iter()
            .map(|(ty, _)| types::resolve_ast_type(ty, &analyzer.symbol_table))
            .collect::<Result<Vec<_>, _>>()?;

        // [REFACTORED] 创建 hir::FunctionDecl 作为权威声明
        let hir_decl = Arc::new(hir::FunctionDecl {
            name: func_decl.name.clone(),
            params,
            return_type,
            is_variadic: func_decl.is_variadic,
        });

        // [REFACTORED] 存储指向权威声明的 SymbolInfo
        let symbol_info = SymbolInfo::Function { decl: hir_decl };
        analyzer.symbol_table.add_symbol(&func_decl.name, symbol_info)?;
    }
    Ok(())
}

fn collect_struct_symbol(
    analyzer: &mut super::Analyzer,
    struct_def: &ast::StructDef,
) -> Result<(), SemanticError> {
    let mut resolved_fields = Vec::new();
    let mut offsets = HashMap::new();
    let mut current_offset = 0;

    for (field_name, field_ast_type) in &struct_def.fields {
        let field_type = types::resolve_ast_type(field_ast_type, &analyzer.symbol_table)?;
        let field_size = field_type.size_of();
        
        offsets.insert(field_name.name.clone(), current_offset);
        resolved_fields.push((field_name.clone(), Arc::new(field_type)));
        current_offset += field_size;
    }

    let struct_type = types::SemanticType::Struct {
        name: struct_def.name.clone(),
        fields: resolved_fields,
        size: current_offset,
        offsets,
    };

    let symbol_info = SymbolInfo::Type { ty: struct_type };
    analyzer.symbol_table.add_symbol(&struct_def.name, symbol_info)?;
    Ok(())
}

fn collect_function_symbol(
    analyzer: &mut super::Analyzer,
    func_def: &ast::FunctionDef,
) -> Result<(), SemanticError> {
    let return_type = types::resolve_ast_type(&func_def.return_type, &analyzer.symbol_table)?;
    
    let params = func_def
        .params
        .iter()
        .map(|(ty, _)| types::resolve_ast_type(ty, &analyzer.symbol_table))
        .collect::<Result<Vec<_>, _>>()?;

    // [REFACTORED] 同样为普通函数创建 hir::FunctionDecl
    let hir_decl = Arc::new(hir::FunctionDecl {
        name: func_def.name.clone(),
        params,
        return_type,
        is_variadic: false,
    });

    let symbol_info = SymbolInfo::Function { decl: hir_decl };
    analyzer.symbol_table.add_symbol(&func_def.name, symbol_info)?;
    Ok(())
}

fn collect_global_variable_symbol(
    analyzer: &mut super::Analyzer,
    var_decl: &ast::VarDecl,
) -> Result<(), SemanticError> {
    let var_type = types::resolve_ast_type(&var_decl.var_type, &analyzer.symbol_table)?;

    // Pass 1 只创建声明，初始化器为 None
    let hir_decl = Arc::new(hir::VarDecl {
        name: var_decl.name.clone(),
        var_type,
        is_const: var_decl.is_const,
        storage: hir::Storage::Global { name: var_decl.name.name.clone() },
        initializer: None,
    });

    let symbol_info = SymbolInfo::Variable { decl: hir_decl };
    analyzer.symbol_table.add_symbol(&var_decl.name, symbol_info)?;
    Ok(())
}

/// [REFACTORED]
/// 在 PASS 2 中，分析全局变量的初始化表达式。
pub(super) fn pass2_lower_global_initializers(
    analyzer: &mut super::Analyzer,
    program: &ast::Program,
) -> Vec<Arc<hir::VarDecl>> {
    let mut hir_globals = Vec::new();
    for item in &program.items {
        if let ast::GlobalItem::VarDecl(var_decl) = item {
            // 从符号表中取出 Pass 1 创建的声明
            if let Some(SymbolInfo::Variable { decl }) = analyzer.symbol_table.lookup_symbol(&var_decl.name) {
                // clone() 会复制 Arc 指针，而不是内部数据，开销很小
                let mut decl_clone = (**decl).clone();

                if let Some(init_expr) = &var_decl.init {
                    let mut ctx = analyzer.create_ctx(Some(&decl_clone.var_type));
                    
                    // [REFACTORED] lower 方法现在也返回 Result
                    match init_expr.lower(&mut ctx) {
                        Ok(hir_init) => {
                            decl_clone.initializer = Some(hir_init);
                        }
                        Err(err) => {
                            analyzer.diagnostics.push(err.into());
                        }
                    }
                }
                
                let final_decl = Arc::new(decl_clone);
                hir_globals.push(final_decl.clone());
                
                // [REFACTORED] 使用更清晰的 update_global_symbol
                analyzer.symbol_table.update_global_symbol(
                    &var_decl.name,
                    SymbolInfo::Variable { decl: final_decl },
                );
            }
        }
    }
    hir_globals
}