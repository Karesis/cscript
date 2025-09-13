// In src/analyzer/statement.rs

use super::{hir, AnalysisContext, Lower}; // 从父模块导入核心抽象
use crate::analyzer::symbols::SymbolInfo;
use crate::parser::ast;
use crate::reporter::SemanticError; 
use crate::utils::Span;
use std::sync::Arc;

/// [REFACTORED] 为 ast::Statement 实现 Lower trait。
/// 它现在返回 Result，并使用 `?` 来优雅地传播错误。
impl Lower for ast::Statement {
    type Output = hir::Statement;

    fn lower(&self, ctx: &mut AnalysisContext<'_>) -> Result<Self::Output, SemanticError> {
        match self {
            ast::Statement::Block(block) => Ok(hir::Statement::Block(lower_block(block, ctx)?)),
            ast::Statement::VarDecl(var_decl) => lower_var_decl(var_decl, ctx),
            ast::Statement::Expr(expr) => Ok(hir::Statement::Expr(expr.lower(ctx)?)),
            ast::Statement::Return { value, span } => {
                lower_return_statement(value.as_ref(), span, ctx)
            }
            ast::Statement::If { condition, then_branch, else_branch, span } => {
                lower_if_statement(condition, then_branch, else_branch.as_deref(), span, ctx)
            }
            ast::Statement::While { condition, body, span } => {
                lower_while_statement(condition, body, span, ctx)
            }
            ast::Statement::Break(span) => lower_break_statement(span, ctx),
            ast::Statement::Continue(span) => lower_continue_statement(span, ctx),
        }
    }
}

// === 辅助函数 ===

/// 降级一个语句块。
pub fn lower_block(block: &ast::Block, ctx: &mut AnalysisContext<'_>) -> Result<hir::Block, SemanticError> {
    ctx.symbol_table.enter_scope();

    // [REFACTORED] 使用 collect::<Result<...>> 来处理可能失败的迭代
    let hir_stmts = block
        .stmts
        .iter()
        .map(|stmt| stmt.lower(ctx)) // 递归调用 Lower trait
        .collect::<Result<Vec<_>, _>>()?;

    ctx.symbol_table.exit_scope();
    Ok(hir::Block { stmts: hir_stmts })
}

/// 降级一个变量声明语句。
fn lower_var_decl(var_decl: &ast::VarDecl, ctx: &mut AnalysisContext<'_>) -> Result<hir::Statement, SemanticError> {
    // [REFACTORED] resolve_ast_type 现在返回 Result，直接使用 `?`
    let var_type = super::types::resolve_ast_type(&var_decl.var_type, ctx.symbol_table)?;

    // [REFACTORED] 使用 .transpose()? 来优雅地处理 Option<Result<T, E>>
    let hir_initializer = var_decl
        .init
        .as_ref()
        .map(|init_expr| {
            let mut init_ctx = AnalysisContext {
                symbol_table: ctx.symbol_table,
                current_function_return_type: ctx.current_function_return_type,
                frame_manager: ctx.frame_manager,
                loop_depth: ctx.loop_depth,
                expected_type: Some(&var_type),
            };
            init_expr.lower(&mut init_ctx)
        })
        .transpose()?;

    // [MODIFIED] 现在可以通过上下文访问 frame_manager 了！
    let offset = ctx.frame_manager.allocate(&var_type);
    let storage = hir::Storage::Local { offset }; 

     let decl = Arc::new(hir::VarDecl {
        name: var_decl.name.clone(),
        var_type,
        is_const: var_decl.is_const,
        storage, // 使用为局部变量计算出的 storage
        initializer: hir_initializer,
    });

    // [REFACTORED] add_symbol 现在返回 Result，直接使用 `?`
    ctx.symbol_table.add_symbol(&var_decl.name, SymbolInfo::Variable { decl: decl.clone() })?;

    Ok(hir::Statement::VarDecl(decl))
}

/// 降级一个 return 语句。
fn lower_return_statement(
    value: Option<&ast::Expression>,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Statement, SemanticError> {
    let return_type = ctx.current_function_return_type.as_ref().expect("Internal error: return outside function context");

    let hir_value = match value {
        Some(expr) => {
            let mut value_ctx = AnalysisContext {
                // 对可变引用进行“再借用”
                symbol_table: &mut *ctx.symbol_table, 
                loop_depth: &mut *ctx.loop_depth,     
                frame_manager: &mut *ctx.frame_manager,
                // 共享引用是 Copy 的，可以直接复制
                current_function_return_type: ctx.current_function_return_type,

                // 使用新提供的值
                expected_type: Some(return_type),
            };
            Some(expr.lower(&mut value_ctx)?)
        }
        None => {
            // [REFACTORED] 如果函数需要返回值但没有提供，直接返回错误
            if !return_type.is_void() {
                return Err(SemanticError::TypeMismatch {
                    expected: return_type.to_string(),
                    found: "void".to_string(),
                    span: (*span).into(),
                });
            }
            None
        }
    };

    Ok(hir::Statement::Return { value: hir_value, span: *span })
}

/// 降级一个 if 语句。
fn lower_if_statement(
    condition: &ast::Expression,
    then_branch: &ast::Block,
    else_branch: Option<&ast::Block>,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Statement, SemanticError> {
    let cond_hir = {
        let mut cond_ctx = AnalysisContext {
            // 对可变字段进行再借用
            symbol_table: &mut *ctx.symbol_table,
            loop_depth: &mut *ctx.loop_depth,
            frame_manager: &mut *ctx.frame_manager,
            // 复制共享引用字段
            current_function_return_type: ctx.current_function_return_type,

            // 设置新的期望类型
            expected_type: Some(&super::types::SemanticType::Bool),
        };
        condition.lower(&mut cond_ctx)?
    };

    // [REFACTORED] 条件类型不匹配时直接返回错误
    if cond_hir.resolved_type != super::types::SemanticType::Bool {
        return Err(SemanticError::TypeMismatch {
            expected: "bool".to_string(),
            found: cond_hir.resolved_type.to_string(),
            span: condition.span.into(),
        });
    }

    let then_hir = lower_block(then_branch, ctx)?;
    
    // [REFACTORED] 同样使用 .transpose()? 优雅处理 Option<Result<...>>
    let else_hir = else_branch.map(|b| lower_block(b, ctx)).transpose()?;

    Ok(hir::Statement::If {
        condition: cond_hir,
        then_branch: then_hir,
        else_branch: else_hir,
        span: *span,
    })
}

/// 降级一个 while 语句。
fn lower_while_statement(
    condition: &ast::Expression,
    body: &ast::Block,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Statement, SemanticError> {
    let cond_hir = {
        let mut cond_ctx = AnalysisContext {
            // 对可变字段进行再借用
            symbol_table: &mut *ctx.symbol_table,
            loop_depth: &mut *ctx.loop_depth,
            frame_manager: &mut *ctx.frame_manager,
            // 复制共享引用字段
            current_function_return_type: ctx.current_function_return_type,

            // 设置新的期望类型
            expected_type: Some(&super::types::SemanticType::Bool),
        };
        condition.lower(&mut cond_ctx)?
    };
    
    if cond_hir.resolved_type != super::types::SemanticType::Bool {
        return Err(SemanticError::TypeMismatch {
            expected: "bool".to_string(),
            found: cond_hir.resolved_type.to_string(),
            span: condition.span.into(),
        });
    }

    *ctx.loop_depth += 1;
    let body_hir = lower_block(body, ctx)?;
    *ctx.loop_depth -= 1;

    Ok(hir::Statement::While {
        condition: cond_hir,
        body: body_hir,
        span: *span,
    })
}

/// 降级一个 break 语句。
fn lower_break_statement(span: &Span, ctx: &mut AnalysisContext<'_>) -> Result<hir::Statement, SemanticError> {
    if *ctx.loop_depth == 0 {
        // [REFACTORED] 不在循环中时直接返回错误
        return Err(SemanticError::ControlFlowOutsideLoop {
            keyword: "break".to_string(),
            span: (*span).into(),
        });
    }
    Ok(hir::Statement::Break(*span))
}

/// 降级一个 continue 语句。
fn lower_continue_statement(span: &Span, ctx: &mut AnalysisContext<'_>) -> Result<hir::Statement, SemanticError> {
    if *ctx.loop_depth == 0 {
        return Err(SemanticError::ControlFlowOutsideLoop {
            keyword: "continue".to_string(),
            span: (*span).into(),
        });
    }
    Ok(hir::Statement::Continue(*span))
}