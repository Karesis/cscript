// In src/analyzer/statement.rs

use super::{hir, AnalysisContext, Lower}; // 从父模块导入核心抽象
use crate::analyzer::{semantic_error::SemanticError, symbols::SymbolInfo, types::SemanticType};
use crate::parser::ast;
use crate::lexer::Span;
use std::sync::Arc;

// [NEW] 为 ast::Statement 实现 Lower trait
// 它将成为新的“语句降级分发器”
impl Lower for ast::Statement {
    type Output = hir::Statement;

    fn lower<'a>(&self, ctx: &mut AnalysisContext<'a>) -> Option<Self::Output> {
        match self {
            ast::Statement::Block(block) => lower_block(block, ctx).map(hir::Statement::Block),
            ast::Statement::VarDecl(var_decl) => lower_var_decl(var_decl, ctx),
            ast::Statement::Expr(expr) => lower_expr_statement(expr, ctx),
            ast::Statement::Return { value, span } => lower_return_statement(value.as_ref(), span, ctx),
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

pub fn lower_block<'a>(block: &ast::Block, ctx: &mut AnalysisContext<'a>) -> Option<hir::Block> {
    ctx.symbol_table.enter_scope();
    let hir_stmts = block
        .stmts
        .iter()
        .map(|stmt| stmt.lower(ctx)) // 递归调用 Lower trait
        .collect::<Option<Vec<_>>>()?;
    ctx.symbol_table.exit_scope();
    Some(hir::Block { stmts: hir_stmts })
}

fn lower_var_decl<'a>(var_decl: &ast::VarDecl, ctx: &mut AnalysisContext<'a>) -> Option<hir::Statement> {
    let var_type = super::types::resolve_ast_type(&var_decl.var_type, ctx.symbol_table, ctx.diagnostics);
    
    let hir_initializer = if let Some(init_expr) = &var_decl.init {
        let mut init_ctx = AnalysisContext {
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            expected_type: Some(&var_type),
        };
        init_expr.lower(&mut init_ctx)
    } else {
        None
    };

    let size = var_type.size_of();
    // TODO: Stack offset calculation needs to be owned by Analyzer, not global
    let storage = hir::Storage::Local { offset: 0 }; // Placeholder for now
    let decl = Arc::new(hir::VarDecl {
        name: var_decl.name.clone(),
        var_type,
        is_const: var_decl.is_const,
        storage,
        initializer: hir_initializer,
    });

    if ctx.symbol_table.add_symbol(&var_decl.name, SymbolInfo::Variable { decl: decl.clone() }).is_err() {
        ctx.diagnostics.report(SemanticError::SymbolAlreadyExists(var_decl.name.clone()).into());
    }

    Some(hir::Statement::VarDecl(decl))
}

fn lower_expr_statement<'a>(expr: &ast::Expression, ctx: &mut AnalysisContext<'a>) -> Option<hir::Statement> {
    expr.lower(ctx).map(hir::Statement::Expr)
}

fn lower_return_statement<'a>(
    value: Option<&ast::Expression>,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Statement> {
    let return_type = ctx.current_function_return_type
        .clone()
        .expect("Internal error: No function context for return");

    let hir_value = if let Some(expr) = value {
        let mut value_ctx = AnalysisContext {
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            expected_type: Some(&return_type),
        };
        expr.lower(&mut value_ctx)
    } else {
        if return_type != SemanticType::Void {
            ctx.diagnostics.report(
                SemanticError::TypeMismatch {
                    expected: return_type,
                    found: SemanticType::Void,
                    span: span.clone(),
                }.into(),
            );
        }
        None
    };
    Some(hir::Statement::Return { value: hir_value, span: span.clone() })
}

fn lower_if_statement<'a>(
    condition: &ast::Expression,
    then_branch: &ast::Block,
    else_branch: Option<&ast::Block>,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Statement> {
    // [FIX] 完整地为条件表达式创建临时上下文
    let cond_hir = {
        let mut cond_ctx = AnalysisContext {
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            expected_type: Some(&SemanticType::Bool), // If 语句的条件必须是布尔值
        };
        condition.lower(&mut cond_ctx)?
    };

    if cond_hir.resolved_type != SemanticType::Bool {
        ctx.diagnostics.report(SemanticError::TypeMismatch {
            expected: SemanticType::Bool,
            found: cond_hir.resolved_type.clone(),
            span: condition.span.clone(),
        }.into());
    }

    let then_hir = lower_block(then_branch, ctx)?;
    let else_hir = if let Some(else_b) = else_branch {
        // 1. 调用 lower_block，它返回 Option<hir::Block>
        // 2. 使用 ? 操作符：如果 lower_block 返回 None，整个 lower_if_statement 就会在这里提前返回 None
        // 3. 如果 lower_block 返回 Some(hir_block)，? 会解包得到 hir_block
        // 4. 我们再用 Some() 把它包回去，赋值给 else_hir
        Some(lower_block(else_b, ctx)?)
    } else {
        // 如果 originally a None, 就还是 None
        None
    };

    Some(hir::Statement::If {
        condition: cond_hir,
        then_branch: then_hir,
        else_branch: else_hir,
        span: span.clone(),
    })
}

fn lower_while_statement<'a>(
    condition: &ast::Expression,
    body: &ast::Block,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Statement> {
    // [FIX] 完整地为条件表达式创建临时上下文
    let cond_hir = {
        let mut cond_ctx = AnalysisContext {
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            expected_type: Some(&SemanticType::Bool), // While 语句的条件也必须是布尔值
        };
        condition.lower(&mut cond_ctx)?
    };

    if cond_hir.resolved_type != SemanticType::Bool {
        ctx.diagnostics.report(SemanticError::TypeMismatch {
            expected: SemanticType::Bool,
            found: cond_hir.resolved_type.clone(),
            span: condition.span.clone(),
        }.into());
    }

    *ctx.loop_depth += 1;
    let body_hir = lower_block(body, ctx)?;
    *ctx.loop_depth -= 1;

    Some(hir::Statement::While {
        condition: cond_hir,
        body: body_hir,
        span: span.clone(),
    })
}

fn lower_break_statement<'a>(span: &Span, ctx: &mut AnalysisContext<'a>) -> Option<hir::Statement> {
    if *ctx.loop_depth == 0 {
        ctx.diagnostics.report(SemanticError::BreakOutsideLoop(span.clone()).into());
    }
    Some(hir::Statement::Break(span.clone()))
}

fn lower_continue_statement<'a>(span: &Span, ctx: &mut AnalysisContext<'a>) -> Option<hir::Statement> {
    if *ctx.loop_depth == 0 {
        ctx.diagnostics.report(SemanticError::ContinueOutsideLoop(span.clone()).into());
    }
    Some(hir::Statement::Continue(span.clone()))
}