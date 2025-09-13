// In src/codegen/statement.rs

use super::{CodeGenCtx, GenerateHIR, CodeGenError};
use crate::analyzer::hir;
use crate::codegen::utils;
use std::sync::Arc;

/// [REFACTORED] 为 hir::Statement 实现 GenerateHIR trait
impl<'a, 'ctx> GenerateHIR<'a, 'ctx> for hir::Statement {
    type Output = ();

    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<Self::Output, CodeGenError> {
        match self {
            hir::Statement::VarDecl(var_decl) => generate_var_decl(var_decl, ctx)?,
            hir::Statement::Expr(expr) => generate_expr_statement(expr, ctx)?,
            hir::Statement::Return { value, .. } => generate_return_statement(value.as_ref(), ctx)?,
            hir::Statement::Block(block) => generate_block_statement(block, ctx)?,
            hir::Statement::If { condition, then_branch, else_branch, .. } => {
                generate_if_statement(condition, then_branch, else_branch.as_ref(), ctx)?
            }
            hir::Statement::While { condition, body, .. } => {
                generate_while_statement(condition, body, ctx)?
            }
            hir::Statement::Break(_) => generate_break_statement(ctx)?,
            hir::Statement::Continue(_) => generate_continue_statement(ctx)?,
        }
        Ok(())
    }
}

// === 辅助函数 (已全部升级为返回 Result) ===

fn generate_var_decl<'a, 'ctx>(
    var_decl: &Arc<hir::VarDecl>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    let var_name = &var_decl.name.name;
    let llvm_type = ctx.codegen.to_llvm_type(&var_decl.var_type);

    let alloca = utils::create_entry_block_alloca(var_name, llvm_type, ctx)?;
    
    // 将变量及其在栈上的地址存入当前函数的局部变量表中
    ctx.variables.insert(var_decl.clone(), alloca);

    if let Some(initializer) = &var_decl.initializer {
        // 调用 expression.rs 中的逻辑来生成初始值
        let init_val = initializer.generate(ctx)?;
        ctx.codegen.builder.build_store(alloca, init_val);
    }
    
    Ok(())
}

fn generate_expr_statement<'a, 'ctx>(
    expr: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    // 为表达式生成代码，但忽略其结果值，只保留其副作用
    expr.generate(ctx)?;
    Ok(())
}

fn generate_return_statement<'a, 'ctx>(
    value: Option<&hir::Expression>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    if let Some(expr) = value {
        let return_val = expr.generate(ctx)?;
        ctx.codegen.builder.build_return(Some(&return_val));
    } else {
        ctx.codegen.builder.build_return(None);
    }
    Ok(())
}

pub(super) fn generate_block_statement<'a, 'ctx>(
    block: &hir::Block,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    for stmt in &block.stmts {
        stmt.generate(ctx)?;
    }
    Ok(())
}

fn generate_if_statement<'a, 'ctx>(
    condition: &hir::Expression,
    then_branch: &hir::Block,
    else_branch: Option<&hir::Block>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    let function = ctx.current_function.expect("Internal Error: current_function was None inside a statement generator.");
    
    let then_bb = ctx.codegen.context.append_basic_block(function, "then");
    let merge_bb = ctx.codegen.context.append_basic_block(function, "merge");
    let else_bb = else_branch.map_or(merge_bb, |_| ctx.codegen.context.append_basic_block(function, "else"));
    
    let cond_val = condition.generate(ctx)?.into_int_value();
    ctx.codegen.builder.build_conditional_branch(cond_val, then_bb, else_bb);

    // 填充 then 块
    ctx.codegen.builder.position_at_end(then_bb);
    generate_block_statement(then_branch, ctx)?;
    // 如果块没有自己的终止指令（如 return），则必须跳转到 merge 块
    if then_bb.get_terminator().is_none() {
        ctx.codegen.builder.build_unconditional_branch(merge_bb);
    }

    // 填充 else 块
    if let Some(else_b) = else_branch {
        // else_bb 在 map_or 中被正确创建，这里可以直接使用
        ctx.codegen.builder.position_at_end(else_bb);
        generate_block_statement(else_b, ctx)?;
        if else_bb.get_terminator().is_none() {
            ctx.codegen.builder.build_unconditional_branch(merge_bb);
        }
    }

    ctx.codegen.builder.position_at_end(merge_bb);
    Ok(())
}

fn generate_while_statement<'a, 'ctx>(
    condition: &hir::Expression,
    body: &hir::Block,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    let function = ctx.current_function.expect("Internal Error: current_function was None inside a statement generator.");
    
    let cond_bb = ctx.codegen.context.append_basic_block(function, "loop_cond");
    let body_bb = ctx.codegen.context.append_basic_block(function, "loop_body");
    let after_bb = ctx.codegen.context.append_basic_block(function, "after_loop");
    
    ctx.loop_stack.push((cond_bb, after_bb));
    ctx.codegen.builder.build_unconditional_branch(cond_bb);

    // 填充条件判断块
    ctx.codegen.builder.position_at_end(cond_bb);
    let cond_val = condition.generate(ctx)?.into_int_value();
    ctx.codegen.builder.build_conditional_branch(cond_val, body_bb, after_bb);

    // 填充循环体块
    ctx.codegen.builder.position_at_end(body_bb);
    generate_block_statement(body, ctx)?;
    // 循环体结束后必须跳回条件判断块
    if body_bb.get_terminator().is_none() {
        ctx.codegen.builder.build_unconditional_branch(cond_bb);
    }
    
    ctx.codegen.builder.position_at_end(after_bb);
    ctx.loop_stack.pop();
    Ok(())
}

fn generate_break_statement<'a, 'ctx>(ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<(), CodeGenError> {
    let break_bb = ctx.loop_stack.last().map(|(_, break_bb)| *break_bb)
        .expect("Analyzer should have caught break outside loop");
    ctx.codegen.builder.build_unconditional_branch(break_bb);
    Ok(())
}

fn generate_continue_statement<'a, 'ctx>(ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<(), CodeGenError> {
    let continue_bb = ctx.loop_stack.last().map(|(continue_bb, _)| *continue_bb)
        .expect("Analyzer should have caught continue outside loop");
    ctx.codegen.builder.build_unconditional_branch(continue_bb);
    Ok(())
}