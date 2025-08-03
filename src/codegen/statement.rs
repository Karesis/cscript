// In src/codegen/statement.rs

use super::{CodeGenCtx, GenerateHIR}; // 从父模块导入核心抽象
use crate::analyzer::hir;
use crate::codegen::utils;


// [NEW] 为 hir::Statement 实现 GenerateHIR trait
// 它将成为新的“语句分发器”
impl<'a, 'ctx> GenerateHIR<'a, 'ctx> for hir::Statement {
    // 语句没有返回值，所以 Output 类型是 ()
    type Output = ();

    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<Self::Output> {
        match self {
            hir::Statement::VarDecl(var_decl) => {
                generate_var_decl(var_decl, ctx)
            }
            hir::Statement::Expr(expr) => {
                generate_expr_statement(expr, ctx)
            }
            hir::Statement::Return { value, .. } => {
                generate_return_statement(value.as_ref(), ctx)
            }
            hir::Statement::Block(block) => {
                generate_block_statement(block, ctx)
            }
            hir::Statement::If { condition, then_branch, else_branch, .. } => {
                generate_if_statement(condition, then_branch, else_branch.as_ref(), ctx)
            }
            hir::Statement::While { condition, body, .. } => {
                generate_while_statement(condition, body, ctx)
            }
            hir::Statement::Break(_) => generate_break_statement(ctx),
            hir::Statement::Continue(_) => generate_continue_statement(ctx),
            
            _ => todo!("Codegen for this statement kind is not yet implemented!"),
        }
    }
}

// [NEW] 专门处理变量声明语句的辅助函数
fn generate_var_decl<'a, 'ctx>(
    var_decl: &hir::VarDecl,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    // 这段逻辑是从旧的 visit_statement 中复制和适配过来的
    let var_name = &var_decl.name.name;
    let llvm_type = utils::to_llvm_type(&var_decl.var_type, ctx);

    // 1. 在函数入口为变量分配栈空间
    let alloca = utils::create_entry_block_alloca(var_name, llvm_type, ctx)?;
    
    // 2. 将变量名和其在栈上的地址（指针）记录到上下文中
    ctx.variables.insert(var_decl.clone().into(), alloca);

    // 3. 如果有初始化表达式...
    if let Some(initializer) = &var_decl.initializer {
        // ...就生成初始化表达式的值。
        // 请注意这一步：我们正在调用刚刚重构好的表达式生成逻辑！
        // 新的语句生成器无缝地使用了新的表达式生成器，这就是分层架构的威力。
        let init_val = initializer.generate(ctx)?;
        
        // 将生成的值存入为变量分配的内存中
        ctx.builder.build_store(alloca, init_val);
    }
    
    Some(())
}

// [ADD] 专门处理表达式语句的辅助函数
fn generate_expr_statement<'a, 'ctx>(
    expr: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    // 非常简单：我们只需要为表达式生成代码以产生其副作用，
    // 然后忽略它的返回值即可。
    expr.generate(ctx)?;
    Some(())
}

// [ADD] 专门处理返回语句的辅助函数
fn generate_return_statement<'a, 'ctx>(
    value: Option<&hir::Expression>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    if let Some(expr) = value {
        // 如果有返回值，先为它生成代码...
        let return_val = expr.generate(ctx)?;
        // ...然后构建带值的 return 指令。
        ctx.builder.build_return(Some(&return_val));
    } else {
        // 如果没有返回值 (void)，则构建不带值的 return 指令。
        ctx.builder.build_return(None);
    }
    Some(())
}

// [ADD] 专门处理代码块语句的辅助函数
pub(super) fn generate_block_statement<'a, 'ctx>(
    block: &hir::Block,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    // 这里的逻辑也很清晰：遍历代码块中的每一条语句...
    for stmt in &block.stmts {
        // ...然后递归地调用 .generate() 来为它们生成代码。
        // 这取代了旧的 visit_block 函数。
        stmt.generate(ctx)?;
    }
    Some(())
}

// [ADD] 专门处理 If 语句的辅助函数
fn generate_if_statement<'a, 'ctx>(
    condition: &hir::Expression,
    then_branch: &hir::Block,
    else_branch: Option<&hir::Block>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    let function = ctx.current_function.unwrap();
    
    // 1. 为 then, else, merge 创建基本块
    let then_bb = ctx.context.append_basic_block(function, "then");
    let merge_bb = ctx.context.append_basic_block(function, "merge");
    // 如果有 else 分支，就创建 else 块；否则，条件为假时直接跳转到 merge 块
    let else_bb = else_branch.map(|_| ctx.context.append_basic_block(function, "else")).unwrap_or(merge_bb);
    
    // 2. 生成条件判断，并创建条件跳转指令
    let cond_val = condition.generate(ctx)?.into_int_value();
    ctx.builder.build_conditional_branch(cond_val, then_bb, else_bb);

    // 3. 填充 then 块
    ctx.builder.position_at_end(then_bb);
    generate_block_statement(then_branch, ctx)?; // 递归调用 generate 来填充块
    if then_bb.get_terminator().is_none() { // 如果块没有 return，就跳转到 merge
        ctx.builder.build_unconditional_branch(merge_bb);
    }

    // 4. 如果存在，填充 else 块
    if let Some(else_b) = else_branch {
        ctx.builder.position_at_end(else_bb);
        generate_block_statement(else_b, ctx)?; // 递归调用
        if else_bb.get_terminator().is_none() {
            ctx.builder.build_unconditional_branch(merge_bb);
        }
    }

    // 5. 将光标定位到 merge 块，继续后续代码生成
    ctx.builder.position_at_end(merge_bb);
    Some(())
}

// [ADD] 专门处理 While 语句的辅助函数
fn generate_while_statement<'a, 'ctx>(
    condition: &hir::Expression,
    body: &hir::Block,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    let function = ctx.current_function.unwrap();
    
    // 1. 为 loop_condition, loop_body, after_loop 创建基本块
    let cond_bb = ctx.context.append_basic_block(function, "loop_cond");
    let body_bb = ctx.context.append_basic_block(function, "loop_body");
    let after_bb = ctx.context.append_basic_block(function, "after_loop");
    
    // 2. 将当前循环的 continue(cond) 和 break(after) 目标压入栈中
    ctx.loop_stack.push((cond_bb, after_bb));
    
    // 3. 跳转到循环条件判断块
    ctx.builder.build_unconditional_branch(cond_bb);

    // 4. 填充条件判断块
    ctx.builder.position_at_end(cond_bb);
    let cond_val = condition.generate(ctx)?.into_int_value();
    ctx.builder.build_conditional_branch(cond_val, body_bb, after_bb);

    // 5. 填充循环体块
    ctx.builder.position_at_end(body_bb);
    generate_block_statement(body, ctx)?;
    ctx.builder.build_unconditional_branch(cond_bb); // 循环体结束后跳回条件判断

    // 6. 将光标定位到循环结束后的块
    ctx.builder.position_at_end(after_bb);
    // 7. 循环结束，将此循环的上下文从栈中弹出
    ctx.loop_stack.pop();
    Some(())
}

// [ADD] 专门处理 Break 语句的辅助函数
fn generate_break_statement<'a, 'ctx>(ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<()> {
    // 从循环栈顶获取 break 应该跳转的目标块
    if let Some((_, break_bb)) = ctx.loop_stack.last() {
        ctx.builder.build_unconditional_branch(*break_bb);
    }
    // 如果栈是空的，意味着 break 不在循环内，这应该在语义分析阶段被捕捉到
    Some(())
}

// [ADD] 专门处理 Continue 语句的辅助函数
fn generate_continue_statement<'a, 'ctx>(ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<()> {
    // 从循环栈顶获取 continue 应该跳转的目标块
    if let Some((continue_bb, _)) = ctx.loop_stack.last() {
        ctx.builder.build_unconditional_branch(*continue_bb);
    }
    Some(())
}