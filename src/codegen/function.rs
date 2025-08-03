// In src/codegen/function.rs

use super::{CodeGenCtx, GenerateHIR};
use crate::analyzer::{hir, types::SemanticType};
use inkwell::types::BasicMetadataTypeEnum;
use std::sync::Arc;
use inkwell::types::BasicType;
use super::statement::generate_block_statement;

// [NEW] PASS 1: 生成所有函数的声明
pub(super) fn generate_function_declarations<'a, 'ctx>(
    program: &hir::Program,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    // extern "C" 函数
    for func_decl in &program.extern_functions {
        generate_single_decl(func_decl, ctx);
    }
    // CScript 函数
    for func in &program.functions {
        let temp_decl = hir::FunctionDecl {
            name: func.name.clone(),
            params: func.params.iter().map(|p| p.var_type.clone()).collect(),
            return_type: func.return_type.clone(),
            is_variadic: false,
        };
        generate_single_decl(&temp_decl, ctx);
    }
    Some(())
}

// [NEW] PASS 2: 生成所有函数体
pub(super) fn generate_function_bodies<'a, 'ctx>(
    program: &hir::Program,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    for func in &program.functions {
        generate_single_body(func, ctx)?;
    }
    Some(())
}

// [MOVED] 从旧 CodeGen 中移动并改造
fn generate_single_decl(func: &hir::FunctionDecl, ctx: &mut CodeGenCtx) {
    let param_types: Vec<BasicMetadataTypeEnum> = func
        .params
        .iter()
        .map(|p_type| super::utils::to_llvm_type(p_type, ctx).into())
        .collect();

    let fn_type = match &func.return_type {
        SemanticType::Void => ctx.context.void_type().fn_type(&param_types, func.is_variadic),
        _ => super::utils::to_llvm_type(&func.return_type, ctx).fn_type(&param_types, func.is_variadic),
    };

    let function = ctx.module.add_function(&func.name.name, fn_type, None);
    ctx.functions.insert(func.name.name.clone(), function);
}

// [MOVED & REFACTORED]
fn generate_single_body<'a, 'ctx>(func: &hir::Function, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<()> {
    let function = ctx.functions.get(&func.name.name).unwrap();
    
    // 设置当前函数上下文
    *ctx.current_function = Some(*function);

    let entry_block = ctx.context.append_basic_block(*function, "entry");
    ctx.builder.position_at_end(entry_block);
    
    // 清空旧函数的局部变量，为新函数做准备
    ctx.variables.clear();

    // 为函数参数分配栈空间并存储初始值
    for (hir_param, llvm_param) in func.params.iter().zip(function.get_param_iter()) {
        let param_name = &hir_param.name.name;
        let llvm_type = super::utils::to_llvm_type(&hir_param.var_type, ctx);
        llvm_param.set_name(param_name);

        let alloca = super::utils::create_entry_block_alloca(param_name, llvm_type, ctx)?;
        ctx.builder.build_store(alloca, llvm_param);
        ctx.variables.insert(hir_param.clone(), alloca);
    }

    // *** 关键连接点 ***
    // 此处调用我们已经重构好的语句生成逻辑，来填充函数体！
    generate_block_statement(&func.body, ctx)?;

    // 确保函数最后有终结符 (terminator)
    let last_block = ctx.builder.get_insert_block().unwrap();
    if last_block.get_terminator().is_none() {
        if func.return_type == SemanticType::Void {
            ctx.builder.build_return(None);
        } else {
            // 如果一个有返回值的函数走到了最后却没有 return，这本身是一个逻辑错误。
            // 在 LLVM IR 中，我们将其标记为不可到达，这有助于优化。
            ctx.builder.build_unreachable();
        }
    }
    Some(())
}