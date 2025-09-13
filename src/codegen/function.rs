// In src/codegen/function.rs

use super::{CodeGen, CodeGenError};
use crate::analyzer::hir;
use crate::reporter::CodeGenError as CGE;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use std::collections::HashMap;
use std::sync::Arc;

/// [REFACTORED] Pass 2: 生成所有函数的声明
pub(super) fn generate_function_declarations(codegen: &mut CodeGen, program: &hir::Program) {
    // extern "C" 函数
    for func_decl in &program.extern_functions {
        generate_single_decl(codegen, func_decl);
    }
    // CScript 函数
    for func in &program.functions {
        // 从 hir::Function 结构中提取出声明所需的信息
        let func_decl = Arc::new(hir::FunctionDecl {
            name: func.name.clone(),
            params: func.params.iter().map(|p| p.var_type.clone()).collect(),
            return_type: func.return_type.clone(),
            is_variadic: false,
        });
        generate_single_decl(codegen, &func_decl);
    }
}

/// [REFACTORED] Pass 3: 生成所有函数体
pub(super) fn generate_function_bodies(codegen: &mut CodeGen, program: &hir::Program) {
    for func in &program.functions {
        // 实现错误恢复：如果一个函数体生成失败，报告错误并继续处理下一个
        if let Err(err) = generate_single_body(codegen, func) {
            codegen.diagnostics.push(err.into());
        }
    }
}

/// [REFACTORED] 辅助函数：生成单个函数声明
fn generate_single_decl(codegen: &mut CodeGen, func_decl: &hir::FunctionDecl) {
    let param_types: Vec<BasicMetadataTypeEnum> = func_decl
        .params
        .iter()
        .map(|p_type| codegen.to_llvm_type(p_type).into())
        .collect();

    let fn_type = if func_decl.return_type.is_void() {
        codegen.context.void_type().fn_type(&param_types, func_decl.is_variadic)
    } else {
        codegen.to_llvm_type(&func_decl.return_type).fn_type(&param_types, func_decl.is_variadic)
    };

    let function = codegen.module.add_function(&func_decl.name.name, fn_type, None);
    codegen.functions.insert(func_decl.name.name.clone(), function);
}

/// [REFACTORED] 辅助函数：生成单个函数体
fn generate_single_body(codegen: &mut CodeGen, func: &hir::Function) -> Result<(), CodeGenError> {
    // 从全局函数表中获取之前创建的 FunctionValue
    let function = codegen.functions.get(&func.name.name)
        .copied()
        .expect("Function declaration should exist from Pass 2");
    
    // --- [KEY REFACTOR] 状态管理 ---
    // 为当前函数创建全新的、独立的局部状态
    let mut local_variables = HashMap::new();
    let mut loop_stack = Vec::new();

    // 创建一个临时的、包含了对全局状态（只读）和局部状态（可写）引用的上下文
    let mut ctx = codegen.create_fn_ctx(function, &mut local_variables, &mut loop_stack);
    
    // --- 函数体生成开始 ---
    let entry_block = ctx.codegen.context.append_basic_block(function, "entry");
    ctx.codegen.builder.position_at_end(entry_block);
    
    // 为函数参数分配栈空间 (alloca) 并存储初始值
    for (hir_param, llvm_param) in func.params.iter().zip(function.get_param_iter()) {
        let param_name = &hir_param.name.name;
        // [FIX] 通过 ctx.codegen 来调用方法，而不是直接用 codegen
        let llvm_type = ctx.codegen.to_llvm_type(&hir_param.var_type);
        llvm_param.set_name(param_name);

        // create_entry_block_alloca 接收 &ctx，是正确的
        let alloca = super::utils::create_entry_block_alloca(param_name, llvm_type, &ctx)?;
        // builder 的访问也通过 ctx.codegen
        ctx.codegen.builder.build_store(alloca, llvm_param);
        // variables 是 ctx 的局部状态，可以直接访问
        ctx.variables.insert(hir_param.clone(), alloca);
    }

    // 调用 statement.rs 中的逻辑来填充函数体
    super::statement::generate_block_statement(&func.body, &mut ctx)?;

    // 检查函数的最后一个基本块是否正确地“终止”
    // 如果一个有返回值的函数所有执行路径都没有 return，analyzer 应该已经报错。
    // 在 codegen 阶段，我们插入一条 unreachable 指令来保证 LLVM IR 的合法性。
    if let Some(last_block) = ctx.codegen.builder.get_insert_block() {
        if last_block.get_terminator().is_none() {
            if func.return_type.is_void() {
                ctx.codegen.builder.build_return(None);
            } else {
                ctx.codegen.builder.build_unreachable();
            }
        }
    }

    // 验证单个函数（在开发阶段非常有用）
    if !function.verify(true) {
        return Err(CGE::InternalError {
            message: format!("Function '{}' failed LLVM verification.", func.name.name),
            span: func.name.span.into(),
        });
    }

    Ok(())
}