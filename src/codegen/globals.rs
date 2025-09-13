// In src/codegen/globals.rs

use super::{CodeGen, CodeGenCtx, CodeGenError};
use crate::analyzer::hir;
use crate::reporter::CodeGenError as CGE; // 为 CodeGenError 创建一个短别名
use inkwell::values::{BasicValueEnum, AsValueRef};
use inkwell::{AddressSpace, module::Linkage};
use std::sync::Arc;
use std::collections::HashMap;

/// [REFACTORED] Pass 函数，负责遍历并生成所有全局变量。
/// 它现在接收 `&mut CodeGen`，并将错误推入其中，而不是返回 Option。
pub(super) fn generate_globals(codegen: &mut CodeGen, program: &hir::Program) {
    // 为全局 Pass 创建临时的空状态
    let mut empty_vars = HashMap::new();
    let mut empty_loops = Vec::new();

    for global_var in &program.globals {
        // 创建一个临时的上下文用于单个全局变量的生成
        let mut ctx = codegen.create_global_ctx(&mut empty_vars, &mut empty_loops);

        // 实现错误恢复：如果单个全局变量生成失败，则报告错误并继续处理下一个。
        if let Err(err) = generate_single_global(global_var, &mut ctx) {
            codegen.diagnostics.push(err.into());
        }
    }
}

/// [REFACTORED] 生成单个全局变量。
/// 现在返回 Result 以便进行错误传播。
fn generate_single_global<'a, 'ctx>(
    var_decl: &Arc<hir::VarDecl>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<(), CodeGenError> {
    // to_llvm_type 可能会在 utils.rs 中定义
    let llvm_type = ctx.codegen.to_llvm_type(&var_decl.var_type);
    let global = ctx.codegen.module.add_global(llvm_type, Some(AddressSpace::default()), &var_decl.name.name);

    if var_decl.is_const {
        global.set_constant(true);
        // 常量通常是模块内部的，使用 Private Linkage
        global.set_linkage(Linkage::Private);
    } else {
        global.set_constant(false);
        // 非常量全局变量需要对其他模块可见
        global.set_linkage(Linkage::External);
    }

    if let Some(initializer) = &var_decl.initializer {
        // [MODIFIED] 使用 `?` 来传播来自常量表达式求值的错误
        let init_val = generate_const_expression(initializer, ctx)?;
        global.set_initializer(&init_val);
    } else {
        // 如果没有初始化器，则默认初始化为 0
        global.set_initializer(&llvm_type.const_zero());
    }

    // 将新创建的全局变量指针存入 CodeGen 的状态中，以便后续使用
    ctx.codegen.global_variables.insert(var_decl.clone(), global);
    Ok(())
}

/// [REFACTORED] 递归地对常量表达式求值。
/// 现在返回 Result，并且能够处理非法的常量表达式。
fn generate_const_expression<'a, 'ctx>(
    expr: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
    match &expr.kind {
        hir::ExprKind::Literal(literal) => match literal {
            hir::LiteralValue::Float(val) => {
                let float_type = ctx.codegen.to_llvm_type(&expr.resolved_type).into_float_type();
                Ok(float_type.const_float(*val).into())
            }
            hir::LiteralValue::Integer(val) => {
                let int_type = ctx.codegen.to_llvm_type(&expr.resolved_type).into_int_type();
                // [FIXED] 正确处理 HirIntValue 枚举
                let const_int = match val {
                    hir::HirIntValue::Signed(s_val) => int_type.const_int(*s_val as u64, true),
                    hir::HirIntValue::Unsigned(u_val) => int_type.const_int(*u_val as u64, false),
                };
                Ok(const_int.into())
            }
            hir::LiteralValue::Bool(b) => {
                let const_bool = ctx.codegen.context.bool_type().const_int(if *b { 1 } else { 0 }, false);
                Ok(const_bool.into())
            }
            hir::LiteralValue::String(_) => {
                // TODO: 实现全局字符串常量的创建
                Err(CGE::InternalError {
                    message: "Global string initializers are not yet implemented.".to_string(),
                    span: expr.span.into(),
                })
            }
        },
        // 任何非字面量的表达式都不是常量，直接报错
        _ => Err(CGE::NonConstantGlobalInitializer { span: expr.span.into() }),
    }
}