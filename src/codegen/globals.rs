// In src/codegen/globals.rs

use super::{CodeGenCtx, CodeGenError};
use crate::analyzer::{hir, types::SemanticType};
use inkwell::values::{BasicValueEnum, GlobalValue};
use inkwell::{AddressSpace, module::Linkage};
use std::sync::Arc;

// [NEW] 顶层函数，负责遍历并生成所有全局变量
pub(super) fn generate_globals<'a, 'ctx>(
    program: &hir::Program,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    for global_var in &program.globals {
        generate_single_global(global_var, ctx)?;
    }
    Some(())
}

// [MOVED] 从旧 CodeGen 中移动并改造的函数
fn generate_single_global<'a, 'ctx>(
    var_decl: &Arc<hir::VarDecl>,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<()> {
    let llvm_type = super::utils::to_llvm_type(&var_decl.var_type, ctx);
    let global = ctx.module.add_global(
        llvm_type, 
        Some(AddressSpace::default()), 
        &var_decl.name.name
    );
    
    if var_decl.is_const {
        global.set_linkage(Linkage::Private);
        global.set_constant(true);
    } else {
        global.set_linkage(Linkage::External);
        global.set_constant(false);
    }

    if let Some(initializer) = &var_decl.initializer {
        let init_val = generate_const_expression(initializer, ctx)?;
        global.set_initializer(&init_val);
    } else {
        global.set_initializer(&llvm_type.const_zero());
    }

    // 将新创建的全局变量指针存入上下文，以便后续使用
    ctx.global_variables.insert(var_decl.clone(), global);
    Some(())
}

// [MOVED] 从旧 CodeGen 中移动并改造的常量表达式求值函数
fn generate_const_expression<'a, 'ctx>(
    expr: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    match &expr.kind {
        hir::ExprKind::Literal(literal) => match literal {
            hir::LiteralValue::Float(val) => {
                let float_type = super::utils::to_llvm_type(&expr.resolved_type, ctx).into_float_type();
                Some(float_type.const_float(*val).into())
            }
            hir::LiteralValue::Integer(val) => {
                let int_type = super::utils::to_llvm_type(&expr.resolved_type, ctx).into_int_type();
                Some(int_type.const_int(*val as u64, true).into())
            }
            hir::LiteralValue::Bool(b) => Some(ctx.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
            _ => {
                // 此处省略了错误报告，实际项目中应添加
                None
            }
        },
        _ => {
            // 此处省略了错误报告
            None
        }
    }
}