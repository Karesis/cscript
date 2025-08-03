// In src/codegen/lvalue.rs

use super::{CodeGenCtx, GenerateHIR}; // 从父模块导入上下文和主 trait
use crate::analyzer::hir;
use crate::codegen::utils;
use inkwell::values::{PointerValue, BasicValueEnum};


// [NEW] 定义一个专门用于生成左值（内存地址）的 Trait
pub(super) trait GenerateLValue<'a, 'ctx> {
    type Output;

    fn generate_lvalue(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<Self::Output>;
}

// [NEW] 为 hir::Expression 实现这个 Trait
// 旧的 visit_lvalue 函数的逻辑将被迁移到这里
impl<'a, 'ctx> GenerateLValue<'a, 'ctx> for hir::Expression {
    type Output = PointerValue<'ctx>;

    fn generate_lvalue(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<Self::Output> {
        match &self.kind {
            hir::ExprKind::Variable(var_decl) => {
                // 1. 首先在局部变量/参数中查找
                if let Some(ptr) = ctx.variables.get(var_decl) {
                    return Some(*ptr);
                }
                // 2. 如果找不到，再去全局变量中查找
                if let Some(global) = ctx.global_variables.get(var_decl) {
                    return Some(global.as_pointer_value());
                }
                // 3. 如果都找不到，这是一个内部错误
                unreachable!("Variable '{}' pointer not found", var_decl.name.name);
            }

            hir::ExprKind::Dereference(ptr_expr) => {
                // 对一个指针表达式进行解引用，其左值就是这个指针本身的值
                ptr_expr.generate(ctx).map(|v| v.into_pointer_value())
            }

            hir::ExprKind::MemberAccess { expression, member } => {
                // 递归地获取结构体的地址
                let struct_ptr = expression.generate_lvalue(ctx)?;
                
                if let crate::analyzer::types::SemanticType::Struct { fields, .. } = &expression.resolved_type {
                    let field_index = fields
                        .iter()
                        .position(|(field_ident, _)| field_ident.name == member.name)
                        .unwrap();
                    
                    let llvm_struct_type = utils::to_llvm_type(&expression.resolved_type, ctx);
                    
                    ctx.builder.build_struct_gep(
                        llvm_struct_type,
                        struct_ptr,
                        field_index as u32,
                        &member.name,
                    ).ok()
                } else {
                    unreachable!("Member access on non-struct type in codegen l-value");
                }
            }
            
            _ => unreachable!("This expression kind does not produce an l-value."),
        }
    }
}