// In src/codegen/lvalue.rs

use super::{CodeGenCtx, GenerateHIR, CodeGenError};
use crate::analyzer::hir;
use crate::codegen::utils;
use crate::reporter::CodeGenError as CGE;
use inkwell::values::{PointerValue};
use std::sync::Arc;


/// [REFACTORED] 定义一个专门用于生成左值（内存地址）的 Trait。
/// 错误处理已升级为 Result。
pub(super) trait GenerateLValue<'a, 'ctx> {
    type Output;
    fn generate_lvalue(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<Self::Output, CodeGenError>;
}

/// [REFACTORED] 为 hir::Expression 实现这个 Trait。
impl<'a, 'ctx> GenerateLValue<'a, 'ctx> for hir::Expression {
    type Output = PointerValue<'ctx>;

    fn generate_lvalue(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<Self::Output, CodeGenError> {
        match &self.kind {
            hir::ExprKind::Variable(var_decl) => {
                // 1. 首先在当前函数的局部变量/参数中查找
                if let Some(ptr) = ctx.variables.get(var_decl) {
                    return Ok(*ptr);
                }
                // 2. 如果找不到，再去全局变量中查找
                if let Some(global) = ctx.codegen.global_variables.get(var_decl) {
                    return Ok(global.as_pointer_value());
                }
                // 3. 如果都找不到，这是一个内部错误。
                //    analyzer 阶段应该保证 HIR 中的所有变量都是已声明的。
                unreachable!("Variable '{}' pointer not found in codegen", var_decl.name.name);
            }

            hir::ExprKind::Dereference(ptr_expr) => {
                // 对一个指针表达式进行解引用，其“地址”就是这个指针表达式的“值”。
                // 例如，对于 `*p`，它的地址就是 `p` 的值。
                let ptr_val = ptr_expr.generate(ctx)?;
                Ok(ptr_val.into_pointer_value())
            }

            hir::ExprKind::MemberAccess { expression, member } => {
                // 递归地获取结构体实例的地址
                let struct_ptr = expression.generate_lvalue(ctx)?;
                
                if let crate::analyzer::types::SemanticType::Struct { fields, .. } = &expression.resolved_type {
                    // analyzer 保证了字段一定存在，所以这里用 expect 是安全的。
                    let field_index = fields
                        .iter()
                        .position(|(field_ident, _)| field_ident.name == member.name)
                        .expect("Field not found in struct during codegen, analyzer should have caught this.");
                    
                    let llvm_struct_type = ctx.codegen.to_llvm_type(&expression.resolved_type).into_struct_type();
                    
                    // [FIXED] 使用 map_err 替代 .ok() 来正确传播 GEP 可能发生的错误
                    ctx.codegen.builder.build_struct_gep(
                        llvm_struct_type,
                        struct_ptr,
                        field_index as u32,
                        &member.name,
                    ).map_err(|e| CGE::InternalError {
                        message: format!("Failed to build GEP for field '{}': {}", member.name, e),
                        span: member.span.into(),
                    })
                } else {
                    unreachable!("Member access on non-struct type should be caught by analyzer");
                }
            }
            
            // 其他所有表达式类型都不能作为左值
            _ => unreachable!("This expression kind does not produce an l-value, analyzer should have caught this."),
        }
    }
}