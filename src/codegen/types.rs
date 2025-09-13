// In src/codegen/types.rs

use super::{CodeGen, hir};
use crate::analyzer::types::SemanticType;

/// Pass 0: 遍历整个 HIR，预先在 LLVM 模块中定义好所有的结构体类型。
/// 这可以解决递归结构体（如链表）的问题，并简化后续 `to_llvm_type` 的逻辑。
pub(super) fn pass0_define_struct_types(codegen: &mut CodeGen, program: &hir::Program) {
    // 第一次遍历：为所有结构体创建不透明（opaque）类型定义。
    // 这允许结构体之间相互引用。
    for global in &program.globals {
        if let SemanticType::Struct { name, .. } = &global.var_type {
            if !codegen.struct_types.contains_key(&name.name) {
                let struct_type = codegen.context.opaque_struct_type(&name.name);
                codegen.struct_types.insert(name.name.clone(), struct_type);
            }
        }
    }
    //（也可以遍历函数参数和局部变量来查找更多结构体类型）

    // 第二次遍历：为所有已创建的不透明结构体设置它们的 body。
    for global in &program.globals {
        if let SemanticType::Struct { name, fields, .. } = &global.var_type {
            if let Some(struct_type) = codegen.struct_types.get(&name.name) {
                // 如果 body 尚未设置
                if struct_type.is_opaque() {
                    let field_types = fields
                        .iter()
                        .map(|(_, ty)| codegen.to_llvm_type(ty))
                        .collect::<Vec<_>>();
                    struct_type.set_body(&field_types, false);
                }
            }
        }
    }
}