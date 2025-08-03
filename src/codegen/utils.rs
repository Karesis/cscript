// In src/codegen/utils.rs

use super::{CodeGenCtx, CodeGenError}; // 从父模块导入上下文
use crate::analyzer::types::SemanticType;
use crate::diagnostics::DiagnosticBag;
use crate::parser::ast::BinaryOp;
use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::{PointerValue, FunctionValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

// [MOVED & REFACTORED]
// 不再是 CodeGen 的方法，而是一个独立的函数。
// 它所需的所有状态都通过 CodeGenCtx 传入。
pub(super) fn to_llvm_type<'ctx>(
    ty: &SemanticType,
    ctx: &mut CodeGenCtx<'_, 'ctx>,
) -> BasicTypeEnum<'ctx> {
    match ty {
        SemanticType::Integer { width, .. } => ctx.context.custom_width_int_type(*width as u32).into(),
        SemanticType::Float { width } => match width {
            32 => ctx.context.f32_type().into(),
            64 => ctx.context.f64_type().into(),
            _ => unreachable!(),
        },
        SemanticType::Bool => ctx.context.bool_type().into(),
        SemanticType::Char => ctx.context.i8_type().into(),
        SemanticType::Struct { name, fields, .. } => {
            // 注意这里现在用的是 ctx.struct_types
            if let Some(struct_type) = ctx.struct_types.get(&name.name) {
                return (*struct_type).into();
            }
            let struct_type = ctx.context.opaque_struct_type(&name.name);
            // 这里需要可变访问，所以 ctx.struct_types 必须是 &mut HashMap
            ctx.struct_types.insert(name.name.clone(), struct_type);
            
            // 递归调用来解析字段类型
            let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                .iter()
                .map(|(_, field_type)| to_llvm_type(field_type, ctx))
                .collect();
                
            struct_type.set_body(&field_types, false);
            struct_type.into()
        }
        SemanticType::Ptr(_) => ctx.context.ptr_type(AddressSpace::default()).into(),
        _ => unimplemented!("LLVM type for {:?} is not a basic type", ty),
    }
}

// [MOVED & REFACTORED]
pub(super) fn create_entry_block_alloca<'ctx, T: BasicType<'ctx>>(
    name: &str,
    ty: T,
    ctx: &mut CodeGenCtx<'_, 'ctx>,
) -> Option<PointerValue<'ctx>> {
    let builder = ctx.context.create_builder();
    let entry = ctx.current_function.unwrap().get_first_basic_block().unwrap();
    match entry.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry),
    }
    match builder.build_alloca(ty, name) {
        Ok(ptr) => Some(ptr),
        Err(e) => {
            ctx.diagnostics.report(CodeGenError::InternalError { message: e.to_string(), span: None }.into());
            None
        }
    }
}

// [MOVED & REFACTORED]
// 这是一个纯函数，不需要 CodeGenCtx
pub(super) fn int_predicate(op: &BinaryOp, is_signed: bool) -> IntPredicate {
    match op {
        BinaryOp::Eq => IntPredicate::EQ,
        BinaryOp::NotEq => IntPredicate::NE,
        BinaryOp::Gt => if is_signed { IntPredicate::SGT } else { IntPredicate::UGT },
        BinaryOp::Gte => if is_signed { IntPredicate::SGE } else { IntPredicate::UGE },
        BinaryOp::Lt => if is_signed { IntPredicate::SLT } else { IntPredicate::ULT },
        BinaryOp::Lte => if is_signed { IntPredicate::SLE } else { IntPredicate::ULE },
        _ => unreachable!(),
    }
}

// [MOVED & REFACTORED]
// 这也是一个纯函数
pub(super) fn float_predicate(op: &BinaryOp) -> FloatPredicate {
    match op {
        BinaryOp::Eq => FloatPredicate::OEQ,
        BinaryOp::NotEq => FloatPredicate::ONE,
        BinaryOp::Gt => FloatPredicate::OGT,
        BinaryOp::Gte => FloatPredicate::OGE,
        BinaryOp::Lt => FloatPredicate::OLT,
        BinaryOp::Lte => FloatPredicate::OLE,
        _ => unreachable!(),
    }
}