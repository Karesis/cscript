// In src/codegen/utils.rs

use super::{CodeGenCtx, CodeGenError};
use crate::analyzer::types::SemanticType;
use crate::parser::ast::BinaryOp;
use crate::reporter::CodeGenError as CGE;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::PointerValue;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};

/// [REFACTORED] 在函数入口块的开头创建一个 alloca 指令。
/// 这是 LLVM 的推荐做法，有利于 mem2reg 等优化。
pub(super) fn create_entry_block_alloca<'ctx, T: BasicType<'ctx>>(
    name: &str,
    ty: T,
    ctx: &CodeGenCtx<'_, 'ctx>,
) -> Result<PointerValue<'ctx>, CodeGenError> {
    // 创建一个临时的 builder，以防干扰主 builder 的位置
    let builder = ctx.codegen.context.create_builder();
    let entry_block = ctx
        .current_function
        .expect("ICE: Attempted to create an alloca when not inside a function context.")
        .get_first_basic_block()
        .expect("ICE: The current function has no entry block. A bug in function body generation logic.");

    match entry_block.get_first_instruction() {
        Some(first_instr) => builder.position_before(&first_instr),
        None => builder.position_at_end(entry_block),
    }

    builder.build_alloca(ty, name).map_err(|e| CGE::InternalError {
        message: format!("Failed to build alloca for '{}': {}", name, e),
        span: (0..0).into(), // alloca 失败通常没有源位置信息
    })
}

/// [UNCHANGED] 将二元操作符转换为 LLVM 的整数比较谓词。
/// 这是一个纯函数，无需修改。
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

/// [UNCHANGED] 将二元操作符转换为 LLVM 的浮点数比较谓词。
/// 这也是一个纯函数，无需修改。
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