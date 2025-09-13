// In src/codegen/expression.rs

use super::{CodeGenCtx, GenerateHIR, lvalue::GenerateLValue};
use crate::analyzer::{hir, types::SemanticType};
use crate::codegen::utils;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::reporter::CodeGenError as CGE;
use inkwell::values::{BasicValue, BasicValueEnum, BasicMetadataValueEnum};
use inkwell::IntPredicate;

/// [REFACTORED] 为 hir::Expression 实现 GenerateHIR trait
impl<'a, 'ctx> GenerateHIR<'a, 'ctx> for hir::Expression {
    type Output = BasicValueEnum<'ctx>;

    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<Self::Output, CGE> {
        match &self.kind {
            hir::ExprKind::Literal(literal_value) => {
                generate_literal(literal_value, &self.resolved_type, ctx)
            }
            hir::ExprKind::BinaryOp { op, left, right } => {
                generate_binary_op(op, left, right, ctx)
            }
            hir::ExprKind::UnaryOp { op, right } => {
                generate_unary_op(op, right, ctx)
            }
            hir::ExprKind::Variable(_) | hir::ExprKind::MemberAccess {..} => {
                // 对于变量或成员访问，其“值”就是从其内存地址中加载出来的内容
                let ptr = self.generate_lvalue(ctx)?;
                let ty = ctx.codegen.to_llvm_type(&self.resolved_type);
                ctx.codegen.builder.build_load(ty, ptr, "loadtmp").map_err(|e| CGE::InternalError{ message: e.to_string(), span: self.span.into() })
            }
            hir::ExprKind::Assignment { left, right } => {
                let ptr_to_store = left.generate_lvalue(ctx)?;
                let val_to_store = right.generate(ctx)?;
                ctx.codegen.builder.build_store(ptr_to_store, val_to_store);
                // 赋值表达式本身的值就是右侧的值
                Ok(val_to_store)
            }
            hir::ExprKind::AddressOf(expr_to_addr) => {
                // 取地址表达式的 "值" 就是其操作数的地址 (L-value)
                Ok(expr_to_addr.generate_lvalue(ctx)?.as_basic_value_enum())
            }
            hir::ExprKind::Dereference(ptr_expr) => {
                let ptr_val = ptr_expr.generate(ctx)?.into_pointer_value();
                let ty = ctx.codegen.to_llvm_type(&self.resolved_type);
                ctx.codegen.builder.build_load(ty, ptr_val, "deref").map_err(|e| CGE::InternalError{ message: e.to_string(), span: self.span.into() })
            }
            hir::ExprKind::FunctionCall { name, args } => {
                generate_function_call(name, args, ctx)
            }
            hir::ExprKind::StructLiteral { struct_type, fields } => {
                generate_struct_literal(struct_type, fields, ctx)
            }
        }
    }
}

// === 辅助函数 ===

fn generate_literal<'a, 'ctx>(
    literal: &hir::LiteralValue,
    resolved_type: &SemanticType,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CGE> {
    match literal {
        hir::LiteralValue::Integer(val) => {
            let int_type = ctx.codegen.to_llvm_type(resolved_type).into_int_type();
            // [FIXED] 正确处理 HirIntValue，不再使用不安全的 as u64
            let const_int = match val {
                hir::HirIntValue::Signed(s_val) => int_type.const_int(*s_val as u64, true),
                hir::HirIntValue::Unsigned(u_val) => int_type.const_int(*u_val as u64, false),
            };
            Ok(const_int.into())
        }
        hir::LiteralValue::Float(val) => {
            let float_type = ctx.codegen.to_llvm_type(resolved_type).into_float_type();
            Ok(float_type.const_float(*val).into())
        }
        hir::LiteralValue::Bool(b) => {
            let const_bool = ctx.codegen.context.bool_type().const_int(if *b { 1 } else { 0 }, false);
            Ok(const_bool.into())
        }
        hir::LiteralValue::String(s) => {
            // 创建一个全局字符串常量，并返回其指针
            let ptr = ctx.codegen.builder.build_global_string_ptr(s, ".str").map_err(|e| CGE::InternalError { message: e.to_string(), span: (0..0).into() })?;
            Ok(ptr.as_pointer_value().into())
        }
    }
}

fn generate_binary_op<'a, 'ctx>(
    op: &BinaryOp,
    left: &hir::Expression,
    right: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CGE> {
    let left_val = left.generate(ctx)?;
    let right_val = right.generate(ctx)?;

    // analyzer 已经保证了 left 和 right 的类型是兼容的
    match left.resolved_type {
        SemanticType::Integer { is_signed, .. } => {
            let left_int = left_val.into_int_value();
            let right_int = right_val.into_int_value();
            let result = match op {
                BinaryOp::Add => ctx.codegen.builder.build_int_add(left_int, right_int, "addtmp"),
                BinaryOp::Subtract => ctx.codegen.builder.build_int_sub(left_int, right_int, "subtmp"),
                BinaryOp::Multiply => ctx.codegen.builder.build_int_mul(left_int, right_int, "multmp"),
                BinaryOp::Divide => if is_signed {
                    ctx.codegen.builder.build_int_signed_div(left_int, right_int, "sdivtmp")
                } else {
                    ctx.codegen.builder.build_int_unsigned_div(left_int, right_int, "udivtmp")
                },
                BinaryOp::Modulo => if is_signed {
                    ctx.codegen.builder.build_int_signed_rem(left_int, right_int, "sremtmp")
                } else {
                    ctx.codegen.builder.build_int_unsigned_rem(left_int, right_int, "uremtmp")
                },
                op => {
                    let predicate = utils::int_predicate(op, is_signed);
                    ctx.codegen.builder.build_int_compare(predicate, left_int, right_int, "cmptmp")
                }
            };
            Ok(result.map(Into::into).map_err(|e| CGE::InternalError{ message: e.to_string(), span: left.span.into() })?)
        }
        SemanticType::Float { .. } => {
             // ... [Similar logic for float operations] ...
            unimplemented!()
        }
        SemanticType::Bool => {
            // ... [Similar logic for bool operations] ...
            unimplemented!()
        }
        _ => unreachable!("Binary operations on non-numeric/bool types should be caught by analyzer"),
    }
}

fn generate_unary_op<'a, 'ctx>(
    op: &UnaryOp,
    right: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CGE> {
    let right_val = right.generate(ctx)?;
    
    let result = match op {
        UnaryOp::Negate => match right.resolved_type {
            SemanticType::Integer { .. } => {
                ctx.codegen.builder.build_int_neg(right_val.into_int_value(), "negtmp").map(Into::into)
            }
            SemanticType::Float { .. } => {
                ctx.codegen.builder.build_float_neg(right_val.into_float_value(), "fnegtmp").map(Into::into)
            }
            _ => unreachable!(),
        },
        UnaryOp::Not => {
            let bool_val = right_val.into_int_value();
            let zero = ctx.codegen.context.bool_type().const_zero();
            ctx.codegen.builder.build_int_compare(IntPredicate::EQ, bool_val, zero, "nottmp").map(Into::into)
        }
        _ => unreachable!("AddressOf/Dereference are handled by generate() directly"),
    };
    Ok(result.map_err(|e| CGE::InternalError{ message: e.to_string(), span: right.span.into() })?)
}


fn generate_function_call<'a, 'ctx>(
    name: &crate::parser::ast::Ident,
    args: &[hir::Expression],
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CGE> {
    
    // 使用 for 循环来处理参数生成
    let mut evaluated_args = Vec::with_capacity(args.len());
    for arg in args {
        // 在循环的每次迭代中，ctx 被短暂地可变借用，然后立即释放
        evaluated_args.push(arg.generate(ctx)?);
    }

    let function = ctx.codegen.functions.get(&name.name).unwrap_or_else(|| {
        panic!("Analyzer should have caught undeclared function '{}'", name.name)
    });
    let hir_args: Vec<BasicMetadataValueEnum<'ctx>> = evaluated_args
        .into_iter()
        .map(|val| val.into())
        .collect();

    let call_site = ctx.codegen.builder.build_call(*function, &hir_args, "calltmp")
        .map_err(|e| CGE::InternalError { message: e.to_string(), span: name.span.into() })?;

    // 处理 void 和非 void 返回值
    Ok(call_site.try_as_basic_value().left().unwrap_or_else(|| {
        ctx.codegen.context.i32_type().const_zero().into() // 为 void 返回提供一个占位符
    }))
}

fn generate_struct_literal<'a, 'ctx>(
    struct_type: &SemanticType,
    fields: &[(crate::parser::ast::Ident, hir::Expression)],
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Result<BasicValueEnum<'ctx>, CGE> {
    let llvm_struct_type = ctx.codegen.to_llvm_type(struct_type).into_struct_type();
    let struct_ptr = utils::create_entry_block_alloca("struct_literal", llvm_struct_type, ctx)?;

    for (i, (field_ident, field_expr)) in fields.iter().enumerate() {
        let field_val = field_expr.generate(ctx)?;
        let field_ptr = ctx.codegen.builder.build_struct_gep(llvm_struct_type, struct_ptr, i as u32, &field_ident.name)
            .map_err(|e| CGE::InternalError { message: e.to_string(), span: field_ident.span.into() })?;
        
        ctx.codegen.builder.build_store(field_ptr, field_val);
    }

    ctx.codegen.builder.build_load(llvm_struct_type, struct_ptr, "loadstruct").map_err(|e| CGE::InternalError{ message: e.to_string(), span: (0..0).into() })
}