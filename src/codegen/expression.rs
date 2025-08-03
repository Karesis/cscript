// In src/codegen/expression.rs

use super::{CodeGenCtx, GenerateHIR}; // 从父模块导入核心抽象
use crate::analyzer::hir;
use crate::analyzer::types::SemanticType;
use inkwell::values::{BasicValueEnum, BasicValue, BasicMetadataValueEnum};
use crate::codegen::utils;
use crate::codegen::lvalue::GenerateLValue;
use crate::parser::ast::{self, BinaryOp, UnaryOp};
use inkwell::IntPredicate;

// [NEW] 为 hir::Expression 实现 GenerateHIR trait
// 这个 impl 将成为新的“表达式分发器”，取代旧的 visit_expression 中的 match 语句
impl<'a, 'ctx> GenerateHIR<'a, 'ctx> for hir::Expression {
    type Output = BasicValueEnum<'ctx>;

    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<Self::Output> {
        // self 是 &hir::Expression
        // self.kind 是 &hir::ExprKind
        match &self.kind {
            hir::ExprKind::Literal(literal_value) => {
                // 对于字面量，我们需要它的值和它的最终类型（比如 10 这个字面量，
                // 它的类型可能是 i32, u8 等）。这两个信息都在 hir::Expression 中，
                // 所以我们把它们都传递给一个专门的辅助函数来处理。
                generate_literal(literal_value, &self.resolved_type, ctx)
            }
            hir::ExprKind::BinaryOp { op, left, right } => {
                // 将工作委托给新的辅助函数
                generate_binary_op(op, left, right, ctx)
            }
            // 新增 UnaryOp 的分发分支
            hir::ExprKind::UnaryOp { op, right } => {
                generate_unary_op(op, right, ctx)
            }
            hir::ExprKind::Variable(_) => {
                // 1. 获取变量的地址 (L-value)
                let var_ptr = self.generate_lvalue(ctx)?;
                // 2. 从地址加载值 (R-value)
                let var_type = utils::to_llvm_type(&self.resolved_type, ctx);
                Some(ctx.builder.build_load(var_type, var_ptr, "loadvar").unwrap())
            }

            hir::ExprKind::Assignment { left, right } => {
                // 1. 获取左侧的地址 (L-value)
                let ptr_to_store = left.generate_lvalue(ctx)?;
                // 2. 计算右侧的值 (R-value)
                let val_to_store = right.generate(ctx)?;
                // 3. 执行存储
                ctx.builder.build_store(ptr_to_store, val_to_store);
                // 赋值表达式本身的值就是右侧的值
                Some(val_to_store)
            }
            
            hir::ExprKind::AddressOf(expr_to_addr) => {
                // 取地址表达式的 "值" 就是其操作数的地址 (L-value)
                expr_to_addr.generate_lvalue(ctx).map(|ptr| ptr.as_basic_value_enum())
            }

            hir::ExprKind::Dereference(ptr_expr) => {
                // 1. 计算出指针的值 (R-value)
                let ptr_val = ptr_expr.generate(ctx)?.into_pointer_value();
                // 2. 从指针地址加载值
                let ty = utils::to_llvm_type(&self.resolved_type, ctx);
                Some(ctx.builder.build_load(ty, ptr_val, "deref").unwrap())
            }

            hir::ExprKind::MemberAccess { .. } => {
                // 1. 获取成员的地址 (L-value)
                let member_ptr = self.generate_lvalue(ctx)?;
                // 2. 从地址加载值 (R-value)
                let member_type = utils::to_llvm_type(&self.resolved_type, ctx);
                Some(ctx.builder.build_load(member_type, member_ptr, "loadmember").unwrap())
            }

            // [ADD] 新增 FunctionCall 的分发分支
            hir::ExprKind::FunctionCall { name, args } => {
                generate_function_call(name, args, &self.resolved_type, ctx)
            }

            // [ADD] 最后一个表达式分支：StructLiteral
            hir::ExprKind::StructLiteral { struct_type, fields } => {
                generate_struct_literal(struct_type, fields, ctx)
            }
            
            // 我们将在这里逐步添加其他表达式类型的处理逻辑
            _ => todo!("Codegen for this expression kind is not yet implemented in the new architecture!"),
        }
    }
}

// [NEW] 一个专门处理字面量生成的辅助函数
// 它不属于 trait 的一部分，只是一个清晰的、可复用的内部函数
fn generate_literal<'ctx>(
    literal: &hir::LiteralValue,
    resolved_type: &SemanticType,
    ctx: &mut CodeGenCtx<'_, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    // 这段逻辑是从旧的 visit_expression 中直接复制和适配过来的
    match literal {
        hir::LiteralValue::Float(val) => {
            // 首先，我们只根据 resolved_type 确定具体的 FloatType
            if let SemanticType::Float { width } = resolved_type {
                // [FIX] `float_type` 变量现在明确是 FloatType 类型，没有 .into()
                let float_type = match width {
                    32 => ctx.context.f32_type(),
                    64 => ctx.context.f64_type(),
                    _ => unreachable!("Unsupported float width in codegen"),
                };
                
                // 然后，在确定的 float_type 上调用 .const_float()，得到一个 FloatValue
                let float_value = float_type.const_float(*val);

                // 最后，将 FloatValue 通过 .into() 转换为 BasicValueEnum
                // 此时编译器知道函数的返回值是 Option<BasicValueEnum>，所以它能正确推断
                Some(float_value.into())
            } else {
                unreachable!("Type mismatch: Float literal must have a float type");
            }
        }
        hir::LiteralValue::Integer(val) => {
            if let SemanticType::Integer { width, .. } = resolved_type {
                 let int_type = ctx.context.custom_width_int_type(*width as u32);
                 Some(int_type.const_int(*val as u64, true).into())
            } else {
                // 应该不会发生，因为类型检查已经保证了这一点
                unreachable!("Integer literal without an integer type");
            }
        }
        hir::LiteralValue::Bool(b) => {
            Some(ctx.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into())
        }
        hir::LiteralValue::String(s) => {
            Some(ctx.builder.build_global_string_ptr(s, ".str").unwrap().as_pointer_value().into())
        }
    }
}

// 专门处理二元运算的辅助函数
fn generate_binary_op<'a, 'ctx>(
    op: &BinaryOp,
    left: &hir::Expression,
    right: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    // 1. 递归地为左操作数和右操作数生成代码
    //    这就是新架构的优雅之处！我们只需调用 .generate() 即可。
    let left_val = left.generate(ctx)?;
    let right_val = right.generate(ctx)?;

    // 2. 下面的逻辑是从旧的 visit_expression 中复制和适配过来的
    //    注意：int_predicate 和 float_predicate 现在需要通过 utils:: 来调用
    match left.resolved_type {
        SemanticType::Integer { is_signed, .. } => {
            let left_int = left_val.into_int_value();
            let right_int = right_val.into_int_value();
            let result = match op {
                BinaryOp::Add => ctx.builder.build_int_add(left_int, right_int, "addtmp").map(Into::into),
                BinaryOp::Subtract => ctx.builder.build_int_sub(left_int, right_int, "subtmp").map(Into::into),
                BinaryOp::Multiply => ctx.builder.build_int_mul(left_int, right_int, "multmp").map(Into::into),
                BinaryOp::Divide => if is_signed {
                    ctx.builder.build_int_signed_div(left_int, right_int, "sdivtmp").map(Into::into)
                } else {
                    ctx.builder.build_int_unsigned_div(left_int, right_int, "udivtmp").map(Into::into)
                },
                BinaryOp::Modulo => if is_signed {
                    ctx.builder.build_int_signed_rem(left_int, right_int, "sremtmp").map(Into::into)
                } else {
                    ctx.builder.build_int_unsigned_rem(left_int, right_int, "uremtmp").map(Into::into)
                },
                op @ (BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte) => {
                    let predicate = utils::int_predicate(op, is_signed); // 使用 utils::
                    ctx.builder.build_int_compare(predicate, left_int, right_int, "cmptmp").map(Into::into)
                }
                _ => unreachable!(),
            };
            Some(result.unwrap())
        }
        SemanticType::Float { .. } => {
            let left_float = left_val.into_float_value();
            let right_float = right_val.into_float_value();
            let result = match op {
                BinaryOp::Add => ctx.builder.build_float_add(left_float, right_float, "faddtmp").map(Into::into),
                BinaryOp::Subtract => ctx.builder.build_float_sub(left_float, right_float, "fsubtmp").map(Into::into),
                BinaryOp::Multiply => ctx.builder.build_float_mul(left_float, right_float, "fmultmp").map(Into::into),
                BinaryOp::Divide => ctx.builder.build_float_div(left_float, right_float, "fdivtmp").map(Into::into),
                op @ (BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte) => {
                    let predicate = utils::float_predicate(op); // 使用 utils::
                    ctx.builder.build_float_compare(predicate, left_float, right_float, "fcmptmp").map(Into::into)
                }
                _ => {
                    // 错误处理也需要通过 ctx.diagnostics 来报告
                    // 为简化，我们暂时用 unreachable!
                    unreachable!("Operator {:?} is not supported for floats", op);
                }
            };
            Some(result.unwrap())
        }
        SemanticType::Bool => {
            let left_bool = left_val.into_int_value();
            let right_bool = right_val.into_int_value();
            let result = match op {
                BinaryOp::And => ctx.builder.build_and(left_bool, right_bool, "andtmp").map(Into::into),
                BinaryOp::Or => ctx.builder.build_or(left_bool, right_bool, "ortmp").map(Into::into),
                _ => unreachable!(),
            };
            Some(result.unwrap())
        }
        _ => unreachable!(),
    }
}

// 专门处理一元运算的辅助函数
fn generate_unary_op<'a, 'ctx>(
    op: &UnaryOp,
    right: &hir::Expression,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    // 1. 递归地为右操作数生成代码
    let right_val = right.generate(ctx)?;

    // 2. 下面的逻辑是从旧的 visit_expression 中复制和适配过来的
    let result = match op {
        UnaryOp::Negate => match right.resolved_type {
            SemanticType::Integer { .. } => {
                ctx.builder.build_int_neg(right_val.into_int_value(), "negtmp").map(Into::into)
            }
            SemanticType::Float { .. } => {
                ctx.builder.build_float_neg(right_val.into_float_value(), "fnegtmp").map(Into::into)
            }
            _ => unreachable!("Negation is only supported for integers and floats"),
        },
        UnaryOp::Not => {
            let bool_val = right_val.into_int_value();
            let zero = ctx.context.bool_type().const_zero();
            ctx.builder.build_int_compare(IntPredicate::EQ, bool_val, zero, "nottmp").map(Into::into)
        }
        // 对于 AddressOf 和 Dereference, 我们稍后在处理 lvalue 时一起解决，
        // 因为它们的语义更特殊。
        _ => unreachable!("This unary operator should be handled elsewhere (e.g., lvalues)"),
    };

    Some(result.unwrap())
}

// [ADD] 专门处理函数调用的辅助函数
fn generate_function_call<'a, 'ctx>(
    // [FIX] 参数类型从 &hir::Identifier 改为 &String 来匹配您的 HIR
    name: &ast::Ident, 
    args: &[hir::Expression],
    // [FIX] 我们不再需要 return_type 参数了，因为 inkwell 的 API 会处理 void
    _return_type: &SemanticType,
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    // 1. 从上下文中查找函数
    let function = *ctx.functions.get(&name.name).unwrap_or_else(|| {
        panic!("Function '{}' not found in function map", name.name)
    });

    // 2. 递归地为所有参数生成代码
    let mut evaluated_args = Vec::with_capacity(args.len());
    for arg in args {
        evaluated_args.push(arg.generate(ctx)?);
    }

    // 3. 将参数转换为 LLVM 需要的元数据格式
    let hir_args: Vec<BasicMetadataValueEnum<'ctx>> = evaluated_args
        .into_iter()
        .map(|val| val.into())
        .collect();

    // 4. 构建 call 指令
    let call_result = ctx.builder.build_call(function, &hir_args, "calltmp").unwrap();

    // 5. [FIX] 使用原始代码中更健壮的 void 返回值处理方式
    Some(call_result.try_as_basic_value().left().unwrap_or_else(|| {
        // 如果是 void 返回，inkwell 会给我们 None，我们则提供一个占位符
        ctx.context.i32_type().const_zero().into()
    }))
}

// 专门处理结构体字面量生成的辅助函数
fn generate_struct_literal<'a, 'ctx>(
    struct_type: &SemanticType,
    fields: &[(ast::Ident, hir::Expression)],
    ctx: &mut CodeGenCtx<'a, 'ctx>,
) -> Option<BasicValueEnum<'ctx>> {
    // 1. 获取 struct 的 LLVM 类型
    let llvm_struct_type = utils::to_llvm_type(struct_type, ctx).into_struct_type();

    // 2. 在栈上为 struct 实例分配内存。
    //    我们使用 utils::create_entry_block_alloca 来确保 alloca 在函数入口，便于优化。
    let struct_ptr = utils::create_entry_block_alloca("struct_literal", llvm_struct_type, ctx)?;

    // 3. 遍历所有字段，并逐一填充
    for (i, (field_ident, field_expr)) in fields.iter().enumerate() {
        // 3a. 递归生成字段初始化表达式的值
        let field_val = field_expr.generate(ctx)?;

        // 3b. 使用 GEP 计算该字段的内存地址
        let field_ptr = ctx.builder
            .build_struct_gep(llvm_struct_type, struct_ptr, i as u32, &field_ident.name)
            .unwrap();
        
        // 3c. 将值存入该地址
        ctx.builder.build_store(field_ptr, field_val);
    }

    // 4. 从栈指针中加载整个 struct 的值，作为表达式的结果返回
    Some(ctx.builder.build_load(llvm_struct_type, struct_ptr, "loadstruct").unwrap())
}