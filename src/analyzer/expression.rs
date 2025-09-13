// In src/analyzer/expression.rs

use super::{hir, AnalysisContext, Lower};
use crate::analyzer::symbols::SymbolInfo;
use crate::analyzer::types::SemanticType;
use crate::utils::Span;
use crate::parser::ast::{self, BinaryOp, UnaryOp};
use crate::reporter::SemanticError;
use std::sync::Arc;

/// [REFACTORED] 为 ast::Expression 实现 Lower trait。
impl Lower for ast::Expression {
    type Output = hir::Expression;

    fn lower(&self, ctx: &mut AnalysisContext<'_>) -> Result<Self::Output, SemanticError> {
        match &self.kind {
            ast::ExprKind::Literal(literal) => lower_literal(literal, &self.span, ctx),
            ast::ExprKind::Variable(ident) => lower_variable(ident, &self.span, ctx),
            ast::ExprKind::UnaryOp { op, right } => lower_unary_op(op, right, &self.span, ctx),
            ast::ExprKind::BinaryOp { op, left, right } => {
                lower_binary_op(op, left, right, &self.span, ctx)
            }
            ast::ExprKind::Assignment { left, right } => {
                lower_assignment(left, right, &self.span, ctx)
            }
            ast::ExprKind::FunctionCall { name, args } => {
                lower_function_call(name, args, &self.span, ctx)
            }
            ast::ExprKind::MemberAccess { expression, member } => {
                lower_member_access(expression, member, &self.span, ctx)
            }
            ast::ExprKind::AggregateLiteral { .. } => {
                // 结构体初始化需要上下文提供期望类型，单独处理
                lower_aggregate_literal(&self, ctx)
            }
        }
    }
}

// === 辅助函数 ===

/// 降级字面量。
fn lower_literal(
    literal: &ast::LiteralValue,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    match literal {

        ast::LiteralValue::Integer(s) => {
            let target_type = ctx.expected_type.cloned().unwrap_or(SemanticType::i32());

            if let SemanticType::Integer { width, is_signed } = target_type {
                if s.starts_with('-') {
                    let value = s.parse::<i128>().map_err(|_| SemanticError::InvalidLiteralFormat {
                        kind: "i128".to_string(), content: s.clone(), span: (*span).into(),
                    })?;

                    if is_signed {
                        let min = -(1i128 << (width - 1));
                        let max = (1i128 << (width - 1)) - 1;
                        if value < min || value > max {
                            return Err(SemanticError::IntegerOverflow { target_type: target_type.to_string(), span: (*span).into() });
                        }
                    } else {
                        return Err(SemanticError::IntegerOverflow { target_type: target_type.to_string(), span: (*span).into() });
                    }
                    
                    // [MODIFIED] 创建 HirIntValue::Signed
                    let hir_val = hir::HirIntValue::Signed(value);
                    Ok(hir::Expression {
                        kind: hir::ExprKind::Literal(hir::LiteralValue::Integer(hir_val)),
                        span: *span,
                        resolved_type: target_type,
                    })

                } else {
                    let value = s.parse::<u128>().map_err(|_| SemanticError::InvalidLiteralFormat {
                        kind: "u128".to_string(), content: s.clone(), span: (*span).into(),
                    })?;

                    if is_signed {
                        let max = (1i128 << (width - 1)) - 1;
                        if value > max as u128 {
                             return Err(SemanticError::IntegerOverflow { target_type: target_type.to_string(), span: (*span).into() });
                        }
                    } else {
                        let max = (1u128 << width) - 1;
                        if value > max {
                             return Err(SemanticError::IntegerOverflow { target_type: target_type.to_string(), span: (*span).into() });
                        }
                    }
                    
                    // 创建 HirIntValue::Unsigned，并移除了不安全的 as i64 转换
                    let hir_val = hir::HirIntValue::Unsigned(value);
                    Ok(hir::Expression {
                        kind: hir::ExprKind::Literal(hir::LiteralValue::Integer(hir_val)),
                        span: *span,
                        resolved_type: target_type,
                    })
                }
            } else {
                Err(SemanticError::TypeMismatch {
                    expected: target_type.to_string(),
                    found: "integer".to_string(),
                    span: (*span).into(),
                })
            }
        }
        
        ast::LiteralValue::Float(s) => {
             let target_type = ctx
                .expected_type
                .cloned()
                .unwrap_or(SemanticType::Float { width: 64 });
            
            if let SemanticType::Float { .. } = target_type {
                let val = s.parse::<f64>().map_err(|_| SemanticError::InvalidLiteralFormat {
                    kind: "f64".to_string(), content: s.clone(), span: (*span).into()
                })?;
                 Ok(hir::Expression {
                    kind: hir::ExprKind::Literal(hir::LiteralValue::Float(val)),
                    span: *span,
                    resolved_type: target_type,
                })
            } else {
                Err(SemanticError::TypeMismatch {
                    expected: target_type.to_string(),
                    found: "float".to_string(),
                    span: (*span).into(),
                })
            }
        }
        ast::LiteralValue::String(s) => Ok(hir::Expression {
            kind: hir::ExprKind::Literal(hir::LiteralValue::String(s.clone())),
            span: *span,
            resolved_type: SemanticType::Ptr(Arc::new(SemanticType::Char)),
        }),
        ast::LiteralValue::Bool(b) => Ok(hir::Expression {
            kind: hir::ExprKind::Literal(hir::LiteralValue::Bool(*b)),
            span: *span,
            resolved_type: SemanticType::Bool,
        }),
    }
}

/// 降级变量引用。
fn lower_variable(
    ident: &ast::Ident,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    match ctx.symbol_table.lookup_symbol(ident) {
        Some(SymbolInfo::Variable { decl }) => Ok(hir::Expression {
            kind: hir::ExprKind::Variable(decl.clone()),
            span: *span,
            resolved_type: decl.var_type.clone(),
        }),
        Some(SymbolInfo::Type { ty }) => Err(SemanticError::ExpectedValueFoundType {
            found: ty.to_string(),
            span: (*span).into(),
        }),
        Some(SymbolInfo::Function { .. }) => Err(SemanticError::ExpectedValueFoundType {
            found: "function".to_string(),
            span: (*span).into(),
        }),
        None => Err(SemanticError::UndefinedVariable {
            name: ident.name.clone(),
            span: (*span).into(),
        }),
    }
}

/// 降级一元运算。
fn lower_unary_op(
    op: &UnaryOp,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    let hir_right = right.lower(ctx)?;

    match op {
        UnaryOp::Negate => {
            if !matches!(hir_right.resolved_type, SemanticType::Integer { .. } | SemanticType::Float { .. }) {
                return Err(SemanticError::TypeMismatch {
                    expected: "integer or float".to_string(),
                    found: hir_right.resolved_type.to_string(),
                    span: right.span.into(),
                });
            }
            let resolved_type = hir_right.resolved_type.clone();
            let kind = hir::ExprKind::UnaryOp { op: op.clone(), right: Box::new(hir_right) };
            Ok(hir::Expression { kind, span: *span, resolved_type })
        }
        UnaryOp::Not => {
            if hir_right.resolved_type != SemanticType::Bool {
                return Err(SemanticError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: hir_right.resolved_type.to_string(),
                    span: right.span.into(),
                });
            }
            let kind = hir::ExprKind::UnaryOp { op: op.clone(), right: Box::new(hir_right) };
            Ok(hir::Expression { kind, span: *span, resolved_type: SemanticType::Bool })
        }
        // ... 其他一元操作符 ...
        _ => todo!("Unary op not implemented yet"),
    }
}

/// 降级二元运算。
fn lower_binary_op(
    op: &BinaryOp,
    left: &ast::Expression,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    // 1. 降级左操作数。我们不知道它的期望类型，所以传 None。
    let hir_left = {
        let mut left_ctx = AnalysisContext {
            symbol_table: &mut *ctx.symbol_table,
            frame_manager: &mut *ctx.frame_manager,
            loop_depth: &mut *ctx.loop_depth,
            current_function_return_type: ctx.current_function_return_type,
            expected_type: None,
        };
        left.lower(&mut left_ctx)?
    };

    // 2. 降级右操作数，期望它的类型与左操作数完全相同。
    let hir_right = {
        let mut right_ctx = AnalysisContext {
            symbol_table: &mut *ctx.symbol_table,
            frame_manager: &mut *ctx.frame_manager,
            loop_depth: &mut *ctx.loop_depth,
            current_function_return_type: ctx.current_function_return_type,
            expected_type: Some(&hir_left.resolved_type),
        };
        right.lower(&mut right_ctx)?
    };

    // 3. 严格检查左右操作数类型是否一致。
    if hir_left.resolved_type != hir_right.resolved_type {
        return Err(SemanticError::TypeMismatch {
            expected: hir_left.resolved_type.to_string(),
            found: hir_right.resolved_type.to_string(),
            span: right.span.into(),
        });
    }

    // 4. 根据运算符，进行最终的类型检查并决定整个表达式的结果类型。
    let resolved_type = match op {
        // 算术和取模运算
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
            if !hir_left.resolved_type.is_numeric() {
                return Err(SemanticError::TypeMismatch {
                    expected: "numeric type".to_string(),
                    found: hir_left.resolved_type.to_string(),
                    span: (*span).into(),
                });
            }
            // 结果类型与操作数类型一致
            hir_left.resolved_type.clone()
        }
        // 比较运算符
        BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Lte | BinaryOp::Gte => {
            // 操作数必须是可比较的（数字或字符）
            if !hir_left.resolved_type.is_numeric() && !hir_left.resolved_type.is_char() {
                return Err(SemanticError::TypeMismatch {
                    expected: "numeric or char type".to_string(),
                    found: hir_left.resolved_type.to_string(),
                    span: (*span).into(),
                });
            }
            // 结果类型永远是 bool
            SemanticType::Bool
        }
        // 逻辑运算符
        BinaryOp::And | BinaryOp::Or => {
            if !hir_left.resolved_type.is_bool() {
                return Err(SemanticError::TypeMismatch {
                    expected: "bool".to_string(),
                    found: hir_left.resolved_type.to_string(),
                    span: (*span).into(),
                });
            }
            // 结果类型也是 bool
            SemanticType::Bool
        }
    };

    let kind = hir::ExprKind::BinaryOp {
        op: op.clone(),
        left: Box::new(hir_left),
        right: Box::new(hir_right),
    };

    Ok(hir::Expression { kind, span: *span, resolved_type })
}
/// 降级赋值表达式。
fn lower_assignment(
    left: &ast::Expression,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    // 检查左侧是否为合法的左值
    match &left.kind {
        ast::ExprKind::Variable(_) | ast::ExprKind::MemberAccess { .. } => { /* OK */ }
        ast::ExprKind::UnaryOp { op: ast::UnaryOp::Dereference, .. } => { /* OK */ }
        _ => return Err(SemanticError::InvalidLValue { span: left.span.into() }),
    }

    let hir_left = left.lower(ctx)?;
    let hir_right = {
        let mut right_ctx = AnalysisContext {
            // 再次使用“再借用”模式
            symbol_table: &mut *ctx.symbol_table,
            loop_depth: &mut *ctx.loop_depth,
            frame_manager: &mut *ctx.frame_manager,
            // 复制共享引用
            current_function_return_type: ctx.current_function_return_type,

            // 设置新的期望类型
            expected_type: Some(&hir_left.resolved_type),
        };
        right.lower(&mut right_ctx)?
    };
    
    // 检查 const 赋值
    if let hir::ExprKind::Variable(decl) = &hir_left.kind {
        if decl.is_const {
            return Err(SemanticError::AssignmentToConst { span: left.span.into() });
        }
    }
    
    // TODO: 类型兼容性检查 (e.g., i32 can be assigned to f64)
    
    let resolved_type = hir_left.resolved_type.clone();
    let kind = hir::ExprKind::Assignment { left: Box::new(hir_left), right: Box::new(hir_right) };
    Ok(hir::Expression { kind, span: *span, resolved_type })
}

/// 降级函数调用。
fn lower_function_call(
    name: &ast::Ident,
    args: &[ast::Expression],
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    let func_decl = match ctx.symbol_table.lookup_symbol(name) {
        Some(SymbolInfo::Function { decl }) => decl.clone(),
        _ => return Err(SemanticError::NotAFunction { name: name.name.clone(), span: name.span.into() }),
    };

    // 参数数量检查
    if !func_decl.is_variadic && args.len() != func_decl.params.len() {
        return Err(SemanticError::ArgumentCountMismatch {
            expected: func_decl.params.len(),
            found: args.len(),
            span: (*span).into(),
            definition_span: func_decl.name.span.into(),
        });
    }

    let hir_args = args
        .iter()
        .zip(func_decl.params.iter())
        .map(|(arg_expr, param_type)| {
            let mut arg_ctx = AnalysisContext {
                // 应用同样的“再借用”模式
                symbol_table: &mut *ctx.symbol_table,
                loop_depth: &mut *ctx.loop_depth,
                frame_manager: &mut *ctx.frame_manager,
                // 复制共享引用
                current_function_return_type: ctx.current_function_return_type,

                // 设置该参数的期望类型
                expected_type: Some(param_type),
            };
            arg_expr.lower(&mut arg_ctx)
        })
        .collect::<Result<Vec<_>, _>>()?;

    // TODO: 处理可变参数部分

    let kind = hir::ExprKind::FunctionCall { name: name.clone(), args: hir_args };
    Ok(hir::Expression { kind, span: *span, resolved_type: func_decl.return_type.clone() })
}


/// 降级结构体成员访问。
fn lower_member_access(
    expression: &ast::Expression,
    member: &ast::Ident,
    span: &Span,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    let hir_expr = expression.lower(ctx)?;
    
    if let SemanticType::Struct { fields, .. } = &hir_expr.resolved_type {
        if let Some((_, field_type)) = fields.iter().find(|(field_ident, _)| field_ident.name == member.name) {
            let resolved_type = (**field_type).clone();
            let kind = hir::ExprKind::MemberAccess { expression: Box::new(hir_expr), member: member.clone() };
            Ok(hir::Expression { kind, span: *span, resolved_type })
        } else {
            Err(SemanticError::FieldNotFound {
                field_name: member.name.clone(),
                struct_type: hir_expr.resolved_type.to_string(),
                span: member.span.into(),
            })
        }
    } else {
        Err(SemanticError::MemberAccessOnNonStruct { span: expression.span.into() })
    }
}

/// 降级聚合体字面量（目前仅支持结构体）。
fn lower_aggregate_literal(
    agg_expr: &ast::Expression,
    ctx: &mut AnalysisContext<'_>,
) -> Result<hir::Expression, SemanticError> {
    let (struct_name, struct_fields) = match ctx.expected_type {
        Some(SemanticType::Struct { name, fields, .. }) => (name, fields),
        _ => return Err(SemanticError::TypeMismatch{
            expected: "struct type".to_string(),
            found: "other".to_string(),
            span: agg_expr.span.into()
        }),
    };

    let values = match &agg_expr.kind {
        ast::ExprKind::AggregateLiteral { values } => values,
        _ => unreachable!(),
    };

    if values.len() != struct_fields.len() {
        return Err(SemanticError::InvalidFieldCount {
            expected: struct_fields.len(),
            found: values.len(),
            span: agg_expr.span.into(),
        });
    }

    let hir_fields = values
        .iter()
        .zip(struct_fields.iter())
        .map(|(value_expr, (field_ident, field_type))| {
            let mut field_ctx = AnalysisContext {
                // 最后一次应用“再借用”模式
                symbol_table: &mut *ctx.symbol_table,
                loop_depth: &mut *ctx.loop_depth,
                frame_manager: &mut *ctx.frame_manager,
                // 复制共享引用
                current_function_return_type: ctx.current_function_return_type,

                // 设置该结构体字段的期望类型
                expected_type: Some(field_type),
            };
            let hir_value_expr = value_expr.lower(&mut field_ctx)?;
            Ok((field_ident.clone(), hir_value_expr))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let resolved_type = ctx.expected_type.unwrap().clone();
    let kind = hir::ExprKind::StructLiteral {
        struct_type: Arc::new(resolved_type.clone()),
        fields: hir_fields,
    };
    Ok(hir::Expression { kind, span: agg_expr.span, resolved_type })
}