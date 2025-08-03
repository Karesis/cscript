// In src/analyzer/expression.rs

use super::{AnalysisContext, Lower}; // 从父模块导入核心抽象
use crate::analyzer::{
    hir,
    semantic_error::SemanticError,
    types::SemanticType,
};
use crate::parser::ast::{self, UnaryOp, BinaryOp};
use crate::lexer::Span;
use crate::analyzer::symbols::SymbolInfo;
use std::sync::Arc;

// [NEW] 为 ast::Expression 实现 Lower trait
// 它将成为新的“表达式降级分发器”
impl Lower for ast::Expression {
    type Output = hir::Expression;

    fn lower<'a>(&self, ctx: &mut AnalysisContext<'a>) -> Option<Self::Output> {
        match &self.kind {
            ast::ExprKind::Literal(literal) => {
                // 将任务委托给专门处理字面量的辅助函数
                lower_literal(literal, &self.span, ctx)
            }
            ast::ExprKind::Variable(ident) => {
                lower_variable(ident, &self.span, ctx)
            }
            ast::ExprKind::UnaryOp { op, right } => lower_unary_op(op, right, &self.span, ctx),
            ast::ExprKind::BinaryOp { op, left, right } => lower_binary_op(op, left, right, &self.span, ctx),
            ast::ExprKind::Assignment { left, right } => {
                lower_assignment(left, right, &self.span, ctx)
            }
            ast::ExprKind::FunctionCall { name, args } => {
                lower_function_call(name, args, &self.span, ctx)
            }
            ast::ExprKind::MemberAccess { expression, member } => {
                lower_member_access(expression, member, &self.span, ctx)
            }
            ast::ExprKind::AggregateLiteral { values } => {
                lower_aggregate_literal(values, &self.span, ctx)
            }

            _ => todo!("Lowering for this expression kind is not yet implemented!"),
        }
    }
}

// 专门处理字面量降级的辅助函数
// 这段逻辑是从旧的 visit_expression 中完整迁移并适配过来的
fn lower_literal<'a>(
    literal: &ast::LiteralValue,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    match literal {
        ast::LiteralValue::Integer(s) => {
            // 检查是否期望一个浮点数类型
            if let Some(SemanticType::Float { width }) = ctx.expected_type {
                return match s.parse::<f64>() {
                    Ok(val) => Some(hir::Expression {
                        kind: hir::ExprKind::Literal(hir::LiteralValue::Float(val)),
                        span: span.clone(),
                        resolved_type: SemanticType::Float { width: *width },
                    }),
                    Err(_) => {
                        ctx.diagnostics.report(
                            SemanticError::InternalError {
                                message: format!("Could not parse integer literal '{}' as float", s),
                                span: Some(span.clone()),
                            }
                            .into(),
                        );
                        None
                    }
                };
            }

            // 确定整数的目标类型，若无上下文则默认为 i32
            let target_type = ctx.expected_type
                .filter(|t| matches!(t, SemanticType::Integer { .. }))
                .unwrap_or(&SemanticType::Integer {
                    width: 32,
                    is_signed: true,
                });

            if let SemanticType::Integer { width, is_signed } = target_type {
                if let Ok(val) = s.parse::<i128>() {
                    // 检查字面量是否溢出目标类型
                    let fits = if *is_signed {
                        let min = -(1i128 << (width - 1));
                        let max = (1i128 << (width - 1)) - 1;
                        val >= min && val <= max
                    } else {
                        let max = (1u128 << width) - 1;
                        val >= 0 && (val as u128) <= max
                    };

                    if fits {
                        Some(hir::Expression {
                            kind: hir::ExprKind::Literal(hir::LiteralValue::Integer(val as i64)),
                            span: span.clone(),
                            resolved_type: target_type.clone(),
                        })
                    } else {
                        ctx.diagnostics.report(
                            SemanticError::IntegerOverflow {
                                target_type: target_type.clone(),
                                span: span.clone(),
                            }
                            .into(),
                        );
                        None
                    }
                } else {
                    ctx.diagnostics.report(
                        SemanticError::IntegerOverflow {
                            target_type: target_type.clone(),
                            span: span.clone(),
                        }
                        .into(),
                    );
                    None
                }
            } else {
                unreachable!();
            }
        }
        ast::LiteralValue::Float(s) => {
            let target_type = ctx.expected_type
                .filter(|t| matches!(t, SemanticType::Float { .. }))
                .unwrap_or(&SemanticType::Float { width: 64 });

            if let SemanticType::Float { .. } = target_type {
                match s.parse::<f64>() {
                    Ok(val) => Some(hir::Expression {
                        kind: hir::ExprKind::Literal(hir::LiteralValue::Float(val)),
                        span: span.clone(),
                        resolved_type: target_type.clone(),
                    }),
                    Err(_) => {
                        ctx.diagnostics.report(
                            SemanticError::InternalError {
                                message: format!("Could not parse float literal '{}'", s),
                                span: Some(span.clone()),
                            }
                            .into(),
                        );
                        None
                    }
                }
            } else {
                ctx.diagnostics.report(
                    SemanticError::TypeMismatch {
                        expected: target_type.clone(),
                        found: SemanticType::Float { width: 64 },
                        span: span.clone(),
                    }
                    .into(),
                );
                None
            }
        }
        ast::LiteralValue::String(s) => Some(hir::Expression {
            kind: hir::ExprKind::Literal(hir::LiteralValue::String(s.clone())),
            span: span.clone(),
            resolved_type: SemanticType::Ptr(Arc::new(SemanticType::Char)),
        }),
        ast::LiteralValue::Bool(b) => Some(hir::Expression {
            kind: hir::ExprKind::Literal(hir::LiteralValue::Bool(*b)),
            span: span.clone(),
            resolved_type: SemanticType::Bool,
        }),
    }
}

// [NEW] 专门处理变量降级的辅助函数
fn lower_variable<'a>(
    ident: &ast::Ident,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    // 这段逻辑是从旧的 visit_expression 中完整迁移并适配过来的
    match ctx.symbol_table.lookup_symbol(ident) {
        Some(SymbolInfo::Variable { decl }) => Some(hir::Expression {
            kind: hir::ExprKind::Variable(decl.clone()),
            span: span.clone(),
            resolved_type: decl.var_type.clone(),
        }),
        Some(SymbolInfo::Function { .. }) => {
            ctx.diagnostics
                .report(SemanticError::NotAFunction(ident.clone()).into());
            None
        }
        Some(SymbolInfo::Type { .. }) => {
            ctx.diagnostics
                .report(SemanticError::ExpectedValueFoundType(ident.clone()).into());
            None
        }
        None => {
            ctx.diagnostics
                .report(SemanticError::SymbolNotFound(ident.clone()).into());
            None
        }
    }
}

// [MOVED] 专门处理一元运算的辅助函数
fn lower_unary_op<'a>(
    op: &UnaryOp,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    // 应用“保存-修改-递归-恢复”模式
    // 因为一元运算的操作数没有特定的期望类型，所以我们将 expected_type 暂时设为 None

    // --- SAVE ---
    let original_expected_type = ctx.expected_type;
    // --- MODIFY ---
    ctx.expected_type = None;
    // --- RECURSE ---
    let hir_right_option = right.lower(ctx);
    // --- RESTORE ---
    ctx.expected_type = original_expected_type;

    let hir_right = hir_right_option?;
    
    // ... 剩余的逻辑和之前一样，完全不需要改变 ...
    match op {
        UnaryOp::AddressOf => {
            match &hir_right.kind {
                hir::ExprKind::Variable(_) | hir::ExprKind::Dereference(_) | hir::ExprKind::MemberAccess {..} => {}
                _ => ctx.diagnostics.report(SemanticError::InvalidLValue(hir_right.span.clone()).into()),
            }
            let resolved_type = SemanticType::Ptr(Arc::new(hir_right.resolved_type.clone()));
            let kind = hir::ExprKind::AddressOf(Box::new(hir_right));
            Some(hir::Expression { kind, span: span.clone(), resolved_type })
        }
        UnaryOp::Dereference => {
            let resolved_type = match &hir_right.resolved_type {
                SemanticType::Ptr(base) => (**base).clone(),
                _ => {
                    let expected = SemanticType::Ptr(Arc::new(SemanticType::Void));
                    ctx.diagnostics.report(
                        SemanticError::TypeMismatch {
                            expected,
                            found: hir_right.resolved_type.clone(),
                            span: right.span.clone(),
                        }.into(),
                    );
                    SemanticType::Void
                }
            };
            let kind = hir::ExprKind::Dereference(Box::new(hir_right));
            Some(hir::Expression { kind, span: span.clone(), resolved_type })
        }
        _ => { // Negate and Not
            let resolved_type = match op {
                UnaryOp::Negate => {
                    if !matches!(hir_right.resolved_type, SemanticType::Integer { .. } | SemanticType::Float { .. }) {
                        ctx.diagnostics.report(SemanticError::TypeMismatch {
                            expected: SemanticType::Integer { width: 32, is_signed: true },
                            found: hir_right.resolved_type.clone(),
                            span: right.span.clone(),
                        }.into());
                    }
                    hir_right.resolved_type.clone()
                }
                UnaryOp::Not => {
                    if hir_right.resolved_type != SemanticType::Bool {
                        ctx.diagnostics.report(SemanticError::TypeMismatch {
                            expected: SemanticType::Bool,
                            found: hir_right.resolved_type.clone(),
                            span: right.span.clone(),
                        }.into());
                    }
                    SemanticType::Bool
                }
                _ => unreachable!(),
            };
            let kind = hir::ExprKind::UnaryOp { op: op.clone(), right: Box::new(hir_right) };
            Some(hir::Expression { kind, span: span.clone(), resolved_type })
        }
    }
}


// 专门处理二元运算的辅助函数
fn lower_binary_op<'a>(
    op: &BinaryOp,
    left: &ast::Expression,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {

    // 1. 先分析左操作数，它可以使用原始的、长寿的上下文
    let hir_left = left.lower(ctx)?;

    // 2. 为右操作数的分析，创建一个全新的、临时的、短期的上下文
    //    它的生命周期仅限于这个代码块 `{...}`
    let hir_right = {
        let mut right_ctx = AnalysisContext {
            // 我们从长寿的 ctx 中“再借用”这些状态。
            // 这些再借用的生命周期被限制在这个代码块内，是安全的。
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            
            // 这是关键：我们将一个短期的引用 &hir_left.resolved_type
            // 存入一个同样是短期的上下文 right_ctx 中。
            // 生命周期完全匹配，所以这是合法的！
            expected_type: Some(&hir_left.resolved_type),
        };
        // 调用 right.lower() 时，Rust 会为 right_ctx 推断出一个新的、更短的生命周期。
        // 由于我们的 `lower` 方法签名是 `lower<'a>`，它可以接受任何生命周期的上下文。
        right.lower(&mut right_ctx)?
    };


    if hir_left.resolved_type != hir_right.resolved_type {
        ctx.diagnostics.report(
            SemanticError::TypeMismatch {
                expected: hir_left.resolved_type.clone(),
                found: hir_right.resolved_type.clone(),
                span: right.span.clone(),
            }.into(),
        );
    }

    // [NOTE] 下面的逻辑是从旧代码完整复制并修正的
    let resolved_type = match op {
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
            match (&hir_left.resolved_type, &hir_right.resolved_type) {
                (SemanticType::Integer { .. }, SemanticType::Integer { .. })
                | (SemanticType::Float { .. }, SemanticType::Float { .. }) => {
                    hir_left.resolved_type.clone()
                }
                _ => {
                    // [FIX] 使用 ctx.diagnostics 和 span.clone()
                    ctx.diagnostics.report(
                        SemanticError::TypeMismatch {
                            expected: hir_left.resolved_type.clone(),
                            found: hir_right.resolved_type.clone(),
                            span: span.clone(),
                        }
                        .into(),
                    );
                    hir_left.resolved_type.clone()
                }
            }
        }
        BinaryOp::Modulo => {
            if !matches!(hir_left.resolved_type, SemanticType::Integer { .. })
                || !matches!(hir_right.resolved_type, SemanticType::Integer { .. })
            {
                // [FIX] 使用 ctx.diagnostics 和 span.clone()
                ctx.diagnostics.report(
                    SemanticError::TypeMismatch {
                        expected: SemanticType::Integer {
                            width: 32,
                            is_signed: true,
                        },
                        found: hir_left.resolved_type.clone(),
                        span: span.clone(),
                    }
                    .into(),
                );
            }
            hir_left.resolved_type.clone()
        }
        BinaryOp::Eq
        | BinaryOp::NotEq
        | BinaryOp::Lt
        | BinaryOp::Gt
        | BinaryOp::Lte
        | BinaryOp::Gte => SemanticType::Bool,
        BinaryOp::And | BinaryOp::Or => {
            if hir_left.resolved_type != SemanticType::Bool
                || hir_right.resolved_type != SemanticType::Bool
            {
                // [FIX] 使用 ctx.diagnostics 和 span.clone()
                ctx.diagnostics.report(
                    SemanticError::TypeMismatch {
                        expected: SemanticType::Bool,
                        found: hir_left.resolved_type.clone(),
                        span: span.clone(),
                    }
                    .into(),
                );
            }
            SemanticType::Bool
        }
    };

    Some(hir::Expression {
        kind: hir::ExprKind::BinaryOp {
            op: op.clone(),
            left: Box::new(hir_left.clone()),
            right: Box::new(hir_right),
        },
        span: span.clone(),
        resolved_type,
    })
}

// [MOVED] 专门处理赋值表达式的辅助函数
fn lower_assignment<'a>(
    left: &ast::Expression,
    right: &ast::Expression,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    // 检查左侧是否为合法的左值
    match &left.kind {
        ast::ExprKind::Variable(_)
        | ast::ExprKind::UnaryOp { op: ast::UnaryOp::Dereference, .. }
        | ast::ExprKind::MemberAccess { .. } => { /* OK */ }
        _ => {
            ctx.diagnostics.report(SemanticError::InvalidLValue(left.span.clone()).into());
            return None;
        }
    }

    // 先分析左边，以确定右边的期望类型
    let hir_left = left.lower(ctx)?;
    // 将左值的类型作为期望类型传递给右值
    let hir_right = {
        let mut right_ctx = AnalysisContext {
            symbol_table: ctx.symbol_table,
            diagnostics: ctx.diagnostics,
            current_function_return_type: ctx.current_function_return_type,
            loop_depth: ctx.loop_depth,
            expected_type: Some(&hir_left.resolved_type),
        };
        right.lower(&mut right_ctx)?
    };

    // 检查 const 赋值
    if let hir::ExprKind::Variable(decl) = &hir_left.kind {
        if decl.is_const {
            ctx.diagnostics.report(SemanticError::AssignmentToConst(left.span.clone()).into());
        }
    }

    let resolved_type = hir_left.resolved_type.clone();
    Some(hir::Expression {
        kind: hir::ExprKind::Assignment {
            left: Box::new(hir_left),
            right: Box::new(hir_right),
        },
        span: span.clone(),
        resolved_type,
    })
}

// [MOVED] 专门处理函数调用的辅助函数
fn lower_function_call<'a>(
    name: &ast::Ident,
    args: &[ast::Expression],
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    if let Some(SymbolInfo::Function { return_type, params, is_variadic }) = ctx.symbol_table.lookup_symbol(name).cloned() {
        // 参数数量检查 (逻辑保持不变)
        if is_variadic {
            if args.len() < params.len() {
                ctx.diagnostics.report(SemanticError::WrongArgumentCount {
                    expected: params.len(), found: args.len(), span: span.clone(),
                }.into());
                return None;
            }
        } else {
            if args.len() != params.len() {
                ctx.diagnostics.report(SemanticError::WrongArgumentCount {
                    expected: params.len(), found: args.len(), span: span.clone(),
                }.into());
                return None;
            }
        }
        
        let mut final_hir_args = Vec::new();

        // 检查具名参数
        for (arg_expr, (param_type, _)) in args.iter().zip(params.iter()) {
            // [FIX] 为每个参数的分析，手动创建一个临时的上下文
            let mut arg_ctx = AnalysisContext {
                symbol_table: ctx.symbol_table,
                diagnostics: ctx.diagnostics,
                current_function_return_type: ctx.current_function_return_type,
                loop_depth: ctx.loop_depth,
                expected_type: Some(param_type), // 将函数签名中的参数类型作为期望类型
            };
            final_hir_args.push(arg_expr.lower(&mut arg_ctx)?);
        }
        
        // 检查可变参数
        if is_variadic {
            for i in params.len()..args.len() {
                 // [FIX] 对于可变参数，我们没有期望类型，所以创建一个 expected_type 为 None 的临时上下文
                let mut variadic_arg_ctx = AnalysisContext {
                    symbol_table: ctx.symbol_table,
                    diagnostics: ctx.diagnostics,
                    current_function_return_type: ctx.current_function_return_type,
                    loop_depth: ctx.loop_depth,
                    expected_type: None, 
                };
                final_hir_args.push(args[i].lower(&mut variadic_arg_ctx)?);
            }
        }
        
        Some(hir::Expression {
            kind: hir::ExprKind::FunctionCall { name: name.clone(), args: final_hir_args },
            span: span.clone(),
            resolved_type: return_type,
        })
    } else {
        ctx.diagnostics.report(SemanticError::NotAFunction(name.clone()).into());
        None
    }
}

// [MOVED] 专门处理成员访问的辅助函数
fn lower_member_access<'a>(
    expression: &ast::Expression,
    member: &ast::Ident,
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    let hir_expr = expression.lower(ctx)?;

    if let SemanticType::Struct { fields, .. } = &hir_expr.resolved_type {
        if let Some((_, field_type)) = fields.iter().find(|(field_ident, _)| field_ident.name == member.name) {
            let resolved_type = (**field_type).clone();
            Some(hir::Expression {
                kind: hir::ExprKind::MemberAccess { expression: Box::new(hir_expr), member: member.clone() },
                span: span.clone(),
                resolved_type,
            })
        } else {
            ctx.diagnostics.report(SemanticError::FieldNotFound {
                field_name: member.clone(), struct_type: hir_expr.resolved_type.clone(),
            }.into());
            None
        }
    } else {
        ctx.diagnostics.report(SemanticError::MemberAccessOnNonStruct { span: expression.span.clone() }.into());
        None
    }
}

// [MOVED] 专门处理聚合体字面量的辅助函数
fn lower_aggregate_literal<'a>(
    values: &[ast::Expression],
    span: &Span,
    ctx: &mut AnalysisContext<'a>,
) -> Option<hir::Expression> {
    if let Some(SemanticType::Struct { name, fields, .. }) = ctx.expected_type {
        if values.len() != fields.len() {
            ctx.diagnostics.report(SemanticError::InvalidFieldCount {
                expected: fields.len(), found: values.len(), struct_name: name.name.clone(), span: span.clone(),
            }.into());
            return None;
        }

        let hir_fields = values.iter().zip(fields.iter())
            .map(|(value_expr, (field_ident, field_type))| {
                let mut field_ctx = AnalysisContext {
                    symbol_table: ctx.symbol_table,
                    diagnostics: ctx.diagnostics,
                    current_function_return_type: ctx.current_function_return_type,
                    loop_depth: ctx.loop_depth,
                    expected_type: Some(field_type),
                };
                let hir_value_expr = value_expr.lower(&mut field_ctx)?;
                Some((field_ident.clone(), hir_value_expr))
            })
            .collect::<Option<Vec<_>>>()?;

        Some(hir::Expression {
            kind: hir::ExprKind::StructLiteral {
                struct_type: Arc::new(ctx.expected_type.unwrap().clone()),
                fields: hir_fields,
            },
            span: span.clone(),
            resolved_type: ctx.expected_type.unwrap().clone(),
        })
    } else {
        ctx.diagnostics.report(SemanticError::AggregateLiteralInInvalidContext { span: span.clone() }.into());
        None
    }
}