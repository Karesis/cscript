pub mod hir;
mod semantic_error;
mod symbols;
pub mod types;

use crate::analyzer::{
    semantic_error::SemanticError,
    symbols::{SymbolInfo, SymbolTable},
    types::SemanticType,
};
use crate::diagnostics::DiagnosticBag;
use crate::parser::ast::{self, Type as AstType, BinaryOp, UnaryOp};
use std::sync::Arc;
use std::collections::HashMap;

/// 语义分析器
pub struct Analyzer {
    symbol_table: SymbolTable,
    current_stack_offset: usize,
    current_function_return_type: Option<SemanticType>,
    loop_depth: u32,
}

// [REFACTORED] 这是完整的、修复了所有已知问题的 Analyzer 实现。
impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            current_stack_offset: 0,
            current_function_return_type: None,
            loop_depth: 0,
        }
    }

    /// 主入口函数，负责执行语义分析的两遍扫描。
    pub fn analyze(
        &mut self,
        program: &ast::Program,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<hir::Program> {
        // --- PASS 1: 收集所有全局符号 ---
        for item in &program.items {
            match item {
                ast::GlobalItem::Struct(struct_def) => {
                    // --- 内存布局计算 ---
                    let mut resolved_fields = Vec::new();
                    let mut offsets = HashMap::new();
                    let mut current_offset = 0;

                    for (field_name, field_ast_type) in &struct_def.fields {
                        // 1. 解析字段的类型
                        let field_type = self.resolve_ast_type(field_ast_type, diagnostics);
                        let field_size = field_type.size_of();

                        // 2. 存储字段的偏移量
                        offsets.insert(field_name.name.clone(), current_offset);
                        
                        // 3. 将解析后的字段信息存起来
                        resolved_fields.push((field_name.clone(), Arc::new(field_type)));
                        
                        // 4. 更新下一个字段的起始偏移量
                        current_offset += field_size;
                    }

                    // 结构体的总大小就是所有字段大小之和（暂不考虑对齐）
                    let total_size = current_offset;

                    // --- 构建完整的 SemanticType::Struct ---
                    let struct_type = SemanticType::Struct {
                        name: struct_def.name.clone(),
                        fields: resolved_fields,
                        size: total_size,
                        offsets,
                    };

                    // --- 将新的类型定义存入符号表 ---
                    if self.symbol_table.add_symbol(&struct_def.name, SymbolInfo::Type { ty: struct_type }).is_err() {
                        diagnostics.report(SemanticError::SymbolAlreadyExists(struct_def.name.clone()).into());
                    }
                }
                ast::GlobalItem::Function(func_def) => {
                    let ret_type = self.resolve_ast_type(&func_def.return_type, diagnostics);
                    let params_type: Vec<SemanticType> = func_def
                        .params
                        .iter()
                        .map(|p| self.resolve_ast_type(&p.0, diagnostics))
                        .collect();

                    let symbol_info = SymbolInfo::Function {
                        return_type: ret_type,
                        params: func_def
                            .params
                            .iter()
                            .zip(params_type)
                            .map(|((_, ident), ty)| (ty, ident.name.clone()))
                            .collect(),
                    };

                    if self.symbol_table.add_symbol(&func_def.name, symbol_info).is_err() {
                        diagnostics
                            .report(SemanticError::SymbolAlreadyExists(func_def.name.clone()).into());
                    }
                }
                ast::GlobalItem::VarDecl(var_decl) => {
                    let var_type = self.resolve_ast_type(&var_decl.var_type, diagnostics);
                    let hir_decl = Arc::new(hir::VarDecl {
                        name: var_decl.name.clone(),
                        var_type,
                        is_const: var_decl.is_const,
                        storage: hir::Storage::Global {
                            name: var_decl.name.name.clone(),
                        },
                        initializer: None,
                    });

                    if self
                        .symbol_table
                        .add_symbol(&var_decl.name, SymbolInfo::Variable { decl: hir_decl })
                        .is_err()
                    {
                        diagnostics
                            .report(SemanticError::SymbolAlreadyExists(var_decl.name.clone()).into());
                    }
                }
            }
        }

        if diagnostics.has_errors() {
            return None;
        }

        // --- PASS 2: 完整分析 ---
        let mut hir_functions = Vec::new();
        let mut hir_globals = Vec::new();

        for item in &program.items {
            match item {
                ast::GlobalItem::Struct(_) => {
                    // 所有对 struct 定义的必要分析都在第一遍扫描中完成了。
                    // 在第二遍扫描中，我们不需要对它做任何事情，只需要识别并跳过即可。
                }
                ast::GlobalItem::Function(func_def) => {
                    if let Some(hir_func) = self.visit_function(func_def, diagnostics) {
                        hir_functions.push(hir_func);
                    }
                }
                ast::GlobalItem::VarDecl(var_decl) => {
                    if let Some(SymbolInfo::Variable { decl }) =
                        self.symbol_table.lookup_symbol(&var_decl.name)
                    {
                        let mut decl_data = (**decl).clone();

                        if let Some(init) = &var_decl.init {
                            if let Some(hir_init) =
                                self.visit_expression(init, Some(&decl_data.var_type), diagnostics)
                            {
                                decl_data.initializer = Some(hir_init);
                            }
                        }

                        let final_decl_arc = Arc::new(decl_data);
                        hir_globals.push(final_decl_arc.clone());
                        self.symbol_table.update_symbol(
                            &var_decl.name,
                            SymbolInfo::Variable {
                                decl: final_decl_arc,
                            },
                        );
                    }
                }
            }
        }

        if diagnostics.has_errors() {
            None
        } else {
            Some(hir::Program {
                functions: hir_functions,
                globals: hir_globals,
            })
        }
    }

    fn resolve_ast_type(
        &mut self, // [MODIFIED] 需要可变引用，因为我们可能需要报告错误
        ast_type: &AstType,
        diagnostics: &mut DiagnosticBag,
    ) -> SemanticType {
        match ast_type {
            AstType::Void => SemanticType::Void,
            AstType::Bool => SemanticType::Bool,
            AstType::Char => SemanticType::Char,
            AstType::Int | AstType::I32 => SemanticType::Integer { width: 32, is_signed: true },
            AstType::I8 => SemanticType::Integer { width: 8, is_signed: true },
            AstType::I16 => SemanticType::Integer { width: 16, is_signed: true },
            AstType::I64 => SemanticType::Integer { width: 64, is_signed: true },
            AstType::U8 => SemanticType::Integer { width: 8, is_signed: false },
            AstType::U16 => SemanticType::Integer { width: 16, is_signed: false },
            AstType::U32 => SemanticType::Integer { width: 32, is_signed: false },
            AstType::U64 => SemanticType::Integer { width: 64, is_signed: false },
            AstType::F32 => SemanticType::Float { width: 32 },
            AstType::F64 => SemanticType::Float { width: 64 },
            // [NEW] 这是处理 struct 类型的核心逻辑
            AstType::Struct(ident) => {
                // 1. 在符号表中查找这个名字
                if let Some(symbol_info) = self.symbol_table.lookup_symbol(ident) {
                    // 2. 检查找到的符号是不是一个类型
                    if let SymbolInfo::Type { ty } = symbol_info {
                        // 3. 确认这个类型确实是一个 struct
                        if let SemanticType::Struct { .. } = ty {
                            // 成功！返回这个类型的克隆
                            ty.clone()
                        } else {
                            // 找到了一个类型，但它不是 struct（未来可能是 enum 等）
                            diagnostics.report(SemanticError::NotAType(ident.clone()).into());
                            // 返回一个“毒丸”值以允许分析继续
                            SemanticType::Void
                        }
                    } else {
                        // 找到了一个同名的符号，但它不是类型（比如是个变量或函数）
                        diagnostics.report(SemanticError::NotAType(ident.clone()).into());
                        SemanticType::Void
                    }
                } else {
                    // 在符号表中完全找不到这个名字
                    diagnostics.report(SemanticError::TypeNotFound(ident.clone()).into());
                    SemanticType::Void
                }
            },
            AstType::Ptr(base) => {
                // 递归调用时，也需要传递 diagnostics
                let resolved_base = self.resolve_ast_type(base, diagnostics);
                SemanticType::Ptr(Arc::new(resolved_base))
            }
        }
    }

    fn visit_function(
        &mut self,
        func_def: &ast::FunctionDef,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<hir::Function> {
        let return_type = self.resolve_ast_type(&func_def.return_type, diagnostics);
        self.current_function_return_type = Some(return_type.clone());
        self.current_stack_offset = 0;

        self.symbol_table.enter_scope();

        let mut hir_params = Vec::new();
        for (param_type, param_name) in &func_def.params {
            let resolved_type = self.resolve_ast_type(param_type, diagnostics);
            let size = resolved_type.size_of();
            self.current_stack_offset += size;
            let storage = hir::Storage::Local {
                offset: self.current_stack_offset as i32,
            };
            let decl = Arc::new(hir::VarDecl {
                name: param_name.clone(),
                var_type: resolved_type,
                is_const: false,
                storage,
                initializer: None,
            });

            if self
                .symbol_table
                .add_symbol(param_name, SymbolInfo::Variable { decl: decl.clone() })
                .is_err()
            {
                diagnostics.report(SemanticError::SymbolAlreadyExists(param_name.clone()).into());
            }
            hir_params.push(decl);
        }

        let body = self.visit_block(&func_def.body, diagnostics)?;

        self.symbol_table.exit_scope();
        self.current_function_return_type = None;

        Some(hir::Function {
            name: func_def.name.clone(),
            params: hir_params,
            return_type,
            body,
        })
    }

    fn visit_block(&mut self, block: &ast::Block, diagnostics: &mut DiagnosticBag) -> Option<hir::Block> {
        self.symbol_table.enter_scope();
        let hir_stmts = block
            .stmts
            .iter()
            .map(|stmt| self.visit_statement(stmt, diagnostics))
            .collect::<Option<Vec<_>>>()?;
        self.symbol_table.exit_scope();
        Some(hir::Block { stmts: hir_stmts })
    }

    fn visit_statement(
        &mut self,
        stmt: &ast::Statement,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<hir::Statement> {
        match stmt {
            ast::Statement::Block(block) => {
                let hir_block = self.visit_block(block, diagnostics)?;
                Some(hir::Statement::Block(hir_block))
            }
            ast::Statement::VarDecl(var_decl) => {
                let var_type = self.resolve_ast_type(&var_decl.var_type, diagnostics);
                let mut hir_initializer = None;

                if let Some(init_expr) = &var_decl.init {
                    hir_initializer = self.visit_expression(init_expr, Some(&var_type), diagnostics);
                }

                let size = var_type.size_of();
                self.current_stack_offset += size;
                let storage = hir::Storage::Local {
                    offset: -(self.current_stack_offset as i32),
                };
                let decl = Arc::new(hir::VarDecl {
                    name: var_decl.name.clone(),
                    var_type,
                    is_const: var_decl.is_const,
                    storage,
                    initializer: hir_initializer,
                });

                if self
                    .symbol_table
                    .add_symbol(&var_decl.name, SymbolInfo::Variable { decl: decl.clone() })
                    .is_err()
                {
                    diagnostics
                        .report(SemanticError::SymbolAlreadyExists(var_decl.name.clone()).into());
                }

                Some(hir::Statement::VarDecl(decl))
            }
            ast::Statement::Expr(expr) => {
                Some(hir::Statement::Expr(self.visit_expression(expr, None, diagnostics)?))
            }
            ast::Statement::Return { value: opt_expr, span } => {
                let return_type = self
                    .current_function_return_type
                    .clone()
                    .expect("Internal error: No function context for return");

                let value = if let Some(expr) = opt_expr {
                    self.visit_expression(expr, Some(&return_type), diagnostics)
                } else {
                    if return_type != SemanticType::Void {
                        diagnostics.report(
                            SemanticError::TypeMismatch {
                                expected: return_type,
                                found: SemanticType::Void,
                                span: span.clone(),
                            }
                            .into(),
                        );
                    }
                    None
                };
                Some(hir::Statement::Return {
                    value,
                    span: span.clone(),
                })
            }
            ast::Statement::If {
                condition,
                then_branch,
                else_branch,
                span,
            } => {
                let cond_hir = self.visit_expression(condition, Some(&SemanticType::Bool), diagnostics)?;

                if cond_hir.resolved_type != SemanticType::Bool {
                    diagnostics.report(
                        SemanticError::TypeMismatch {
                            expected: SemanticType::Bool,
                            found: cond_hir.resolved_type.clone(),
                            span: condition.span.clone(),
                        }
                        .into(),
                    );
                }

                let then_hir = self.visit_block(then_branch, diagnostics)?;

                let else_hir = if let Some(else_b) = else_branch {
                    Some(self.visit_block(else_b, diagnostics)?)
                } else {
                    None
                };

                Some(hir::Statement::If {
                    condition: cond_hir,
                    then_branch: then_hir,
                    else_branch: else_hir,
                    span: span.clone(),
                })
            }
            ast::Statement::While {
                condition,
                body,
                span,
            } => {
                let cond_hir = self.visit_expression(condition, Some(&SemanticType::Bool), diagnostics)?;

                if cond_hir.resolved_type != SemanticType::Bool {
                    diagnostics.report(
                        SemanticError::TypeMismatch {
                            expected: SemanticType::Bool,
                            found: cond_hir.resolved_type.clone(),
                            span: condition.span.clone(),
                        }
                        .into(),
                    );
                }

                self.loop_depth += 1;
                let body_hir = self.visit_block(body, diagnostics)?;
                self.loop_depth -= 1;

                Some(hir::Statement::While {
                    condition: cond_hir,
                    body: body_hir,
                    span: span.clone(),
                })
            }
            ast::Statement::Break(span) => {
                if self.loop_depth == 0 {
                    diagnostics.report(SemanticError::BreakOutsideLoop(span.clone()).into());
                }
                Some(hir::Statement::Break(span.clone()))
            }
            ast::Statement::Continue(span) => {
                if self.loop_depth == 0 {
                    diagnostics.report(SemanticError::ContinueOutsideLoop(span.clone()).into());
                }
                Some(hir::Statement::Continue(span.clone()))
            }
        }
    }

    /// [CORRECTED] The fully refactored "brain" of the analyzer.
    fn visit_expression(
        &mut self,
        expr: &ast::Expression,
        expected_type: Option<&SemanticType>,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<hir::Expression> {
        match &expr.kind {
            // [REFACTORED] 这是处理聚合体字面量的、完整的“审查”逻辑
            ast::ExprKind::AggregateLiteral { values } => {
                // 1. 检查上下文：必须期望一个 struct 类型
                if let Some(SemanticType::Struct { name, fields, .. }) = expected_type {
                    // 2. 检查字段数量是否匹配
                    if values.len() != fields.len() {
                        diagnostics.report(SemanticError::InvalidFieldCount {
                            expected: fields.len(),
                            found: values.len(),
                            struct_name: name.name.clone(),
                            span: expr.span.clone(),
                        }.into());
                        return None;
                    }

                    // 3. 按顺序检查每个字段的类型
                    let hir_fields = values.iter().zip(fields.iter())
                        .map(|(value_expr, (field_ident, field_type))| {
                            // 递归调用 visit_expression，并将字段的类型作为新的期望类型传递下去
                            let hir_value_expr = self.visit_expression(value_expr, Some(field_type), diagnostics)?;
                            Some((field_ident.clone(), hir_value_expr))
                        })
                        .collect::<Option<Vec<_>>>()?;

                    // 4. 所有检查通过，构建 HIR 节点
                    Some(hir::Expression {
                        kind: hir::ExprKind::StructLiteral {
                            struct_type: Arc::new(expected_type.unwrap().clone()),
                            fields: hir_fields,
                        },
                        span: expr.span.clone(),
                        resolved_type: expected_type.unwrap().clone(),
                    })

                } else {
                    // 错误：在无效的上下文（非 struct 或无上下文）中使用了聚合体字面量
                    diagnostics.report(SemanticError::AggregateLiteralInInvalidContext {
                        span: expr.span.clone(),
                    }.into());
                    None
                }
            }
            ast::ExprKind::Literal(literal) => match literal {
                ast::LiteralValue::Integer(s) => {
                    if let Some(SemanticType::Float { width }) = expected_type {
                        return match s.parse::<f64>() {
                            Ok(val) => Some(hir::Expression {
                                kind: hir::ExprKind::Literal(hir::LiteralValue::Float(val)),
                                span: expr.span.clone(),
                                resolved_type: SemanticType::Float { width: *width },
                            }),
                            Err(_) => {
                                diagnostics.report(
                                    SemanticError::InternalError {
                                        message: format!("Could not parse integer literal '{}' as float", s),
                                        span: Some(expr.span.clone()),
                                    }
                                    .into(),
                                );
                                None
                            }
                        };
                    }

                    let target_type = expected_type
                        .filter(|t| matches!(t, SemanticType::Integer { .. }))
                        .unwrap_or(&SemanticType::Integer {
                            width: 32,
                            is_signed: true,
                        });

                    if let SemanticType::Integer { width, is_signed } = target_type {
                        if let Ok(val) = s.parse::<i128>() {
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
                                    span: expr.span.clone(),
                                    resolved_type: target_type.clone(),
                                })
                            } else {
                                diagnostics.report(
                                    SemanticError::IntegerOverflow {
                                        target_type: target_type.clone(),
                                        span: expr.span.clone(),
                                    }
                                    .into(),
                                );
                                None
                            }
                        } else {
                            diagnostics.report(
                                SemanticError::IntegerOverflow {
                                    target_type: target_type.clone(),
                                    span: expr.span.clone(),
                                }
                                .into(),
                            );
                            None
                        }
                    } else {
                        unreachable!()
                    }
                }
                ast::LiteralValue::Float(s) => {
                    let target_type = expected_type
                        .filter(|t| matches!(t, SemanticType::Float { .. }))
                        .unwrap_or(&SemanticType::Float { width: 64 });

                    if let SemanticType::Float { .. } = target_type {
                        match s.parse::<f64>() {
                            Ok(val) => Some(hir::Expression {
                                kind: hir::ExprKind::Literal(hir::LiteralValue::Float(val)),
                                span: expr.span.clone(),
                                resolved_type: target_type.clone(),
                            }),
                            Err(_) => {
                                diagnostics.report(
                                    SemanticError::InternalError {
                                        message: format!("Could not parse float literal '{}'", s),
                                        span: Some(expr.span.clone()),
                                    }
                                    .into(),
                                );
                                None
                            }
                        }
                    } else {
                        diagnostics.report(
                            SemanticError::TypeMismatch {
                                expected: target_type.clone(),
                                found: SemanticType::Float { width: 64 },
                                span: expr.span.clone(),
                            }
                            .into(),
                        );
                        None
                    }
                }
                ast::LiteralValue::String(s) => Some(hir::Expression {
                    kind: hir::ExprKind::Literal(hir::LiteralValue::String(s.clone())),
                    span: expr.span.clone(),
                    resolved_type: SemanticType::Ptr(Arc::new(SemanticType::Char)),
                }),
                ast::LiteralValue::Bool(b) => Some(hir::Expression {
                    kind: hir::ExprKind::Literal(hir::LiteralValue::Bool(*b)),
                    span: expr.span.clone(),
                    resolved_type: SemanticType::Bool,
                }),
            },

            ast::ExprKind::Variable(ident) => match self.symbol_table.lookup_symbol(ident) {
                Some(SymbolInfo::Variable { decl }) => Some(hir::Expression {
                    kind: hir::ExprKind::Variable(decl.clone()),
                    span: expr.span.clone(),
                    resolved_type: decl.var_type.clone(),
                }),
                Some(SymbolInfo::Function { .. }) => {
                    diagnostics.report(SemanticError::NotAFunction(ident.clone()).into());
                    None
                }
                Some(SymbolInfo::Type { .. }) => {
                    diagnostics.report(SemanticError::ExpectedValueFoundType(ident.clone()).into());
                    None
                }
                None => {
                    diagnostics.report(SemanticError::SymbolNotFound(ident.clone()).into());
                    None
                }
            },
            ast::ExprKind::UnaryOp { op, right } => {
                let hir_right = self.visit_expression(right, None, diagnostics)?;
                match op {
                    UnaryOp::AddressOf => {
                        match &hir_right.kind {
                            hir::ExprKind::Variable(_) | hir::ExprKind::Dereference(_) => {}
                            _ => diagnostics
                                .report(SemanticError::InvalidLValue(hir_right.span.clone()).into()),
                        }
                        let resolved_type =
                            SemanticType::Ptr(Arc::new(hir_right.resolved_type.clone()));
                        let kind = hir::ExprKind::AddressOf(Box::new(hir_right));
                        Some(hir::Expression {
                            kind,
                            span: expr.span.clone(),
                            resolved_type,
                        })
                    }
                    UnaryOp::Dereference => {
                        let resolved_type = match &hir_right.resolved_type {
                            SemanticType::Ptr(base) => (**base).clone(),
                            _ => {
                                diagnostics.report(
                                    SemanticError::TypeMismatch {
                                        expected: SemanticType::Ptr(Arc::new(SemanticType::Void)),
                                        found: hir_right.resolved_type.clone(),
                                        span: right.span.clone(),
                                    }
                                    .into(),
                                );
                                SemanticType::Void
                            }
                        };
                        let kind = hir::ExprKind::Dereference(Box::new(hir_right));
                        Some(hir::Expression {
                            kind,
                            span: expr.span.clone(),
                            resolved_type,
                        })
                    }
                    _ => {
                        let resolved_type = match op {
                            UnaryOp::Negate => {
                                if !matches!(
                                    hir_right.resolved_type,
                                    SemanticType::Integer { .. } | SemanticType::Float { .. }
                                ) {
                                    diagnostics.report(
                                        SemanticError::TypeMismatch {
                                            expected: SemanticType::Integer {
                                                width: 32,
                                                is_signed: true,
                                            },
                                            found: hir_right.resolved_type.clone(),
                                            span: right.span.clone(),
                                        }
                                        .into(),
                                    );
                                }
                                hir_right.resolved_type.clone()
                            }
                            UnaryOp::Not => {
                                if hir_right.resolved_type != SemanticType::Bool {
                                    diagnostics.report(
                                        SemanticError::TypeMismatch {
                                            expected: SemanticType::Bool,
                                            found: hir_right.resolved_type.clone(),
                                            span: right.span.clone(),
                                        }
                                        .into(),
                                    );
                                }
                                SemanticType::Bool
                            }
                            _ => unreachable!(),
                        };
                        let kind =
                            hir::ExprKind::UnaryOp { op: op.clone(), right: Box::new(hir_right) };
                        Some(hir::Expression {
                            kind,
                            span: expr.span.clone(),
                            resolved_type,
                        })
                    }
                }
            }
            ast::ExprKind::BinaryOp { op, left, right } => {
                let hir_left = self.visit_expression(left, None, diagnostics)?;
                let hir_right =
                    self.visit_expression(right, Some(&hir_left.resolved_type), diagnostics)?;

                if hir_left.resolved_type != hir_right.resolved_type {
                    diagnostics.report(
                        SemanticError::TypeMismatch {
                            expected: hir_left.resolved_type.clone(),
                            found: hir_right.resolved_type.clone(),
                            span: right.span.clone(),
                        }
                        .into(),
                    );
                }

                let resolved_type = match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                        match (&hir_left.resolved_type, &hir_right.resolved_type) {
                            (SemanticType::Integer { .. }, SemanticType::Integer { .. })
                            | (SemanticType::Float { .. }, SemanticType::Float { .. }) => {
                                hir_left.resolved_type.clone()
                            }
                            _ => {
                                diagnostics.report(
                                    SemanticError::TypeMismatch {
                                        expected: hir_left.resolved_type.clone(),
                                        found: hir_right.resolved_type.clone(),
                                        span: expr.span.clone(),
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
                            diagnostics.report(
                                SemanticError::TypeMismatch {
                                    expected: SemanticType::Integer {
                                        width: 32,
                                        is_signed: true,
                                    },
                                    found: hir_left.resolved_type.clone(),
                                    span: expr.span.clone(),
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
                            diagnostics.report(
                                SemanticError::TypeMismatch {
                                    expected: SemanticType::Bool,
                                    found: hir_left.resolved_type.clone(),
                                    span: expr.span.clone(),
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
                        left: Box::new(hir_left),
                        right: Box::new(hir_right),
                    },
                    span: expr.span.clone(),
                    resolved_type,
                })
            }
            ast::ExprKind::Assignment { left, right } => {
                // [CORRECTED] 更新了左值检查的逻辑
                match &left.kind {
                    ast::ExprKind::Variable(_)
                    | ast::ExprKind::UnaryOp { op: ast::UnaryOp::Dereference, .. }
                    | ast::ExprKind::MemberAccess { .. } => {
                        // This is a valid l-value, proceed.
                    }
                    _ => {
                        diagnostics.report(SemanticError::InvalidLValue(left.span.clone()).into());
                        return None;
                    }
                }

                // 先分析左边，以确定右边的期望类型
                let hir_left = self.visit_expression(left, None, diagnostics)?;
                // 将左值的类型作为期望类型传递给右值
                let hir_right = self.visit_expression(right, Some(&hir_left.resolved_type), diagnostics)?;

                if let hir::ExprKind::Variable(decl) = &hir_left.kind {
                    if decl.is_const {
                        diagnostics.report(SemanticError::AssignmentToConst(left.span.clone()).into());
                    }
                }

                let resolved_type = hir_left.resolved_type.clone();
                Some(hir::Expression {
                    kind: hir::ExprKind::Assignment {
                        left: Box::new(hir_left),
                        right: Box::new(hir_right),
                    },
                    span: expr.span.clone(),
                    resolved_type,
                })
            }
            ast::ExprKind::FunctionCall { name, args } => {
                if let Some(SymbolInfo::Function {
                    return_type,
                    params,
                }) = self.symbol_table.lookup_symbol(name).cloned()
                {
                    if args.len() != params.len() {
                        diagnostics.report(
                            SemanticError::WrongArgumentCount {
                                expected: params.len(),
                                found: args.len(),
                                span: expr.span.clone(),
                            }
                            .into(),
                        );
                        return None;
                    }

                    let hir_args = args
                        .iter()
                        .zip(params.iter())
                        .map(|(arg_expr, (param_type, _))| {
                            self.visit_expression(arg_expr, Some(param_type), diagnostics)
                        })
                        .collect::<Option<Vec<_>>>()?;

                    Some(hir::Expression {
                        kind: hir::ExprKind::FunctionCall {
                            name: name.clone(),
                            args: hir_args,
                        },
                        span: expr.span.clone(),
                        resolved_type: return_type,
                    })
                } else {
                    diagnostics.report(SemanticError::NotAFunction(name.clone()).into());
                    None
                }
            }
            // [NEW] 这是处理成员访问的核心逻辑
            ast::ExprKind::MemberAccess { expression, member } => {
                // 1. 首先，递归地分析点号 `.` 左边的表达式，此时我们不知道它的期望类型
                let hir_expr = self.visit_expression(expression, None, diagnostics)?;

                // 2. 检查左边表达式的类型是否真的是一个 struct
                if let SemanticType::Struct { fields, .. } = &hir_expr.resolved_type {
                    // 3. 在 struct 的字段列表中查找我们想要访问的成员
                    if let Some((_, field_type)) = fields.iter().find(|(field_ident, _)| field_ident.name == member.name) {
                        // 4. 成功！整个表达式的类型就是该成员的类型
                        let resolved_type = (**field_type).clone();
                        Some(hir::Expression {
                            kind: hir::ExprKind::MemberAccess {
                                expression: Box::new(hir_expr),
                                member: member.clone(),
                            },
                            span: expr.span.clone(),
                            resolved_type,
                        })
                    } else {
                        // 5. 错误：在 struct 中找不到该字段
                        diagnostics.report(SemanticError::FieldNotFound {
                            field_name: member.clone(),
                            struct_type: hir_expr.resolved_type.clone(),
                        }.into());
                        None
                    }
                } else {
                    // 6. 错误：试图在非 struct 类型上使用 `.` 操作符
                    diagnostics.report(SemanticError::MemberAccessOnNonStruct {
                        span: expression.span.clone(),
                    }.into());
                    None
                }
            }
        }
    }
}
