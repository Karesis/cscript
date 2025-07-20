// src/analyzer/main.rs

use crate::parser::ast::{self, Type as AstType, UnaryOp, BinaryOp}; 
use crate::analyzer::{hir, types::SemanticType, symbols::{SymbolTable, SymbolInfo}};
use std::sync::Arc;
use crate::lexer::Span;

/// 语义错误
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticError {
    SymbolNotFound(String),
    SymbolAlreadyExists(String),
    TypeMismatch { expected: SemanticType, found: SemanticType, span: Span },
    InvalidLValue(Span),
    NotAFunction(String),
    WrongArgumentCount { expected: usize, found: usize, span: Span },
    BreakOutsideLoop(Span),
    ContinueOutsideLoop(Span),
    AssignmentToConst(Span),
}

/// 语义分析器
pub struct Analyzer {
    symbol_table: SymbolTable,
    current_stack_offset: usize, 
    current_function_return_type: Option<SemanticType>,
    loop_depth: u32,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            current_stack_offset: 0,
            current_function_return_type: None,
            loop_depth: 0,
        }
    }

    pub fn analyze(&mut self, program: &ast::Program) -> Result<hir::Program, Vec<SemanticError>> {
        let mut errors = Vec::new();

        // PASS 1: 收集所有全局符号
        for item in &program.items {
            match item {
                ast::GlobalItem::Function(func_def) => {
                    let ret_type = self.resolve_ast_type(&func_def.return_type);
                    let params_type: Vec<SemanticType> = func_def.params.iter().map(|p| self.resolve_ast_type(&p.0)).collect();
                    let symbol_info = SymbolInfo::Function {
                        return_type: ret_type,
                        params: func_def.params.iter().zip(params_type).map(|((_, ident), ty)| (ty, ident.name.clone())).collect(),
                    };
                    if let Err(e) = self.symbol_table.add_symbol(&func_def.name, symbol_info) {
                        errors.push(SemanticError::SymbolAlreadyExists(e));
                    }
                }
                ast::GlobalItem::VarDecl(var_decl) => {
                    let var_type = self.resolve_ast_type(&var_decl.var_type);
                    let hir_decl = Arc::new(hir::VarDecl {
                        name: var_decl.name.clone(), var_type, is_const: var_decl.is_const,
                        storage: hir::Storage::Global { name: var_decl.name.name.clone() },
                        initializer: None,
                    });
                    if let Err(e) = self.symbol_table.add_symbol(&var_decl.name, SymbolInfo::Variable { decl: hir_decl }) {
                        errors.push(SemanticError::SymbolAlreadyExists(e));
                    }
                }
            }
        }

        if !errors.is_empty() { return Err(errors); }

        // PASS 2: 分析函数体和全局变量初始化
        let mut hir_functions = Vec::new();
        let mut hir_globals = Vec::new();
        for item in &program.items {
            match item {
                ast::GlobalItem::Function(func_def) => match self.visit_function(func_def) {
                    Ok(hir_func) => hir_functions.push(hir_func),
                    Err(e) => errors.push(e),
                },
                ast::GlobalItem::VarDecl(var_decl) => {
                    if let Some(SymbolInfo::Variable { decl }) = self.symbol_table.lookup_symbol(&var_decl.name) {
                        let mut decl_clone = (**decl).clone();
                        if let Some(init) = &var_decl.init {
                            match self.visit_expression(init) {
                                Ok(hir_init) => {
                                    if hir_init.resolved_type != decl_clone.var_type {
                                        errors.push(SemanticError::TypeMismatch { expected: decl_clone.var_type.clone(), found: hir_init.resolved_type.clone(), span: init.span.clone() });
                                    }
                                    decl_clone.initializer = Some(hir_init);
                                }
                                Err(e) => errors.push(e),
                            }
                        }
                        hir_globals.push(Arc::new(decl_clone));
                    }
                }
            }
        }
        
        if !errors.is_empty() { return Err(errors); }
        Ok(hir::Program { functions: hir_functions, globals: hir_globals })
    }
    
    fn resolve_ast_type(&self, ast_type: &AstType) -> SemanticType {
        match ast_type {
            AstType::Void => SemanticType::Void, AstType::Bool => SemanticType::Bool,
            AstType::Char => SemanticType::Char, AstType::Int => SemanticType::Int { width: 32, is_signed: true },
            AstType::Ptr(base) => SemanticType::Ptr(Arc::new(self.resolve_ast_type(base))),
        }
    }

    fn visit_function(&mut self, func_def: &ast::FunctionDef) -> Result<hir::Function, SemanticError> {
        let return_type = self.resolve_ast_type(&func_def.return_type);
        self.current_function_return_type = Some(return_type.clone());
        self.current_stack_offset = 0;
        self.symbol_table.enter_scope();
        let mut hir_params = Vec::new();
        for (param_type, param_name) in &func_def.params {
            let resolved_type = self.resolve_ast_type(param_type);
            let size = resolved_type.size_of();
            self.current_stack_offset += size;
            let storage = hir::Storage::Local { offset: self.current_stack_offset as i32 };
            let decl = Arc::new(hir::VarDecl { name: param_name.clone(), var_type: resolved_type, is_const: false, storage, initializer: None });
            self.symbol_table.add_symbol(param_name, SymbolInfo::Variable { decl: decl.clone() }).map_err(SemanticError::SymbolAlreadyExists)?;
            hir_params.push(decl);
        }
        let body = self.visit_block(&func_def.body)?;
        self.symbol_table.exit_scope();
        self.current_function_return_type = None;
        Ok(hir::Function { name: func_def.name.clone(), params: hir_params, return_type, body })
    }

    fn visit_block(&mut self, block: &ast::Block) -> Result<hir::Block, SemanticError> {
        self.symbol_table.enter_scope();
        let hir_stmts = block.stmts.iter().map(|stmt| self.visit_statement(stmt)).collect::<Result<Vec<_>, _>>()?;
        self.symbol_table.exit_scope();
        Ok(hir::Block { stmts: hir_stmts })
    }
    
    fn visit_statement(&mut self, stmt: &ast::Statement) -> Result<hir::Statement, SemanticError> {
        match stmt {
            ast::Statement::VarDecl(var_decl) => {
                let var_type = self.resolve_ast_type(&var_decl.var_type);
                let mut hir_initializer = None;
                if let Some(init_expr) = &var_decl.init {
                    let expr = self.visit_expression(init_expr)?;
                    if expr.resolved_type != var_type {
                        return Err(SemanticError::TypeMismatch { expected: var_type, found: expr.resolved_type, span: init_expr.span.clone() });
                    }
                    hir_initializer = Some(expr);
                }
                let size = var_type.size_of();
                self.current_stack_offset += size;
                let storage = hir::Storage::Local { offset: -(self.current_stack_offset as i32) };
                let decl = Arc::new(hir::VarDecl { name: var_decl.name.clone(), var_type, is_const: var_decl.is_const, storage, initializer: hir_initializer });
                self.symbol_table.add_symbol(&var_decl.name, SymbolInfo::Variable { decl: decl.clone() }).map_err(SemanticError::SymbolAlreadyExists)?;
                Ok(hir::Statement::VarDecl(decl))
            }
            ast::Statement::Expr(expr) => Ok(hir::Statement::Expr(self.visit_expression(expr)?)),
            ast::Statement::Return { value: opt_expr, span } => {
                let return_type = self.current_function_return_type.clone().expect("No function context for return");
                let value = if let Some(expr) = opt_expr {
                    let hir_expr = self.visit_expression(expr)?;
                    if hir_expr.resolved_type != return_type {
                        return Err(SemanticError::TypeMismatch { expected: return_type, found: hir_expr.resolved_type, span: expr.span.clone() });
                    }
                    Some(hir_expr)
                } else {
                    if return_type != SemanticType::Void {
                        return Err(SemanticError::TypeMismatch { expected: return_type, found: SemanticType::Void, span: span.clone() });
                    }
                    None
                };
                Ok(hir::Statement::Return { value, span: span.clone() })
            }
            ast::Statement::If { condition, then_branch, else_branch, span } => {
                let cond_hir = self.visit_expression(condition)?;
                if cond_hir.resolved_type != SemanticType::Bool {
                    return Err(SemanticError::TypeMismatch { expected: SemanticType::Bool, found: cond_hir.resolved_type, span: condition.span.clone() });
                }
                let then_hir = self.visit_block(then_branch)?;
                let else_hir = if let Some(else_b) = else_branch { Some(self.visit_block(else_b)?) } else { None };
                Ok(hir::Statement::If { condition: cond_hir, then_branch: then_hir, else_branch: else_hir, span: span.clone() })
            }
            ast::Statement::While { condition, body, span } => {
                let cond_hir = self.visit_expression(condition)?;
                if cond_hir.resolved_type != SemanticType::Bool {
                    return Err(SemanticError::TypeMismatch { expected: SemanticType::Bool, found: cond_hir.resolved_type, span: condition.span.clone() });
                }
                self.loop_depth += 1;
                let body_hir = self.visit_block(body)?;
                self.loop_depth -= 1;
                Ok(hir::Statement::While { condition: cond_hir, body: body_hir, span: span.clone() })
            }
            ast::Statement::Break(span) => {
                if self.loop_depth == 0 { return Err(SemanticError::BreakOutsideLoop(span.clone())); }
                Ok(hir::Statement::Break(span.clone()))
            }
            ast::Statement::Continue(span) => {
                if self.loop_depth == 0 { return Err(SemanticError::ContinueOutsideLoop(span.clone())); }
                Ok(hir::Statement::Continue(span.clone()))
            }
        }
    }

    fn visit_expression(&mut self, expr: &ast::Expression) -> Result<hir::Expression, SemanticError> {
        match &expr.kind {
            ast::ExprKind::Literal(literal) => {
                let (resolved_type, literal_kind) = match literal {
                    ast::LiteralValue::Integer(s) => (SemanticType::Int { width: 32, is_signed: true }, ast::LiteralValue::Integer(s.clone())),
                    ast::LiteralValue::String(s) => (SemanticType::Ptr(Arc::new(SemanticType::Char)), ast::LiteralValue::String(s.clone())),
                    ast::LiteralValue::Bool(b) => (SemanticType::Bool, ast::LiteralValue::Bool(*b)),
                };
                Ok(hir::Expression { kind: hir::ExprKind::Literal(literal_kind), span: expr.span.clone(), resolved_type })
            }
            ast::ExprKind::Variable(ident) => match self.symbol_table.lookup_symbol(ident) {
                Some(SymbolInfo::Variable { decl }) => Ok(hir::Expression {
                    kind: hir::ExprKind::Variable(decl.clone()), span: expr.span.clone(), resolved_type: decl.var_type.clone(),
                }),
                Some(SymbolInfo::Function { .. }) => Err(SemanticError::NotAFunction(ident.name.clone())),
                None => Err(SemanticError::SymbolNotFound(format!("Variable '{}' not found", ident.name))),
            },
            ast::ExprKind::UnaryOp { op, right } => {
                let hir_right = self.visit_expression(right)?;
                let resolved_type = match op {
                    UnaryOp::Negate => {
                        if !matches!(hir_right.resolved_type, SemanticType::Int { .. }) {
                            return Err(SemanticError::TypeMismatch{ expected: SemanticType::Int{width: 32, is_signed: true}, found: hir_right.resolved_type, span: right.span.clone() });
                        }
                        hir_right.resolved_type.clone()
                    }
                    UnaryOp::Not => {
                        if hir_right.resolved_type != SemanticType::Bool {
                             return Err(SemanticError::TypeMismatch{ expected: SemanticType::Bool, found: hir_right.resolved_type, span: right.span.clone() });
                        }
                        SemanticType::Bool
                    }
                    UnaryOp::AddressOf => {
                        match &hir_right.kind {
                            hir::ExprKind::Variable(_) | hir::ExprKind::Dereference(_) => {},
                            _ => return Err(SemanticError::InvalidLValue(hir_right.span)),
                        }
                        SemanticType::Ptr(Arc::new(hir_right.resolved_type.clone()))
                    }
                    UnaryOp::Dereference => match &hir_right.resolved_type {
                        SemanticType::Ptr(base) => (**base).clone(),
                        _ => return Err(SemanticError::TypeMismatch{ expected: SemanticType::Ptr(Arc::new(SemanticType::Void)), found: hir_right.resolved_type, span: right.span.clone() })
                    }
                };
                Ok(hir::Expression { kind: hir::ExprKind::UnaryOp { op: op.clone(), right: Box::new(hir_right) }, span: expr.span.clone(), resolved_type })
            }
            ast::ExprKind::BinaryOp { op, left, right } => {
                let hir_left = self.visit_expression(left)?;
                let hir_right = self.visit_expression(right)?;
                let resolved_type = match op {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => {
                        if !matches!(hir_left.resolved_type, SemanticType::Int { .. }) || !matches!(hir_right.resolved_type, SemanticType::Int { .. }) {
                           return Err(SemanticError::TypeMismatch { expected: SemanticType::Int { width: 32, is_signed: true }, found: hir_left.resolved_type, span: expr.span.clone() });
                        }
                        hir_left.resolved_type.clone()
                    }
                    BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Lte | BinaryOp::Gte => {
                        if hir_left.resolved_type != hir_right.resolved_type {
                           return Err(SemanticError::TypeMismatch { expected: hir_left.resolved_type, found: hir_right.resolved_type, span: expr.span.clone() });
                        }
                        SemanticType::Bool
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if hir_left.resolved_type != SemanticType::Bool || hir_right.resolved_type != SemanticType::Bool {
                           return Err(SemanticError::TypeMismatch { expected: SemanticType::Bool, found: hir_left.resolved_type, span: expr.span.clone() });
                        }
                        SemanticType::Bool
                    }
                };
                Ok(hir::Expression { kind: hir::ExprKind::BinaryOp { op: op.clone(), left: Box::new(hir_left), right: Box::new(hir_right) }, span: expr.span.clone(), resolved_type })
            }
            ast::ExprKind::Assignment { left, right } => {
                let hir_right = self.visit_expression(right)?;
                match &left.kind {
                    ast::ExprKind::Variable(_) | ast::ExprKind::UnaryOp { op: ast::UnaryOp::Dereference, .. } => {},
                    _ => return Err(SemanticError::InvalidLValue(left.span.clone())),
                }
                let hir_left = self.visit_expression(left)?;
                if let hir::ExprKind::Variable(decl) = &hir_left.kind {
                    if decl.is_const {
                        return Err(SemanticError::AssignmentToConst(left.span.clone()));
                    }
                }
                if hir_left.resolved_type != hir_right.resolved_type {
                    return Err(SemanticError::TypeMismatch { expected: hir_left.resolved_type, found: hir_right.resolved_type, span: right.span.clone() });
                }
                let resolved_type = hir_left.resolved_type.clone();
                Ok(hir::Expression { kind: hir::ExprKind::Assignment { left: Box::new(hir_left), right: Box::new(hir_right) }, span: expr.span.clone(), resolved_type })
            }
            ast::ExprKind::FunctionCall { name, args } => {
                if let Some(SymbolInfo::Function { return_type, params }) = self.symbol_table.lookup_symbol(name) {
                    let return_type = return_type.clone();
                    let params = params.clone();
                    if args.len() != params.len() {
                        return Err(SemanticError::WrongArgumentCount { expected: params.len(), found: args.len(), span: expr.span.clone() });
                    }
                    let mut hir_args = Vec::new();
                    for (arg_expr, (param_type, _)) in args.iter().zip(params.iter()) {
                        let hir_arg = self.visit_expression(arg_expr)?;
                        if hir_arg.resolved_type != *param_type {
                            return Err(SemanticError::TypeMismatch { expected: param_type.clone(), found: hir_arg.resolved_type, span: arg_expr.span.clone() });
                        }
                        hir_args.push(hir_arg);
                    }
                    Ok(hir::Expression { kind: hir::ExprKind::FunctionCall { name: name.clone(), args: hir_args }, span: expr.span.clone(), resolved_type: return_type })
                } else {
                    Err(SemanticError::NotAFunction(name.name.clone()))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser;
    use super::{Analyzer, SemanticError};

    /// [FIXED] 重写了测试辅助函数，使其在解析失败时直接 panic
    fn analyze_test_source(source: &str) -> Result<super::hir::Program, Vec<SemanticError>> {
        let (ast_program, parse_errs) = parser::parse_source(source);

        // 前置条件检查：如果解析失败，这个测试就应该 panic，因为它不是一个有效的分析器测试用例
        if !parse_errs.is_empty() {
            panic!("Test source failed to parse: {:?}", parse_errs);
        }

        let ast_program = ast_program.expect("Parsing succeeded but no AST was produced.");
        
        // 只有在解析成功后，才运行分析器
        let mut analyzer = Analyzer::new();
        analyzer.analyze(&ast_program)
    }

    #[test]
    fn test_valid_variable_declaration_and_use() {
        let source = r#"
            int main() {
                int x = 10;
                return x;
            }
        "#;
        // 现在直接调用新的辅助函数
        let result = analyze_test_source(source);
        assert!(result.is_ok(), "Should analyze successfully. Errors: {:?}", result.err());
        
        let hir = result.unwrap();
        assert_eq!(hir.functions.len(), 1);
    }

    #[test]
    fn test_error_undeclared_variable() {
        let source = r#"
            int main() {
                return x;
            }
        "#;
        let result = analyze_test_source(source);
        assert!(result.is_err(), "Should fail analysis");
        let errors = result.err().unwrap();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0], SemanticError::SymbolNotFound("Variable 'x' not found".to_string()));
    }

    #[test]
    fn test_error_type_mismatch_in_assignment() {
        let source = r#"
            int main() {
                int x = true;
            }
        "#;
        let result = analyze_test_source(source);
        assert!(result.is_err(), "Should fail analysis");
        let errors = result.err().unwrap();
        assert!(matches!(errors[0], SemanticError::TypeMismatch { .. }));
    }

    #[test]
    fn test_error_break_outside_loop() {
        let source = r#"
            int main() {
                break;
            }
        "#;
        let result = analyze_test_source(source);
        assert!(result.is_err(), "Should fail analysis");
        let errors = result.err().unwrap();
        assert!(matches!(errors[0], SemanticError::BreakOutsideLoop(_)));
    }

    #[test]
    fn test_valid_pointer_operations() {
        let source = r#"
            int main() {
                int x = 10;
                int* p = &x;
                return *p;
            }
        "#;
         let result = analyze_test_source(source);
         assert!(result.is_ok(), "Should analyze pointers successfully. Errors: {:?}", result.err());
    }

    #[test]
    fn test_error_function_redeclaration() {
        let source = r#"
            int main() { return 0; }
            void main() {}
        "#;
        let result = analyze_test_source(source);
        assert!(result.is_err(), "Should fail analysis");
        let errors = result.err().unwrap();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0], SemanticError::SymbolAlreadyExists("Symbol 'main' already defined in this scope".to_string()));
    }
}