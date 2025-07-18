// 在 src/semantic/analyzer.rs (或一个新文件中)

use crate::parser::*;
use crate::lexer::*;
use crate::diagnostic::*;
use super::symbol::{Symbol, SymbolKind, SymbolTable};
use super::types::Type;

// --- 1. 主结构体与上下文定义 ---

/// 语义分析器的主结构体，持有分析过程所需的全部状态。
pub struct SemanticAnalyzer<'a> {
    diagnostics: &'a mut DiagnosticBag,
    symbol_table: SymbolTable,
}

/// 用于在递归访问 AST 时向下传递上下文信息。
#[derive(Debug, Clone, Default)]
struct AnalysisContext {
    /// 当前所在的函数的期望返回类型。
    current_function_return_type: Option<Type>,
}

// --- 2. Trait 定义：组织分析逻辑 ---

/// `SemanticAnalysis` Trait 是语义分析器的总入口。
pub trait SemanticAnalysis {
    /// 消耗分析器并启动整个分析过程。
    /// 成功则返回 Ok(()), 失败则返回 Err(()), 错误信息已存入 diagnostics。
    fn analyze(&mut self, program: &Program) -> Result<(), ()>;
}

/// `SymbolRegistration` Trait 负责第一遍分析：注册所有顶层符号。
trait SymbolRegistration {
    /// 遍历所有顶层声明，将函数和全局变量的签名注册到全局符号表。
    fn register_symbols(&mut self, program: &Program) -> Result<(), ()>;
}

/// `DefinitionChecker` Trait 负责第二遍分析：检查所有定义的内部逻辑。
trait DefinitionChecker {
    /// 遍历所有顶层声明，深入检查函数体和全局变量的初始化表达式。
    fn check_definitions(&mut self, program: &Program) -> Result<(), ()>;
    /// 检查一个函数定义的内部。
    fn check_function_definition(&mut self, func_def: &FunctionDefinition);
    /// 检查一个全局变量定义的初始化表达式。
    fn check_global_variable_declaration(&mut self, var_decl: &VariableDeclaration);
}

/// `StatementChecker` Trait 负责检查各类语句。
trait StatementChecker {
    /// 检查任意类型的语句（作为分发函数）。
    fn check_statement(&mut self, stmt: &Node<Statement>, ctx: &mut AnalysisContext);
    
    // --- 具体的语句检查辅助函数 ---
    /// 检查 if 语句。
    fn check_if_statement(&mut self, if_stmt: &IfStatement, ctx: &mut AnalysisContext);
    /// 检查 while 语句。
    fn check_while_statement(&mut self, while_stmt: &WhileStatement, ctx: &mut AnalysisContext);
    /// 检查 return 语句。
    fn check_return_statement(&mut self, return_stmt: &ReturnStatement, stmt_span: Span, ctx: &mut AnalysisContext);
    /// 检查代码块语句。
    fn check_block_statement(&mut self, block_stmt: &BlockStatement, ctx: &mut AnalysisContext);
    /// 检查局部变量声明语句。
    fn check_local_variable_declaration(&mut self, var_decl: &VariableDeclaration, ctx: &mut AnalysisContext);
}

/// `ExpressionChecker` Trait 负责检查表达式并推断其类型。
trait ExpressionChecker {
    /// 检查任意表达式，并返回其语义类型 (分发函数)。
    fn check_expression(&mut self, expr: &Node<Expression>, ctx: &AnalysisContext) -> Type;

    // --- 具体的表达式检查辅助函数 ---
    /// 检查一元运算表达式。
    fn check_unary_expression(&mut self, unary_expr: &UnaryExpression, ctx: &AnalysisContext) -> Type;
    /// 检查二元运算表达式。
    fn check_binary_expression(&mut self, binary_expr: &BinaryExpression, ctx: &AnalysisContext) -> Type;
    /// 检查赋值表达式。
    fn check_assignment_expression(&mut self, assign_expr: &AssignmentExpression, ctx: &AnalysisContext) -> Type;
    /// 检查函数调用表达式。
    fn check_call_expression(&mut self, call_expr: &CallExpression, ctx: &AnalysisContext) -> Type;
    /// 检查标识符表达式 (变量使用)。
    fn check_identifier_expression(&mut self, ident: &Identifier) -> Type;
}

/// `TypeResolver` Trait 提供类型解析和比较的工具。
trait TypeResolver {
    /// 将 AST 中的 `ast::TypeSpec` 解析为内部的 `semantic::Type`。
    fn resolve_type_spec(&mut self, type_spec_node: &Node<TypeSpec>) -> Type;
}


// --- 3. 基础实现：入口点 ---

impl<'a> SemanticAnalyzer<'a> {
    /// 创建一个新的语义分析器实例。
    pub fn new(diagnostics: &'a mut DiagnosticBag) -> Self {
        Self {
            diagnostics,
            symbol_table: SymbolTable::new(),
        }
    }
}

/// 实现总入口 Trait
impl<'a> SemanticAnalysis for SemanticAnalyzer<'a> {
    fn analyze(&mut self, program: &Program) -> Result<(), ()> {
        // 第一遍：注册所有顶层符号
        self.register_symbols(program)?;

        // 第二遍：检查所有函数体和全局变量初始化
        self.check_definitions(program)?;

        // 如果在任何阶段出现了错误，has_errors()会返回 true
        if self.diagnostics.has_errors() {
            Err(())
        } else {
            Ok(())
        }
    }
}

/// TypeResolver 的实现，提供类型解析工具。
impl<'a> TypeResolver for SemanticAnalyzer<'a> {
    /// 将 AST 中的类型规范 (`ast::TypeSpec`) 递归地转换为内部的语义类型 (`semantic::Type`)。
    fn resolve_type_spec(&mut self, type_spec_node: &Node<TypeSpec>) -> Type {
        match &type_spec_node.kind {
            // 对于命名类型，我们通过匹配字符串来确定是哪个内建类型。
            TypeSpec::Named(ident) => match ident.name.as_str() {
                "int" => Type::Int,
                "char" => Type::Char,
                "bool" => Type::Bool,
                "void" => Type::Void,
                // c0 阶段不支持用户自定义类型，所以任何其他名字都是未知的。
                _ => {
                    let label = Label::new(ident.span, "未知的类型名");
                    self.diagnostics.report_error_with_code(
                        format!("找不到类型 `{}`", ident.name),
                        ErrorCode::E1004, // 期望一个（已知的）类型说明符
                        label,
                    );
                    Type::Error
                }
            },
            // 对于指针类型，我们递归解析其内部的类型，然后用 `Pointer` 包裹起来。
            TypeSpec::Pointer(inner_spec) => {
                let inner_type = self.resolve_type_spec(inner_spec);
                if inner_type == Type::Void {
                    let label = Label::new(type_spec_node.span, "不能创建 `void` 类型的指针");
                    self.diagnostics.report_error_with_code(
                        "非法的指针类型",
                        ErrorCode::E2001, // 可以视为一种类型不匹配
                        label,
                    );
                    return Type::Error;
                }
                Type::Pointer(Box::new(inner_type))
            }
        }
    }
}

/// SymbolRegistration 的实现，负责第一遍的符号注册。
impl<'a> SymbolRegistration for SemanticAnalyzer<'a> {
    /// 遍历所有顶层声明，将函数和全局变量的签名注册到全局符号表。
    fn register_symbols(&mut self, program: &Program) -> Result<(), ()> {
        for decl_node in &program.declarations {
            match &decl_node.kind {
                // 情况一：这是一个函数定义
                Declaration::Function(func_def) => {
                    // 1. 解析函数的返回类型和参数类型，构成完整的函数签名
                    let return_type = self.resolve_type_spec(&func_def.return_type);
                    let param_types = func_def.params
                        .iter()
                        .map(|p| self.resolve_type_spec(&p.kind.type_spec))
                        .collect();

                    let func_type = Type::Function {
                        param_types,
                        return_type: Box::new(return_type),
                    };

                    // 2. 创建一个函数符号
                    let symbol = Symbol {
                        kind: SymbolKind::Function,
                        ty: func_type,
                        defined_at: func_def.name.span,
                    };
                    
                    // 3. 将符号定义到全局符号表中，并处理重复定义错误
                    if !self.symbol_table.define(func_def.name.name.clone(), symbol) {
                        // 如果 define 返回 false，说明发生了重复定义
                        let existing = self.symbol_table.resolve(&func_def.name.name).unwrap(); // 此时一定能找到
                        let label1 = Label::new(func_def.name.span, "重复的函数定义");
                        let label2 = Label::new(existing.defined_at, "之前的定义在这里");
                        // 1. 先使用 Diagnostic 的构建器方法，创建一个完整的诊断对象
                        let diag = Diagnostic::error("重复定义", label1) // 创建基础错误
                            .with_code(ErrorCode::E2003)              // 添加错误码
                            .with_secondary_label(label2);                      // 添加第二个标签

                        // 2. 然后将这个构建好的对象报告给 DiagnosticBag
                        self.diagnostics.report(diag);
                    }
                }
                // 情况二：这是一个全局变量声明
                Declaration::Variable(var_decl) => {
                    // 1. 解析变量的类型
                    let var_type = self.resolve_type_spec(&var_decl.type_spec);

                    // 不允许声明 void 类型的变量
                    if var_type == Type::Void {
                        let label = Label::new(var_decl.type_spec.span, "变量不能被声明为 `void` 类型");
                        self.diagnostics.report_error_with_code("非法的变量类型", ErrorCode::E2001, label);
                    }
                    
                    // 2. 创建一个变量符号
                    let symbol = Symbol {
                        kind: SymbolKind::Variable,
                        ty: var_type,
                        defined_at: var_decl.name.span,
                    };

                    // 3. 将符号定义到全局符号表中，并处理重复定义错误
                    if !self.symbol_table.define(var_decl.name.name.clone(), symbol) {
                        let existing = self.symbol_table.resolve(&var_decl.name.name).unwrap();
                        let label1 = Label::new(var_decl.name.span, "重复的全局变量定义");
                        let label2 = Label::new(existing.defined_at, "之前的定义在这里");
                        let diag = Diagnostic::error("重复定义", label1) 
                            .with_secondary_label(label2);                      
                        self.diagnostics.report(diag);
                    }
                }
            }
        }
        // 在我们的设计中，第一遍分析只负责收集信息和报告错误，
        // 而不中断整个流程。最终是否成功由 analyze() 函数统一检查。
        Ok(())
    }
}

/// DefinitionChecker 的实现，负责第二遍的定义检查。
impl<'a> DefinitionChecker for SemanticAnalyzer<'a> {
    /// 遍历所有顶层声明，分派到更具体的检查函数。
    fn check_definitions(&mut self, program: &Program) -> Result<(), ()> {
        for decl_node in &program.declarations {
            match &decl_node.kind {
                Declaration::Function(func_def) => {
                    self.check_function_definition(func_def);
                }
                Declaration::Variable(var_decl) => {
                    self.check_global_variable_declaration(var_decl);
                }
            }
        }
        Ok(())
    }

    /// 检查一个全局变量定义的初始化表达式。
    fn check_global_variable_declaration(&mut self, var_decl: &VariableDeclaration) {
        // 1. 获取该变量在第一遍分析中注册的类型。
        let var_symbol = self.symbol_table.resolve(&var_decl.name.name).unwrap(); // 此时一定能找到
        let declared_type = var_symbol.ty.clone();

        // 2. 检查初始化表达式（如果存在）。
        if let Some(initializer) = &var_decl.initializer {
            // C 语言要求全局变量的初始化必须是常量表达式。
            // 为简化 c0 的实现，我们规定全局变量的初始值只能是字面量。
            if !matches!(initializer.kind, Expression::Literal(_)) {
                let label = Label::new(initializer.span, "全局变量的初始值必须是一个编译期常量（如此处应为一个字面量）");
                self.diagnostics.report_error_with_code(
                    "非法的初始化表达式",
                    ErrorCode::E2012, // (需要添加的新错误码) 全局变量初始化必须是常量
                    label,
                );
                return; // 出错后不再进行后续类型检查
            }

            // 3. 对初始化表达式进行类型检查。
            // 全局作用域没有特殊的上下文，所以我们使用默认上下文。
            let init_type = self.check_expression(initializer, &AnalysisContext::default());

            // 4. 比较声明类型和初始值类型是否匹配。
            if declared_type != init_type && init_type != Type::Error {
                let label = Label::new(
                    initializer.span,
                    format!("期望类型是 `{}`，但得到的表达式类型是 `{}`", declared_type, init_type),
                );
                self.diagnostics.report_error_with_code("类型不匹配", ErrorCode::E2001, label);
            }
        }
    }

    /// 检查一个函数定义的内部（参数、函数体等）。
    fn check_function_definition(&mut self, func_def: &FunctionDefinition) {
        // 1. 从符号表中获取此函数的类型签名（在第一遍中注册的）。
        let func_symbol = self.symbol_table.resolve(&func_def.name.name).unwrap();
        let (param_types, return_type) = if let Type::Function { param_types, return_type } = &func_symbol.ty {
            (param_types.clone(), *return_type.clone())
        } else {
            // 这是一个内部逻辑错误，理论上不应发生
            unreachable!();
        };

        // 2. 创建一个新的分析上下文，传入函数的返回类型。
        let mut ctx = AnalysisContext {
            current_function_return_type: Some(return_type.clone()),
        };

        // 3. 进入一个新的作用域，用于存放函数参数和局部变量。
        self.symbol_table.enter_scope();

        // 4. 将所有参数定义为新作用域内的变量。
        for (param_node, param_type) in func_def.params.iter().zip(param_types.iter()) {
            let symbol = Symbol {
                kind: SymbolKind::Variable, // 在函数体内，参数就是局部变量
                ty: param_type.clone(),
                defined_at: param_node.kind.name.span,
            };
            if !self.symbol_table.define(param_node.kind.name.name.clone(), symbol) {
                // 这个错误理论上在第一遍的函数签名解析时就可以被发现，但这里做一个双重保险。
                let label = Label::new(param_node.kind.name.span, "重复的参数名");
                self.diagnostics.report_error_with_code("重复定义", ErrorCode::E2003, label);
            }
        }

        // 5. 递归地检查函数体内的所有语句。
        // check_block_statement 会使用我们传入的 ctx 来检查 return 语句的类型是否正确。
        self.check_block_statement(&func_def.body.kind, &mut ctx);

        // 6. 检查非 void 函数是否至少有一条 return 语句。
        // (这是一个简化的检查，只检查函数体的直接子语句)
        if return_type != Type::Void {
            let has_return = func_def.body.kind.statements.iter().any(|stmt| matches!(stmt.kind, Statement::Return(_)));
            if !has_return {
                let label = Label::new(func_def.name.span, "这个函数声明了返回值，但可能没有 `return` 语句");
                self.diagnostics.report_error_with_code(
                    "函数可能未在所有路径返回值",
                    ErrorCode::E2013, // (需要添加的新错误码) 函数可能未返回值
                    label,
                );
            }
        }

        // 7. 完成检查后，离开函数的作用域。
        self.symbol_table.leave_scope();
    }
}

impl<'a> StatementChecker for SemanticAnalyzer<'a> {
    /// 检查语句的总入口，根据语句的类型分发到具体的检查函数。
    fn check_statement(&mut self, stmt: &Node<Statement>, ctx: &mut AnalysisContext) {
        match &stmt.kind {
            Statement::Expression(expr) => {
                self.check_expression(expr, ctx);
            }
            Statement::VariableDeclaration(var_decl) => {
                self.check_local_variable_declaration(var_decl, ctx);
            }
            Statement::If(if_stmt) => {
                self.check_if_statement(if_stmt, ctx);
            }
            Statement::While(while_stmt) => {
                self.check_while_statement(while_stmt, ctx);
            }
            Statement::Return(return_stmt) => {
                self.check_return_statement(return_stmt, stmt.span, ctx);
            }
            Statement::Block(block_stmt) => {
                self.check_block_statement(block_stmt, ctx);
            }
        }
    }

    /// 检查在函数内部声明的局部变量。
    fn check_local_variable_declaration(&mut self, var_decl: &VariableDeclaration, ctx: &mut AnalysisContext) {
        // 1. 解析声明的类型
        let declared_type = self.resolve_type_spec(&var_decl.type_spec);
        if declared_type == Type::Void {
            let label = Label::new(var_decl.type_spec.span, "变量不能被声明为 `void` 类型");
            self.diagnostics.report_error_with_code("非法的变量类型", ErrorCode::E2001, label);
        }

        // 2. 如果有初始化表达式，检查其类型是否与声明的类型匹配
        if let Some(initializer) = &var_decl.initializer {
            let init_type = self.check_expression(initializer, ctx);
            if declared_type != init_type && init_type != Type::Error {
                let label = Label::new(
                    initializer.span,
                    format!("期望类型是 `{}`，但得到的表达式类型是 `{}`", declared_type, init_type),
                );
                self.diagnostics.report_error_with_code("类型不匹配", ErrorCode::E2001, label);
            }
        }
        
        // 3. 在当前作用域定义这个新变量，并检查是否重复定义
        let symbol = Symbol {
            kind: SymbolKind::Variable,
            ty: declared_type,
            defined_at: var_decl.name.span,
        };
        if !self.symbol_table.define(var_decl.name.name.clone(), symbol) {
            let existing = self.symbol_table.resolve(&var_decl.name.name).unwrap();
            let label1 = Label::new(var_decl.name.span, "此作用域内已存在同名变量");
            let label2 = Label::new(existing.defined_at, "之前的定义在这里");
            let diag = Diagnostic::error("重复定义", label1) 
                .with_code(ErrorCode::E2003)              
                .with_secondary_label(label2);          
            self.diagnostics.report(diag);
        }
    }

    /// 检查 if 语句。
    fn check_if_statement(&mut self, if_stmt: &IfStatement, ctx: &mut AnalysisContext) {
        // 1. 检查条件表达式的类型必须是 bool
        let cond_type = self.check_expression(&if_stmt.condition, ctx);
        if cond_type != Type::Bool && cond_type != Type::Error {
            let label = Label::new(if_stmt.condition.span, format!("期望 `bool` 类型，但得到的是 `{}`", cond_type));
            self.diagnostics.report_error_with_code("if 条件类型错误", ErrorCode::E2008, label);
        }

        // 2. 递归检查 `then` 分支
        self.check_statement(&if_stmt.then_block, ctx);
        
        // 3. 如果存在 `else` 分支，递归检查它
        if let Some(else_branch) = &if_stmt.else_branch {
            self.check_statement(else_branch, ctx);
        }
    }

    /// 检查 while 语句。
    fn check_while_statement(&mut self, while_stmt: &WhileStatement, ctx: &mut AnalysisContext) {
        // 1. 检查条件表达式的类型必须是 bool
        let cond_type = self.check_expression(&while_stmt.condition, ctx);
        if cond_type != Type::Bool && cond_type != Type::Error {
            let label = Label::new(while_stmt.condition.span, format!("期望 `bool` 类型，但得到的是 `{}`", cond_type));
            self.diagnostics.report_error_with_code("while 条件类型错误", ErrorCode::E2008, label);
        }

        // 2. 递归检查循环体
        self.check_statement(&while_stmt.body, ctx);
    }

    /// 检查 return 语句。
    fn check_return_statement(&mut self, return_stmt: &ReturnStatement, stmt_span: Span, ctx: &mut AnalysisContext) {
        // 1. 从上下文中获取当前函数期望的返回类型
        let expected_type = match &ctx.current_function_return_type {
            Some(ty) => ty.clone(),
            // 如果上下文不存在返回类型（例如，在全局作用域写 return），这是个错误
            None => {
                let label = Label::new(stmt_span, "不能在函数外部使用 `return`");
                self.diagnostics.report_error("非法的 `return` 语句", label);
                return;
            }
        };

        // 2. 确定实际返回的类型
        let actual_type = match &return_stmt.value {
            // 如果有返回值表达式，检查它的类型
            Some(expr) => self.check_expression(expr, ctx),
            // 如果没有返回值表达式（即 `return;`），那么它的类型就是 void
            None => Type::Void,
        };

        // 3. 比较期望类型和实际类型
        if actual_type != Type::Error && expected_type != actual_type {
            let label = Label::new(
                return_stmt.value.as_ref().map_or(stmt_span, |n| n.span),
                format!("期望返回类型是 `{}`，但得到的是 `{}`", expected_type, actual_type)
            );
            self.diagnostics.report_error_with_code("返回类型不匹配", ErrorCode::E2007, label);
        }
    }

    /// 检查一个代码块语句，核心是管理作用域。
    fn check_block_statement(&mut self, block_stmt: &BlockStatement, ctx: &mut AnalysisContext) {
        // 1. 进入一个新的作用域
        self.symbol_table.enter_scope();
        
        // 2. 遍历并检查代码块中的每一条语句
        for stmt in &block_stmt.statements {
            self.check_statement(stmt, ctx);
        }
        
        // 3. 离开作用域，此作用域中定义的所有局部变量随之销毁
        self.symbol_table.leave_scope();
    }
}

impl<'a> ExpressionChecker for SemanticAnalyzer<'a> {
    /// 表达式检查的总入口，根据表达式的种类分发到具体的检查函数。
    fn check_expression(&mut self, expr: &Node<Expression>, ctx: &AnalysisContext) -> Type {
        match &expr.kind {
            Expression::Literal(lit) => match lit {
                Literal::Integer(_) => Type::Int,
                Literal::Char(_) => Type::Char,
                Literal::String(_) => Type::Pointer(Box::new(Type::Char)), // 字符串字面量的类型是 `char*`
                Literal::Boolean(_) => Type::Bool,
            },
            Expression::Identifier(ident) => self.check_identifier_expression(ident),
            Expression::Unary(unary_expr) => self.check_unary_expression(unary_expr, ctx),
            Expression::Binary(binary_expr) => self.check_binary_expression(binary_expr, ctx),
            Expression::Assignment(assign_expr) => self.check_assignment_expression(assign_expr, ctx),
            Expression::Call(call_expr) => self.check_call_expression(call_expr, ctx),
        }
    }
    
    /// 检查一个标识符的使用，并在符号表中查找它。
    fn check_identifier_expression(&mut self, ident: &Identifier) -> Type {
        if let Some(symbol) = self.symbol_table.resolve(&ident.name) {
            // 如果找到了符号，返回其类型
            symbol.ty.clone()
        } else {
            // 如果没找到，报告“未声明的标识符”错误
            let label = Label::new(ident.span, "在此作用域内找不到这个变量或函数");
            self.diagnostics.report_error_with_code(
                format!("使用了未声明的标识符 `{}`", ident.name),
                ErrorCode::E2002,
                label,
            );
            Type::Error
        }
    }

    /// 检查一元运算表达式。
    fn check_unary_expression(&mut self, unary_expr: &UnaryExpression, ctx: &AnalysisContext) -> Type {
        let operand_type = self.check_expression(&unary_expr.operand, ctx);
        if operand_type == Type::Error { return Type::Error; }

        match unary_expr.operator.kind {
            // 负号 `-`：操作数必须是 int
            Operator::Minus => {
                if operand_type == Type::Int {
                    Type::Int
                } else {
                    let label = Label::new(unary_expr.operand.span, format!("期望 `int` 类型，但得到 `{}`", operand_type));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2006, label);
                    Type::Error
                }
            }
            // 逻辑非 `!`：操作数必须是 bool
            Operator::Not => {
                if operand_type == Type::Bool {
                    Type::Bool
                } else {
                    let label = Label::new(unary_expr.operand.span, format!("期望 `bool` 类型，但得到 `{}`", operand_type));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2006, label);
                    Type::Error
                }
            }
            // 取地址 `&`：操作数必须是一个合法的左值（在 c0 中即为一个标识符）
            Operator::And => {
                if matches!(unary_expr.operand.kind, Expression::Identifier(_)) {
                    Type::Pointer(Box::new(operand_type))
                } else {
                    let label = Label::new(unary_expr.operand.span, "只有变量可以被取地址");
                    self.diagnostics.report_error_with_code("非法的取地址操作", ErrorCode::E2011, label);
                    Type::Error
                }
            }
            // 解引用 `*`：操作数必须是指针
            Operator::Star => {
                if let Type::Pointer(inner_type) = operand_type {
                    *inner_type
                } else {
                    let label = Label::new(unary_expr.operand.span, format!("期望指针类型，但得到 `{}`", operand_type));
                    self.diagnostics.report_error_with_code("解引用了非指针类型", ErrorCode::E2009, label);
                    Type::Error
                }
            }
            _ => unreachable!(), // 其他操作符不可能是前缀
        }
    }

    /// 检查二元运算表达式。
    fn check_binary_expression(&mut self, binary_expr: &BinaryExpression, ctx: &AnalysisContext) -> Type {
        let left_type = self.check_expression(&binary_expr.left, ctx);
        let right_type = self.check_expression(&binary_expr.right, ctx);
        if left_type == Type::Error || right_type == Type::Error { return Type::Error; }

        let op = binary_expr.operator.kind;
        match op {
            // 算术运算: 操作数必须都是 int，结果也是 int
            Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash | Operator::Percent => {
                if left_type == Type::Int && right_type == Type::Int {
                    Type::Int
                } else {
                    let full_span = binary_expr.left.span.to(binary_expr.right.span);
                    let label = Label::new(full_span, format!("不能对 `{}` 和 `{}` 类型应用 `{}` 操作", left_type, right_type, op));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2006, label);
                    Type::Error
                }
            }
            // 关系运算: 操作数必须都是 int，结果是 bool
            Operator::Lt | Operator::Gt | Operator::LtEq | Operator::GtEq => {
                if left_type == Type::Int && right_type == Type::Int {
                    Type::Bool
                } else {
                    let full_span = binary_expr.left.span.to(binary_expr.right.span);
                    let label = Label::new(full_span, format!("不能对 `{}` 和 `{}` 类型应用 `{}` 操作", left_type, right_type, op));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2006, label);
                    Type::Error
                }
            }
            // 相等运算: 操作数类型必须相同，结果是 bool
            Operator::Eq | Operator::NotEq => {
                if left_type == right_type {
                    Type::Bool
                } else {
                    let full_span = binary_expr.left.span.to(binary_expr.right.span);
                    let label = Label::new(full_span, format!("不能对 `{}` 和 `{}` 类型应用 `{}` 操作", left_type, right_type, op));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2001, label);
                    Type::Error
                }
            }
            // 逻辑运算: 操作数必须都是 bool，结果也是 bool
            Operator::AndAnd | Operator::OrOr => {
                if left_type == Type::Bool && right_type == Type::Bool {
                    Type::Bool
                } else {
                    let full_span = binary_expr.left.span.to(binary_expr.right.span);
                    let label = Label::new(full_span, format!("不能对 `{}` 和 `{}` 类型应用 `{}` 操作", left_type, right_type, op));
                    self.diagnostics.report_error_with_code("非法的操作数类型", ErrorCode::E2006, label);
                    Type::Error
                }
            }
            _ => unreachable!(),
        }
    }

    /// 检查赋值表达式。
    fn check_assignment_expression(&mut self, assign_expr: &AssignmentExpression, ctx: &AnalysisContext) -> Type {
        // 1. 检查左边是否是合法的“左值”（可以被赋值的东西）
        match &assign_expr.lvalue.kind {
            Expression::Identifier(_) | Expression::Unary(_) => {
                // 对于 c0，我们允许对变量和指针解引用进行赋值，这两种情况都可接受。
            }
            _ => {
                let label = Label::new(assign_expr.lvalue.span, "表达式的左侧必须是一个可赋值的变量或内存位置");
                self.diagnostics.report_error_with_code("非法的赋值目标", ErrorCode::E2010, label);
                return Type::Error;
            }
        }

        // 2. 检查类型是否匹配
        let left_type = self.check_expression(&assign_expr.lvalue, ctx);
        let right_type = self.check_expression(&assign_expr.rvalue, ctx);

        if left_type == Type::Error || right_type == Type::Error { return Type::Error; }

        if left_type != right_type {
            let label = Label::new(assign_expr.rvalue.span, format!("期望类型是 `{}`，但得到的表达式类型是 `{}`", left_type, right_type));
            self.diagnostics.report_error_with_code("赋值时类型不匹配", ErrorCode::E2001, label);
        }

        // 赋值表达式本身的值和类型就是左值的类型
        left_type
    }

    /// 检查函数调用表达式。
    fn check_call_expression(&mut self, call_expr: &CallExpression, ctx: &AnalysisContext) -> Type {
        let callee_type = self.check_expression(&call_expr.callee, ctx);

        if let Type::Function { param_types, return_type } = callee_type {
            // 1. 检查参数数量
            if call_expr.arguments.len() != param_types.len() {
                let label = Label::new(call_expr.callee.span, format!("期望 {} 个参数，但得到了 {} 个", param_types.len(), call_expr.arguments.len()));
                self.diagnostics.report_error_with_code("函数调用参数数量错误", ErrorCode::E2005, label);
                return *return_type; // 即使参数数量错误，也返回预期的返回类型，以减少连锁错误
            }
            // 2. 逐个检查参数类型
            for (arg_expr, expected_type) in call_expr.arguments.iter().zip(param_types.iter()) {
                let arg_type = self.check_expression(arg_expr, ctx);
                if arg_type != Type::Error && &arg_type != expected_type {
                    let label = Label::new(arg_expr.span, format!("期望类型 `{}`，但此参数的类型是 `{}`", expected_type, arg_type));
                    self.diagnostics.report_error_with_code("函数调用参数类型不匹配", ErrorCode::E2001, label);
                }
            }
            // 3. 返回函数的返回类型
            *return_type
        } else if callee_type != Type::Error {
            let label = Label::new(call_expr.callee.span, format!("此表达式的类型是 `{}`，它不是一个函数，不能被调用", callee_type));
            self.diagnostics.report_error_with_code("尝试调用非函数类型", ErrorCode::E2004, label);
            Type::Error
        } else {
            Type::Error
        }
    }
}