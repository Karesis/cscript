use crate::parser::*;
use crate::lexer::*;
use crate::semantic::{Type as SemanticType, SymbolTable}; // 使用别名以区分 AST 类型
use crate::diagnostic::DiagnosticBag;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::types::{BasicTypeEnum, FunctionType, BasicType, BasicMetadataTypeEnum};

use std::collections::HashMap;

// --- 1. 主代码生成器结构体 ---

/// 代码生成器，负责将 AST 最终转换为 LLVM IR。
pub struct CodeGenerator<'ctx, 'sym> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // 持有对符号表的引用
    symbol_table: &'sym SymbolTable,
    
    /// 用于存储【局部变量】在栈上的内存地址（指针）。
    variables: HashMap<String, PointerValue<'ctx>>,
    
    /// 指向当前正在生成的函数，用于处理 return 语句。
    current_function: Option<FunctionValue<'ctx>>,
}

// --- 2. Trait 定义：组织代码生成逻辑 ---

/// `CodeGen` Trait 是代码生成的总入口。
pub trait CodeGen {
    /// 消耗代码生成器，遍历整个程序 AST，最终返回包含 LLVM IR 的字符串。
    fn codegen(self, program: &Program) -> Result<String, ()>;
}

/// `DeclarationCodeGen` Trait 负责处理顶层声明。
trait DeclarationCodeGen {
    /// 生成一个顶层声明的代码（分发函数）。
    fn codegen_declaration(&mut self, decl: &Node<Declaration>);
    /// 为一个函数定义生成代码（包括函数签名和函数体）。
    fn codegen_function_definition(&mut self, func_def: &FunctionDefinition);
    /// 为一个全局变量生成代码。
    fn codegen_global_variable(&mut self, var_decl: &VariableDeclaration);
}

/// `StatementCodeGen` Trait 负责为各类语句生成代码。
trait StatementCodeGen {
    /// 为任意语句生成代码（分发函数）。
    fn codegen_statement(&mut self, stmt: &Node<Statement>);
    /// 为 if 语句生成代码（涉及基本块和分支）。
    fn codegen_if_statement(&mut self, if_stmt: &IfStatement);
    /// 为 while 语句生成代码（涉及基本块和分支）。
    fn codegen_while_statement(&mut self, while_stmt: &WhileStatement);
    /// 为 return 语句生成代码。
    fn codegen_return_statement(&mut self, return_stmt: &ReturnStatement);
    /// 为代码块生成代码（涉及作用域管理）。
    fn codegen_block_statement(&mut self, block_stmt: &BlockStatement);
    /// 为局部变量声明生成代码（在栈上分配空间）。
    fn codegen_local_variable_declaration(&mut self, var_decl: &VariableDeclaration);
}

/// `ExpressionCodeGen` Trait 负责为表达式生成代码并返回其 LLVM 值。
trait ExpressionCodeGen<'ctx> {
    /// 为任意表达式生成代码，并返回其对应的 LLVM `Value`（分发函数）。
    fn codegen_expression(&mut self, expr: &Node<Expression>) -> Result<BasicValueEnum<'ctx>, String>;
    
    // --- 具体的表达式生成辅助函数 ---
    /// 为标识符（变量使用）生成代码，执行 `load` 指令。
    fn codegen_identifier_load(&mut self, ident: &Identifier) -> Result<BasicValueEnum<'ctx>, String>;
    /// 为一元运算表达式生成代码。
    fn codegen_unary_expression(&mut self, unary_expr: &UnaryExpression) -> Result<BasicValueEnum<'ctx>, String>;
    /// 为二元运算表达式生成代码。
    fn codegen_binary_expression(&mut self, binary_expr: &BinaryExpression) -> Result<BasicValueEnum<'ctx>, String>;
    /// 为赋值表达式生成代码，执行 `store` 指令。
    fn codegen_assignment_expression(&mut self, assign_expr: &AssignmentExpression) -> Result<BasicValueEnum<'ctx>, String>;
    /// 为函数调用表达式生成代码。
    fn codegen_call_expression(&mut self, call_expr: &CallExpression) -> Result<BasicValueEnum<'ctx>, String>;
}

/// `TypeCodeGen` Trait 是一个工具集，用于将我们自己的语义类型转换为 `inkwell` 的 LLVM 类型。
trait TypeCodeGen<'ctx> {
    /// 将 `semantic::Type` 转换为 `inkwell` 中的基础类型（如 i32, i8*）。
    fn codegen_basic_type(&self, ty: &SemanticType) -> BasicTypeEnum<'ctx>;
    /// 将 `semantic::Type::Function` 转换为 `inkwell` 中的函数类型。
    fn codegen_function_type(&self, ty: &SemanticType) -> FunctionType<'ctx>;
}


// --- 3. 基础实现：入口点 ---

impl<'ctx, 'sym> CodeGenerator<'ctx, 'sym> {
    /// 创建一个新的代码生成器实例。
    pub fn new(
        context: &'ctx Context,
        symbol_table: &'sym SymbolTable // <-- 传入符号表
    ) -> Self {
        let module = context.create_module("cscript_module");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            symbol_table, // <-- 保存符号表
            variables: HashMap::new(),
            current_function: None,
        }
    }
}

/// 实现总入口 Trait
impl<'ctx, 'sym> CodeGen for CodeGenerator<'ctx, 'sym> {
    fn codegen(mut self, program: &Program) -> Result<String, ()> {
        for decl in &program.declarations {
            self.codegen_declaration(decl);
        }
        
        // 在这里可以添加验证模块正确性的代码
        // self.module.verify().map_err(|e| ...)?;

        Ok(self.module.print_to_string().to_string())
    }
}

/// TypeCodeGen 的实现，负责类型转换。
impl<'ctx, 'sym> TypeCodeGen<'ctx> for CodeGenerator<'ctx, 'sym> {
    /// 将我们的语义类型，转换为 inkwell 的基础类型。
    fn codegen_basic_type(&self, ty: &SemanticType) -> BasicTypeEnum<'ctx> {
        match ty {
            SemanticType::Int => self.context.i32_type().into(), // c0 的 int 统一为 32位
            SemanticType::Bool => self.context.bool_type().into(), // i1 类型
            SemanticType::Char => self.context.i8_type().into(),
            SemanticType::Pointer(inner) => {
                let inner_llvm_type = self.codegen_basic_type(inner);
                inner_llvm_type.ptr_type(inkwell::AddressSpace::default()).into()
            }
            // void 不是一个“基础类型”，它不能被存入变量，所以在这里是未处理的
            _ => unimplemented!("不支持的类型转换: {}", ty),
        }
    }

    /// 将我们的函数类型，转换为 inkwell 的函数类型。
    fn codegen_function_type(&self, ty: &SemanticType) -> FunctionType<'ctx> {
        if let SemanticType::Function { param_types, return_type } = ty {
            // --- 修改开始 ---
            let params_llvm_types: Vec<BasicMetadataTypeEnum> = param_types // 1. 容器类型改为 BasicMetadataTypeEnum
                .iter()
                .map(|t| self.codegen_basic_type(t)) // 2. 得到一个 BasicTypeEnum
                .map(|t| t.into())                  // 3. 将每个 BasicTypeEnum 转换为 BasicMetadataTypeEnum
                .collect();
            // --- 修改结束 ---

            if **return_type == SemanticType::Void {
                self.context.void_type().fn_type(&params_llvm_types, false)
            } else {
                let return_llvm_type = self.codegen_basic_type(return_type);
                return_llvm_type.fn_type(&params_llvm_types, false)
            }
        } else {
            unreachable!("codegen_function_type 只应被用于函数类型");
        }
    }
}


/// DeclarationCodeGen 的实现，负责处理顶层声明。
impl<'ctx, 'sym> DeclarationCodeGen for CodeGenerator<'ctx, 'sym> {
    /// 顶层声明的分发函数。
    fn codegen_declaration(&mut self, decl: &Node<Declaration>) {
        match &decl.kind {
            Declaration::Function(func_def) => self.codegen_function_definition(func_def),
            Declaration::Variable(var_decl) => self.codegen_global_variable(var_decl),
        }
    }

    /// 为一个全局变量生成代码。
    fn codegen_global_variable(&mut self, var_decl: &VariableDeclaration) {
        // 1. 从符号表获取变量的语义类型
        let symbol = self.symbol_table.resolve(&var_decl.name.name).unwrap();
        let var_type = self.codegen_basic_type(&symbol.ty);
        
        // 2. 在 LLVM Module 中添加一个全局变量
        let global = self.module.add_global(var_type, None, &var_decl.name.name);

        // 3. 处理初始化（c0 要求全局变量的初始值必须是常量）
        if let Some(initializer) = &var_decl.initializer {
            if let Expression::Literal(lit) = &initializer.kind {
                let const_val = match lit {
                    Literal::Integer(i) => self.context.i32_type().const_int(*i as u64, true),
                    Literal::Char(c) => self.context.i8_type().const_int(*c as u64, false),
                    Literal::Boolean(b) => self.context.bool_type().const_int(*b as u64, false),
                    // 全局字符串字面量初始化更复杂，c0 暂不处理
                    _ => unimplemented!(),
                };
                global.set_initializer(&const_val);
            }
        }
    }

    /// 为一个函数定义生成完整的代码。
    fn codegen_function_definition(&mut self, func_def: &FunctionDefinition) {
        // 1. 获取函数的类型签名
        let func_symbol = self.symbol_table.resolve(&func_def.name.name).unwrap();
        let func_type = self.codegen_function_type(&func_symbol.ty);
        
        // 2. 在 LLVM Module 中添加函数
        let function = self.module.add_function(&func_def.name.name, func_type, None);
        self.current_function = Some(function);

        // 3. 创建函数的入口基本块 (entry basic block)
        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        // 4. 为函数参数在栈上分配空间，并存储初始值
        self.variables.clear(); // 清空上一函数的局部变量
        for (i, param_ast) in func_def.params.iter().enumerate() {
            let param_name = &param_ast.kind.name.name;
            let param_val = function.get_nth_param(i as u32).unwrap();
            param_val.set_name(param_name);
            
            // a. 在栈上为参数分配一块内存
            let alloca = self.builder.build_alloca(param_val.get_type(), param_name).unwrap();
            // b. 将传入的参数值存储到这块内存中
            self.builder.build_store(alloca, param_val);
            // c. 在我们的变量表中记录下这个参数的内存地址
            self.variables.insert(param_name.clone(), alloca);
        }

        // 5. 生成函数体的代码
        self.codegen_block_statement(&func_def.body.kind);
        
        // 6. 检查并添加隐式的 `return void`
        // 如果一个 void 函数的最后一个基本块没有终结指令（即没有 return），
        // LLVM 要求我们显式地添加一个 `ret void`。
        // 首先，我们用模式匹配来安全地检查函数类型
        if let SemanticType::Function { return_type, .. } = &func_symbol.ty {
            // 检查函数的返回类型是否确实是 Void
            if **return_type == SemanticType::Void {
                // 然后，我们检查当前 builder 所在的最后一个基本块是否还没有终结者指令
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_return(None); // 如果没有，就为它添加一个 `ret void`
                }
            }
        }

        // 7. 完成检查后，清理上下文
        self.current_function = None;
    }
}

/// StatementCodeGen 的实现，负责为各类语句生成代码。
impl<'ctx, 'sym> StatementCodeGen for CodeGenerator<'ctx, 'sym> {
    /// 为任意语句生成代码（分发函数）。
    fn codegen_statement(&mut self, stmt: &Node<Statement>) {
        // 使用 match 将不同种类的语句分发到各自具体的生成函数中
        match &stmt.kind {
            Statement::Expression(expr) => {
                // 对于表达式语句，我们只关心其副作用（如赋值、函数调用），
                // 所以我们生成表达式的代码，但忽略其返回值。
                self.codegen_expression(expr);
            }
            Statement::VariableDeclaration(var_decl) => {
                self.codegen_local_variable_declaration(var_decl);
            }
            Statement::If(if_stmt) => self.codegen_if_statement(if_stmt),
            Statement::While(while_stmt) => self.codegen_while_statement(while_stmt),
            Statement::Return(return_stmt) => self.codegen_return_statement(return_stmt),
            Statement::Block(block_stmt) => self.codegen_block_statement(block_stmt),
        }
    }

    /// 为局部变量声明生成代码。
    fn codegen_local_variable_declaration(&mut self, var_decl: &VariableDeclaration) {
        // 1. 获取变量的语义类型，并转换为 LLVM 类型。
        let symbol = self.symbol_table.resolve(&var_decl.name.name).unwrap();
        let var_type = self.codegen_basic_type(&symbol.ty);

        // 2. 在当前函数的栈帧上为变量分配内存空间。
        let alloca = self.builder.build_alloca(var_type, &var_decl.name.name).unwrap();

        // 3. 将变量名和其在栈上的地址（指针）存入我们的变量表中。
        self.variables.insert(var_decl.name.name.clone(), alloca);

        // 4. 如果有初始化表达式，生成其代码并将其值存入分配好的内存中。
        if let Some(initializer) = &var_decl.initializer {
            // 注意：这里我们假定 codegen_expression 已经实现
            if let Ok(init_val) = self.codegen_expression(initializer) {
                self.builder.build_store(alloca, init_val);
            }
        }
    }

    /// 为 return 语句生成代码。
    fn codegen_return_statement(&mut self, return_stmt: &ReturnStatement) {
        if let Some(value_expr) = &return_stmt.value {
            // 如果有返回值，生成表达式的代码
            if let Ok(return_val) = self.codegen_expression(value_expr) {
                self.builder.build_return(Some(&return_val));
            }
        } else {
            // 如果是 `return;`，则生成 `ret void`
            self.builder.build_return(None);
        }
    }

    /// 为代码块 `{...}` 生成代码。
    fn codegen_block_statement(&mut self, block_stmt: &BlockStatement) {
        // 注意：语义分析阶段已经处理了作用域的创建和销毁。
        // 在代码生成阶段，局部变量的生命周期由 LLVM 的 SSA 形式和内存指令自动管理。
        // 我们只需按顺序为代码块中的每一条语句生成代码即可。
        for stmt in &block_stmt.statements {
            self.codegen_statement(stmt);
        }
    }

    /// 为 if 语句生成代码，这是控制流生成的开始。
    fn codegen_if_statement(&mut self, if_stmt: &IfStatement) {
        let function = self.current_function.unwrap();

        let condition = self.codegen_expression(&if_stmt.condition).unwrap().into_int_value();

        // 创建 then, else, 和 merge 基本块
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "if_cont");

        self.builder.build_conditional_branch(condition, then_bb, else_bb);

        // --- 生成 then 块的代码 ---
        self.builder.position_at_end(then_bb);
        self.codegen_statement(&if_stmt.then_block);
        
        // 只有在当前块没有被终结时，才添加跳转指令
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb);
        }

        // --- 生成 else 块的代码 ---
        self.builder.position_at_end(else_bb);
        if let Some(else_branch) = &if_stmt.else_branch {
            self.codegen_statement(else_branch);
        }
        // 同样，只有在当前块没有被终结时，才添加跳转指令
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb);
        }
        // --- 将 builder 定位到 merge 块 ---
        self.builder.position_at_end(merge_bb);
    }

    /// 为 while 语句生成代码。
    fn codegen_while_statement(&mut self, while_stmt: &WhileStatement) {
        let function = self.current_function.unwrap();
        
        // 1. 创建三个基本块：一个用于循环条件判断（header），一个用于循环体（body），
        // 一个用于循环结束后的代码（exit）。
        let header_bb = self.context.append_basic_block(function, "loop_header");
        let body_bb = self.context.append_basic_block(function, "loop_body");
        let exit_bb = self.context.append_basic_block(function, "loop_exit");
        
        // 2. 从当前块无条件跳转到循环头，开始循环。
        self.builder.build_unconditional_branch(header_bb);
        
        // 3. 生成循环头的代码
        self.builder.position_at_end(header_bb);
        // a. 计算循环条件
        let condition = self.codegen_expression(&while_stmt.condition).unwrap();
        let condition_bool = condition.into_int_value();
        // b. 根据条件决定是进入循环体还是退出循环
        self.builder.build_conditional_branch(condition_bool, body_bb, exit_bb);
        
        // 4. 生成循环体的代码
        self.builder.position_at_end(body_bb);
        self.codegen_statement(&while_stmt.body);
        // 循环体执行完毕后，必须无条件跳转回循环头，进行下一次条件判断
        self.builder.build_unconditional_branch(header_bb);
        
        // 5. 将 builder 定位到循环退出块，后续代码从这里开始
        self.builder.position_at_end(exit_bb);
    }
}

// 在 impl<'ctx, 'sym> CodeGenerator<'ctx, 'sym> 中

/// ExpressionCodeGen 的实现，负责所有表达式的代码生成。
impl<'ctx, 'sym> ExpressionCodeGen<'ctx> for CodeGenerator<'ctx, 'sym> {
    /// 表达式代码生成的总入口，根据表达式种类进行分派。
    fn codegen_expression(&mut self, expr: &Node<Expression>) -> Result<BasicValueEnum<'ctx>, String> {
        match &expr.kind {
            Expression::Literal(lit) => {
                // 处理字面量
                let const_val = match lit {
                    Literal::Integer(i) => self.context.i32_type().const_int(*i as u64, true).into(),
                    Literal::Char(c) => self.context.i8_type().const_int(*c as u64, false).into(),
                    Literal::Boolean(b) => self.context.bool_type().const_int(*b as u64, false).into(),
                    // 字符串字面量返回一个 `i8*` 指针
                    Literal::String(s) => self.builder.build_global_string_ptr(s, ".str").unwrap().as_pointer_value().into(),
                };
                Ok(const_val)
            }
            Expression::Identifier(ident) => self.codegen_identifier_load(ident),
            Expression::Unary(unary) => self.codegen_unary_expression(unary),
            Expression::Binary(binary) => self.codegen_binary_expression(binary),
            Expression::Assignment(assign) => self.codegen_assignment_expression(assign),
            Expression::Call(call) => self.codegen_call_expression(call),
        }
    }

    /// 为标识符生成 `load` 指令，从内存中读取变量的值。
    fn codegen_identifier_load(&mut self, ident: &Identifier) -> Result<BasicValueEnum<'ctx>, String> {
        // 1. 在局部变量和全局变量中查找符号
        if let Some(symbol) = self.symbol_table.resolve(&ident.name) {
            // 2. 将符号的语义类型转换为 LLVM 的基础类型
            let llvm_type = self.codegen_basic_type(&symbol.ty);
            
            // 3. 查找变量的内存地址 (PointerValue)
            let ptr = self.variables.get(&ident.name) // 先查局部变量
                .copied()
                .or_else(|| self.module.get_global(&ident.name).map(|g| g.as_pointer_value()))
                .unwrap(); // 此时一定能找到，因为 resolve 成功了

            // 4. 使用正确的三个参数调用 build_load
            Ok(self.builder.build_load(llvm_type, ptr, &ident.name).unwrap())
        } else {
            Err(format!("使用了未声明的变量 `{}`", ident.name))
        }
    }

    /// 为一元运算表达式生成代码。
    fn codegen_unary_expression(&mut self, unary_expr: &UnaryExpression) -> Result<BasicValueEnum<'ctx>, String> {
        let operand = self.codegen_expression(&unary_expr.operand)?;
        match unary_expr.operator.kind {
            Operator::Minus => Ok(self.builder.build_int_neg(operand.into_int_value(), "negtmp").unwrap().into()),
            Operator::Not => Ok(self.builder.build_not(operand.into_int_value(), "nottmp").unwrap().into()),
            Operator::And => { // 取地址 &
                // 操作数必须是一个标识符，我们直接返回它在符号表中的地址（指针）
                if let Expression::Identifier(ident) = &unary_expr.operand.kind {
                     if let Some(pointer) = self.variables.get(&ident.name) {
                         return Ok((*pointer).into());
                     }
                }
                Err("取地址操作 `&` 只能应用于变量".to_string())
            }
            Operator::Star => { // 解引用 *
                // 操作数必须是一个指针，我们加载它指向的值
                if operand.is_pointer_value() {
                    Ok(self.builder.build_load(operand.into_pointer_value(), "deref").unwrap())
                } else {
                    Err("解引用 `*` 操作只能应用于指针类型".to_string())
                }
            }
            _ => unreachable!(),
        }
    }

    /// 为二元运算表达式生成代码。
    fn codegen_binary_expression(&mut self, binary_expr: &BinaryExpression) -> Result<BasicValueEnum<'ctx>, String> {
        let op = binary_expr.operator.kind;
        // --- 修正：为 && 和 || 实现短路逻辑 ---
        if op == Operator::AndAnd || op == Operator::OrOr {
            let function = self.current_function.unwrap();
            
            // --- 为 && 实现短路 ---
            if op == Operator::AndAnd {
                // 1. 计算左操作数的值
                let lhs = self.codegen_expression(&binary_expr.left)?.into_int_value();
                
                // 2. 创建基本块：一个用于计算右操作数，另一个用于汇合结果
                let rhs_bb = self.context.append_basic_block(function, "and_rhs");
                let merge_bb = self.context.append_basic_block(function, "and_cont");

                // 3. 获取当前块，用于 phi 节点
                let lhs_bb = self.builder.get_insert_block().unwrap();
                
                // 4. 创建条件分支：如果 lhs 为 false，直接跳到 merge 块；否则跳到 rhs 块
                self.builder.build_conditional_branch(lhs, rhs_bb, merge_bb);
                
                // 5. 生成 rhs 块的代码
                self.builder.position_at_end(rhs_bb);
                let rhs = self.codegen_expression(&binary_expr.right)?.into_int_value();
                self.builder.build_unconditional_branch(merge_bb); // 计算完后跳到 merge 块
                let rhs_bb = self.builder.get_insert_block().unwrap();

                // 6. 生成 merge 块的代码
                self.builder.position_at_end(merge_bb);
                // a. 创建 phi 节点，它的值取决于代码从哪条路径而来
                let phi = self.builder.build_phi(self.context.bool_type(), "andtmp");
                // b. 添加两种可能的值：
                //    - 如果从 lhs 块来（意味着 lhs 是 false），那结果就是 false
                phi.add_incoming(&[(&self.context.bool_type().const_int(0, false), lhs_bb)]);
                //    - 如果从 rhs 块来，那结果就是 rhs 的值
                phi.add_incoming(&[(&rhs, rhs_bb)]);
                
                return Ok(phi.as_basic_value());
            }

            // --- 为 || 实现短路 (逻辑与 && 对称) ---
            if op == Operator::OrOr {
                let lhs = self.codegen_expression(&binary_expr.left)?.into_int_value();
                let rhs_bb = self.context.append_basic_block(function, "or_rhs");
                let merge_bb = self.context.append_basic_block(function, "or_cont");
                let lhs_bb = self.builder.get_insert_block().unwrap();
                
                // 如果 lhs 为 true，直接跳到 merge 块；否则跳到 rhs 块
                self.builder.build_conditional_branch(lhs, merge_bb, rhs_bb);
                
                self.builder.position_at_end(rhs_bb);
                let rhs = self.codegen_expression(&binary_expr.right)?.into_int_value();
                self.builder.build_unconditional_branch(merge_bb);

                let rhs_bb = self.builder.get_insert_block().unwrap();
                
                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(self.context.bool_type(), "ortmp");
                // 如果从 lhs 块来（意味着 lhs 是 true），那结果就是 true
                phi.add_incoming(&[(&self.context.bool_type().const_int(1, false), lhs_bb)]);
                // 如果从 rhs 块来，那结果就是 rhs 的值
                phi.add_incoming(&[(&rhs, rhs_bb)]);

                return Ok(phi.as_basic_value());
            }
        }
        let left = self.codegen_expression(&binary_expr.left)?.into_int_value();
        let right = self.codegen_expression(&binary_expr.right)?.into_int_value();
        
        let result = match op {
            Operator::Plus => self.builder.build_int_add(left, right, "addtmp"),
            Operator::Minus => self.builder.build_int_sub(left, right, "subtmp"),
            Operator::Star => self.builder.build_int_mul(left, right, "multmp"),
            Operator::Slash => self.builder.build_int_signed_div(left, right, "divtmp"),
            Operator::Percent => self.builder.build_int_signed_rem(left, right, "remtmp"),
            // 比较运算
            Operator::Eq => self.builder.build_int_compare(inkwell::IntPredicate::EQ, left, right, "eqtmp"),
            Operator::NotEq => self.builder.build_int_compare(inkwell::IntPredicate::NE, left, right, "netmp"),
            Operator::Lt => self.builder.build_int_compare(inkwell::IntPredicate::SLT, left, right, "lttmp"),
            Operator::Gt => self.builder.build_int_compare(inkwell::IntPredicate::SGT, left, right, "gttmp"),
            Operator::LtEq => self.builder.build_int_compare(inkwell::IntPredicate::SLE, left, right, "letmp"),
            Operator::GtEq => self.builder.build_int_compare(inkwell::IntPredicate::SGE, left, right, "getmp"),
            _ => unreachable!(),
        };
        Ok(result.into())
    }

    /// 为赋值表达式生成代码。
    fn codegen_assignment_expression(&mut self, assign_expr: &AssignmentExpression) -> Result<BasicValueEnum<'ctx>, String> {
        // 1. 获取左值（l-value）的【地址】。
        let target_ptr = match &assign_expr.lvalue.kind {
            Expression::Identifier(ident) => {
                // 如果是 `x = ...`，就从变量表中查找 x 的地址
                self.variables.get(&ident.name).copied()
            }
            Expression::Unary(unary) if unary.operator.kind == Operator::Star => {
                // 如果是 `*p = ...`，就先计算出 p 的值，这个值本身就是一个地址
                self.codegen_expression(&unary.operand)?.into_pointer_value_option()
            }
            _ => None,
        };

        if let Some(ptr) = target_ptr {
            // 2. 计算右边的表达式的值
            let value_to_store = self.codegen_expression(&assign_expr.rvalue)?;
            // 3. 生成 store 指令，将值存入地址
            self.builder.build_store(ptr, value_to_store);
            // 赋值表达式本身的值就是被赋的值
            Ok(value_to_store)
        } else {
            Err("非法的赋值目标".to_string())
        }
    }

    /// 为函数调用表达式生成代码。
    fn codegen_call_expression(&mut self, call_expr: &CallExpression) -> Result<BasicValueEnum<'ctx>, String> {
        let callee_name = if let Expression::Identifier(ident) = &call_expr.callee.kind {
            &ident.name
        } else {
            return Err("复杂的函数调用（如函数指针）暂不支持".to_string());
        };

        if let Some(function) = self.module.get_function(callee_name) {
            // 1. 为所有参数生成代码，得到它们的 LLVM Value
            let mut args: Vec<BasicValueEnum<'ctx>> = Vec::new();
            for arg_expr in &call_expr.arguments {
                args.push(self.codegen_expression(arg_expr)?);
            }

            // 2. 生成 call 指令
            let call_site = self.builder.build_call(function, &args, "calltmp");

            // 3. 函数的返回值就是 call 指令的结果
            // `try_as_basic_value()` 用于处理 `void` 返回类型的函数
            Ok(call_site.try_as_basic_value().left().unwrap_or_else(|| self.context.i32_type().const_int(0, false).into()))
        } else {
            Err(format!("调用了未定义的函数 `{}`", callee_name))
        }
    }
}
