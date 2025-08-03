// src/codegen.rs

use crate::analyzer::{hir, types::SemanticType};
use crate::diagnostics::{
    codes::{E0300_LLVM_VERIFICATION_FAILED, E0301_INTERNAL_CODEGEN_ERROR},
    Diagnostic, DiagnosticBag, Label,
};
use crate::lexer::Span;
use crate::parser::ast::{BinaryOp, LiteralValue, UnaryOp};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, GlobalValue};
use inkwell::{AddressSpace, IntPredicate, FloatPredicate};
use std::collections::HashMap;
use std::sync::Arc;

// [REFACTORED] 升级后的内部错误类型
#[derive(Debug)]
pub enum CodeGenError {
    LLVMVerificationFailed { message: String },
    InternalError { message: String, span: Option<Span> },
}

// [NEW] CodeGenError 到 Diagnostic 的转换桥梁
impl From<CodeGenError> for Diagnostic {
    fn from(error: CodeGenError) -> Self {
        match error {
            CodeGenError::LLVMVerificationFailed { message } => Diagnostic::error(
                &E0300_LLVM_VERIFICATION_FAILED,
                Label::new(0..0, "in generated LLVM IR"),
            )
            .with_dynamic_message(format!("LLVM module verification failed: {}", message))
            .with_note("This is a bug in the CScript compiler. Please report it."),
            CodeGenError::InternalError { message, span } => {
                let span = span.unwrap_or(0..0);
                let label = Label::new(span, &message);
                Diagnostic::error(&E0301_INTERNAL_CODEGEN_ERROR, label).with_dynamic_message(message)
            }
        }
    }
}

/// 代码生成器
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    struct_types: HashMap<String, StructType<'ctx>>,
    global_variables: HashMap<Arc<hir::VarDecl>, GlobalValue<'ctx>>,
    variables: HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("cscript_module");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
            struct_types: HashMap::new(),
            global_variables: HashMap::new(), 
            variables: HashMap::new(),
            current_function: None,
            loop_stack: Vec::new(),
        }
    }

    /// [REFACTORED] 主入口函数，使用 DiagnosticBag 并返回 Option。
    pub fn run(&mut self, program: &hir::Program, diagnostics: &mut DiagnosticBag) -> Option<String> {
        // --- PASS 0: 全局变量定义 ---
        for global_var in &program.globals {
            self.codegen_global_variable(global_var, diagnostics)?;
        }

        // --- PASS 1: 函数声明 ---
        
        // 首先，为所有 extern "C" 函数生成声明
        for func_decl in &program.extern_functions {
            self.codegen_function_declaration(func_decl, diagnostics)?;
        }

        // 然后，为所有我们自己实现的函数生成声明
        for func in &program.functions {
            // 我们需要将 hir::Function 适配为 codegen_function_declaration 需要的 hir::FunctionDecl 格式
            // 这是一个临时的适配，未来可以优化
            let temp_decl = hir::FunctionDecl {
                name: func.name.clone(),
                params: func.params.iter().map(|p| p.var_type.clone()).collect(),
                return_type: func.return_type.clone(),
                is_variadic: false, // 我们自己实现的函数永远不是可变参数的
            };
            self.codegen_function_declaration(&temp_decl, diagnostics)?;
        }

        // --- PASS 2: 函数定义 ---
        for func in &program.functions {
            self.codegen_function_body(func, diagnostics)?;
        }

        if let Err(e) = self.module.verify() {
            diagnostics.report(CodeGenError::LLVMVerificationFailed { message: e.to_string() }.into());
            return None;
        }

        if diagnostics.has_errors() {
            None
        } else {
            Some(self.module.print_to_string().to_string())
        }
    }

    // [NEW] 新增一个专门用于生成全局变量的函数
    fn codegen_global_variable(
        &mut self,
        var_decl: &Arc<hir::VarDecl>,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<()> {
        let llvm_type = self.to_llvm_type(&var_decl.var_type);
        let global = self.module.add_global(llvm_type, Some(AddressSpace::default()), &var_decl.name.name);
        
        // [FIX] 根据变量是否为 const 来设置正确的属性
        if var_decl.is_const {
            global.set_linkage(Linkage::Private); // 常量是模块私有的
            global.set_constant(true);           // 明确标记为常量
        } else {
            global.set_linkage(Linkage::External); // 可变全局变量是外部可见的
            global.set_constant(false);
        }

        // 初始化器逻辑保持不变，但现在它会在正确的链接类型下工作
        if let Some(initializer) = &var_decl.initializer {
            let init_val = self.visit_const_expression(initializer, diagnostics)?;
            global.set_initializer(&init_val);
        } else {
            global.set_initializer(&llvm_type.const_zero());
        }

        self.global_variables.insert(var_decl.clone(), global);
        Some(())
    }

    /// [REFACTORED] PASS 1: 现在可以处理任何函数声明（内部或外部），并支持可变参数。
    fn codegen_function_declaration(
        &mut self,
        func: &hir::FunctionDecl, // [MODIFIED] 参数类型改为更通用的 FunctionDecl
        _diagnostics: &mut DiagnosticBag,
    ) -> Option<()> {
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func
            .params
            .iter()
            .map(|p_type| self.to_llvm_type(p_type).into())
            .collect();

        let fn_type = match &func.return_type {
            SemanticType::Void => self.context.void_type().fn_type(
                &param_types,
                func.is_variadic, // [MODIFIED] 将 is_variadic 标志传递给 LLVM
            ),
            _ => self.to_llvm_type(&func.return_type).fn_type(
                &param_types,
                func.is_variadic, // [MODIFIED] 将 is_variadic 标志传递给 LLVM
            ),
        };

        let function = self.module.add_function(&func.name.name, fn_type, None);
        self.functions.insert(func.name.name.clone(), function);
        Some(())
    }

    /// [REFACTORED] PASS 2: 为已声明的函数生成函数体。
    fn codegen_function_body(
        &mut self,
        func: &hir::Function,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<()> {
        let function = self.functions.get(&func.name.name).unwrap();
        self.current_function = Some(*function);

        let entry_block = self.context.append_basic_block(*function, "entry");
        self.builder.position_at_end(entry_block);
        self.variables.clear();

        for (hir_param, llvm_param) in func.params.iter().zip(function.get_param_iter()) {
            let param_name = &hir_param.name.name;
            let llvm_type = self.to_llvm_type(&hir_param.var_type);
            llvm_param.set_name(param_name);

            let alloca = self.create_entry_block_alloca(param_name, llvm_type, diagnostics)?;
            self.builder.build_store(alloca, llvm_param).unwrap();
            self.variables.insert(hir_param.clone(), alloca);
        }

        self.visit_block(&func.body, diagnostics)?;

        let last_block = self.builder.get_insert_block().unwrap();
        if last_block.get_terminator().is_none() {
            if func.return_type == SemanticType::Void {
                self.builder.build_return(None);
            } else {
                self.builder.build_unreachable();
            }
        }
        Some(())
    }

    /// [CORRECTED] visit_const_expression 现在可以正确处理浮点数常量。
    fn visit_const_expression(
        &mut self,
        expr: &hir::Expression,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<BasicValueEnum<'ctx>> {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => match literal {
                hir::LiteralValue::Float(val) => {
                    let float_type = self.to_llvm_type(&expr.resolved_type).into_float_type();
                    Some(float_type.const_float(*val).into())
                }
                hir::LiteralValue::Integer(val) => {
                    let int_type = self.to_llvm_type(&expr.resolved_type).into_int_type();
                    Some(int_type.const_int(*val as u64, true).into())
                }
                hir::LiteralValue::Bool(b) => Some(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
                _ => {
                    diagnostics.report(CodeGenError::InternalError {
                        message: "Only numeric and boolean literals are supported as global initializers".to_string(),
                        span: Some(expr.span.clone()),
                    }.into());
                    None
                }
            },
            _ => {
                diagnostics.report(CodeGenError::InternalError {
                    message: "Global variable initializers must be constant expressions".to_string(),
                    span: Some(expr.span.clone()),
                }.into());
                None
            }
        }
    }

    fn to_llvm_type(&mut self, ty: &SemanticType) -> BasicTypeEnum<'ctx> {
        match ty {
            SemanticType::Integer { width, .. } => self.context.custom_width_int_type(*width as u32).into(),
            SemanticType::Float { width } => match width {
                32 => self.context.f32_type().into(),
                64 => self.context.f64_type().into(),
                _ => unreachable!(),
            },
            SemanticType::Bool => self.context.bool_type().into(),
            SemanticType::Char => self.context.i8_type().into(),
            SemanticType::Struct { name, fields, .. } => {
                if let Some(struct_type) = self.struct_types.get(&name.name) {
                    return (*struct_type).into();
                }
                let struct_type = self.context.opaque_struct_type(&name.name);
                self.struct_types.insert(name.name.clone(), struct_type);
                let field_types: Vec<BasicTypeEnum<'ctx>> = fields
                    .iter()
                    .map(|(_, field_type)| self.to_llvm_type(field_type))
                    .collect();
                struct_type.set_body(&field_types, false);
                struct_type.into()
            }
            SemanticType::Ptr(_) => self.context.ptr_type(AddressSpace::default()).into(),
            _ => unimplemented!("LLVM type for {:?} is not a basic type", ty),
        }
    }


    fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        name: &str,
        ty: T,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<PointerValue<'ctx>> {
        let builder = self.context.create_builder();
        let entry = self.current_function.unwrap().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        match builder.build_alloca(ty, name) {
            Ok(ptr) => Some(ptr),
            Err(e) => {
                diagnostics.report(CodeGenError::InternalError { message: e.to_string(), span: None }.into());
                None
            }
        }
    }

    fn visit_block(&mut self, block: &hir::Block, diagnostics: &mut DiagnosticBag) -> Option<()> {
        for stmt in &block.stmts {
            self.visit_statement(stmt, diagnostics)?;
        }
        Some(())
    }

    fn visit_statement(&mut self, stmt: &hir::Statement, diagnostics: &mut DiagnosticBag) -> Option<()> {
        match stmt {
            hir::Statement::Block(block) => {
                // 递归地访问这个嵌套的块，`?` 会处理可能的错误。
                self.visit_block(block, diagnostics)?;
            }
            hir::Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    let llvm_value = self.visit_expression(expr, diagnostics)?;
                    self.builder.build_return(Some(&llvm_value));
                } else {
                    self.builder.build_return(None);
                }
            }
            hir::Statement::VarDecl(var_decl) => {
                let var_name = &var_decl.name.name;
                let llvm_type = self.to_llvm_type(&var_decl.var_type);
                let alloca = self.create_entry_block_alloca(var_name, llvm_type, diagnostics)?;
                self.variables.insert(var_decl.clone(), alloca);
                if let Some(initializer) = &var_decl.initializer {
                    let init_val = self.visit_expression(initializer, diagnostics)?;
                    self.builder.build_store(alloca, init_val).unwrap();
                }
            }
            hir::Statement::Expr(expr) => {
                self.visit_expression(expr, diagnostics)?;
            }
            hir::Statement::If { condition, then_branch, else_branch, .. } => {
                let function = self.current_function.unwrap();
                let then_bb = self.context.append_basic_block(function, "then");
                let merge_bb = self.context.append_basic_block(function, "merge");
                let else_bb = else_branch.as_ref().map(|_| self.context.append_basic_block(function, "else")).unwrap_or(merge_bb);
                
                let cond_val = self.visit_expression(condition, diagnostics)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).unwrap();

                self.builder.position_at_end(then_bb);
                self.visit_block(then_branch, diagnostics)?;
                if then_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                if let Some(else_b) = else_branch {
                    self.builder.position_at_end(else_bb);
                    self.visit_block(else_b, diagnostics)?;
                    if else_bb.get_terminator().is_none() {
                        self.builder.build_unconditional_branch(merge_bb).unwrap();
                    }
                }

                self.builder.position_at_end(merge_bb);
            }
            hir::Statement::While { condition, body, .. } => {
                let function = self.current_function.unwrap();
                let cond_bb = self.context.append_basic_block(function, "loop_cond");
                let body_bb = self.context.append_basic_block(function, "loop_body");
                let after_bb = self.context.append_basic_block(function, "after_loop");
                
                self.loop_stack.push((cond_bb, after_bb));
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(cond_bb);
                let cond_val = self.visit_expression(condition, diagnostics)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, body_bb, after_bb).unwrap();

                self.builder.position_at_end(body_bb);
                self.visit_block(body, diagnostics)?;
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(after_bb);
                self.loop_stack.pop();
            }
            hir::Statement::Break(_) => {
                if let Some((_, break_bb)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*break_bb).unwrap();
                }
            }
            hir::Statement::Continue(_) => {
                if let Some((continue_bb, _)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*continue_bb).unwrap();
                }
            }
        }
        Some(())
    }

    fn visit_expression(
        &mut self,
        expr: &hir::Expression,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<BasicValueEnum<'ctx>> {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => match literal {
                hir::LiteralValue::Float(val) => {
                    let float_type = self.to_llvm_type(&expr.resolved_type).into_float_type();
                    Some(float_type.const_float(*val).into())
                }
                hir::LiteralValue::Integer(val) => {
                    let int_type = self.to_llvm_type(&expr.resolved_type).into_int_type();
                    Some(int_type.const_int(*val as u64, true).into())
                }
                hir::LiteralValue::Bool(b) => Some(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
                hir::LiteralValue::String(s) => Some(self.builder.build_global_string_ptr(s, "str").unwrap().as_pointer_value().into()),
            },

            // [NEW] 这是处理结构体实例化的核心逻辑
            hir::ExprKind::StructLiteral { struct_type, fields } => {
                // 1. 获取 struct 的 LLVM 类型
                let llvm_struct_type = self.to_llvm_type(struct_type).into_struct_type();

                // 2. 在栈上为 struct 实例分配内存
                let struct_ptr = self.create_entry_block_alloca("struct_literal", llvm_struct_type, diagnostics)?;

                // 3. 遍历所有字段，并逐一填充
                for (i, (field_ident, field_expr)) in fields.iter().enumerate() {
                    // 3a. 递归生成字段初始化表达式的值
                    let field_val = self.visit_expression(field_expr, diagnostics)?;

                    // 3b. 使用 GEP 计算该字段的内存地址
                    let field_ptr = self.builder
                        .build_struct_gep(llvm_struct_type, struct_ptr, i as u32, &field_ident.name)
                        .unwrap();
                    
                    // 3c. 将值存入该地址
                    self.builder.build_store(field_ptr, field_val).unwrap();
                }

                // 4. 从栈指针中加载整个 struct 的值，作为表达式的结果返回
                Some(self.builder.build_load(llvm_struct_type, struct_ptr, "loadstruct").unwrap())
            }

            hir::ExprKind::Variable(_) => {
                let var_ptr = self.visit_lvalue(expr, diagnostics)?;
                let var_type = self.to_llvm_type(&expr.resolved_type);
                Some(self.builder.build_load(var_type, var_ptr, "loadtmp").unwrap())
            }

            hir::ExprKind::UnaryOp { op, right } => {
                let right_val = self.visit_expression(right, diagnostics)?;
                let result = match op {
                    UnaryOp::Negate => match right.resolved_type {
                        SemanticType::Integer { .. } => self.builder.build_int_neg(right_val.into_int_value(), "negtmp").map(Into::into),
                        SemanticType::Float { .. } => self.builder.build_float_neg(right_val.into_float_value(), "fnegtmp").map(Into::into),
                        _ => unreachable!(),
                    },
                    UnaryOp::Not => {
                        let bool_val = right_val.into_int_value();
                        let zero = self.context.bool_type().const_zero();
                        self.builder.build_int_compare(IntPredicate::EQ, bool_val, zero, "nottmp").map(Into::into)
                    }
                    _ => unreachable!(),
                };
                Some(result.unwrap())
            }

            hir::ExprKind::AddressOf(expr_to_addr) => self.visit_lvalue(expr_to_addr, diagnostics).map(|ptr| ptr.as_basic_value_enum()),
            hir::ExprKind::Dereference(ptr_expr) => {
                let ptr_val = self.visit_expression(ptr_expr, diagnostics)?.into_pointer_value();
                let ty = self.to_llvm_type(&expr.resolved_type);
                Some(self.builder.build_load(ty, ptr_val, "dereftmp").unwrap())
            }

            hir::ExprKind::BinaryOp { op, left, right } => {
                let left_val = self.visit_expression(left, diagnostics)?;
                let right_val = self.visit_expression(right, diagnostics)?;

                match left.resolved_type {
                    SemanticType::Integer { is_signed, .. } => {
                        let left_int = left_val.into_int_value();
                        let right_int = right_val.into_int_value();
                        let result = match op {
                            BinaryOp::Add => self.builder.build_int_add(left_int, right_int, "addtmp").map(Into::into),
                            BinaryOp::Subtract => self.builder.build_int_sub(left_int, right_int, "subtmp").map(Into::into),
                            BinaryOp::Multiply => self.builder.build_int_mul(left_int, right_int, "multmp").map(Into::into),
                            BinaryOp::Divide => if is_signed {
                                self.builder.build_int_signed_div(left_int, right_int, "sdivtmp").map(Into::into)
                            } else {
                                self.builder.build_int_unsigned_div(left_int, right_int, "udivtmp").map(Into::into)
                            },
                            BinaryOp::Modulo => if is_signed {
                                self.builder.build_int_signed_rem(left_int, right_int, "sremtmp").map(Into::into)
                            } else {
                                self.builder.build_int_unsigned_rem(left_int, right_int, "uremtmp").map(Into::into)
                            },
                            op @ (BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte) => {
                                let predicate = int_predicate(op, is_signed);
                                self.builder.build_int_compare(predicate, left_int, right_int, "cmptmp").map(Into::into)
                            }
                            _ => unreachable!(),
                        };
                        Some(result.unwrap())
                    }
                    SemanticType::Float { .. } => {
                        let left_float = left_val.into_float_value();
                        let right_float = right_val.into_float_value();
                        let result = match op {
                            BinaryOp::Add => self.builder.build_float_add(left_float, right_float, "faddtmp").map(Into::into),
                            BinaryOp::Subtract => self.builder.build_float_sub(left_float, right_float, "fsubtmp").map(Into::into),
                            BinaryOp::Multiply => self.builder.build_float_mul(left_float, right_float, "fmultmp").map(Into::into),
                            BinaryOp::Divide => self.builder.build_float_div(left_float, right_float, "fdivtmp").map(Into::into),
                            op @ (BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte) => {
                                let predicate = float_predicate(op);
                                self.builder.build_float_compare(predicate, left_float, right_float, "fcmptmp").map(Into::into)
                            }
                            _ => {
                                diagnostics.report(CodeGenError::InternalError { message: format!("Operator {:?} is not supported for floats", op), span: Some(expr.span.clone()) }.into());
                                return None;
                            }
                        };
                        Some(result.unwrap())
                    }
                    SemanticType::Bool => {
                        let left_bool = left_val.into_int_value();
                        let right_bool = right_val.into_int_value();
                        let result = match op {
                            BinaryOp::And => self.builder.build_and(left_bool, right_bool, "andtmp").map(Into::into),
                            BinaryOp::Or => self.builder.build_or(left_bool, right_bool, "ortmp").map(Into::into),
                            _ => unreachable!(),
                        };
                        Some(result.unwrap())
                    }
                    _ => unreachable!(),
                }
            }
            
            hir::ExprKind::Assignment { left, right } => {
                let ptr_to_store = self.visit_lvalue(left, diagnostics)?;
                let val_to_store = self.visit_expression(right, diagnostics)?;
                self.builder.build_store(ptr_to_store, val_to_store).unwrap();
                Some(val_to_store)
            }

            hir::ExprKind::FunctionCall { name, args } => {
                let function = *self.functions.get(&name.name).unwrap();
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.visit_expression(arg, diagnostics)?);
                }
                let hir_args: Vec<BasicMetadataValueEnum<'ctx>> = evaluated_args
                    .into_iter()
                    .map(|val| val.into())
                    .collect();
                let call_result = self.builder.build_call(function, &hir_args, "calltmp").unwrap();
                Some(call_result.try_as_basic_value().left().unwrap_or_else(|| self.context.i32_type().const_zero().into()))
            }
            hir::ExprKind::MemberAccess { .. } => {
                // 1. 获取成员的地址 (l-value)
                let member_ptr = self.visit_lvalue(expr, diagnostics)?;
                // 2. 从地址中加载值
                let member_type = self.to_llvm_type(&expr.resolved_type);
                Some(self.builder.build_load(member_type, member_ptr, "loadmember").unwrap())
            }
        }
    }

    // [REFACTORED] visit_lvalue 现在会同时查找局部和全局变量
    fn visit_lvalue(
        &mut self,
        expr: &hir::Expression,
        diagnostics: &mut DiagnosticBag,
    ) -> Option<PointerValue<'ctx>> {
        match &expr.kind {
            hir::ExprKind::Variable(var_decl) => {
                // 1. 首先在局部变量/参数中查找
                if let Some(ptr) = self.variables.get(var_decl) {
                    return Some(*ptr);
                }
                // 2. 如果找不到，再去全局变量中查找
                if let Some(global) = self.global_variables.get(var_decl) {
                    return Some(global.as_pointer_value());
                }
                // 3. 如果都找不到，这是一个内部错误
                diagnostics.report(CodeGenError::InternalError {
                    message: format!("Variable '{}' pointer not found in any map", var_decl.name.name),
                    span: Some(expr.span.clone()),
                }.into());
                None
            }
            hir::ExprKind::Dereference(ptr_expr) => {
                Some(self.visit_expression(ptr_expr, diagnostics)?.into_pointer_value())
            }
            hir::ExprKind::MemberAccess { expression, member } => {
                let struct_ptr = self.visit_lvalue(expression, diagnostics)?;
                if let SemanticType::Struct { fields, .. } = &expression.resolved_type {
                    let field_index = fields
                        .iter()
                        .position(|(field_ident, _)| field_ident.name == member.name)
                        .unwrap();

                    // [CORRECTED] 解决了所有权冲突
                    // 1. 先调用 to_llvm_type，结束对 `self` 的第一次可变借用。
                    let llvm_struct_type = self.to_llvm_type(&expression.resolved_type);
                    // 2. 现在可以安全地对 `self` 进行第二次可变借用（通过 self.builder）。

                    match self.builder.build_struct_gep(
                        llvm_struct_type, // 使用临时变量
                        struct_ptr,
                        field_index as u32,
                        &member.name,

                    ) {

                        Ok(ptr) => Some(ptr.into()),
                        Err(e) => {
                            diagnostics.report(
                                CodeGenError::InternalError {
                                    message: format!("Failed to generate GEP for member '{}': {}", member.name, e.to_string()),
                                    span: Some(expr.span.clone()),
                                }
                                .into(),
                            );
                            None
                        }
                    }
                } else {
                    unreachable!("Member access on non-struct type in codegen");
                }
            } 
            _ => {
                diagnostics.report(CodeGenError::InternalError {
                    message: "Invalid l-value".to_string(),
                    span: Some(expr.span.clone()),
                }.into());
                None
            }
        }
    }
}

/// 辅助函数，用于将我们的 BinaryOp 转换为 LLVM 的 IntPredicate
fn int_predicate(op: &BinaryOp, is_signed: bool) -> IntPredicate {
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

/// 辅助函数，用于将我们的 BinaryOp 转换为 LLVM 的 FloatPredicate
fn float_predicate(op: &BinaryOp) -> FloatPredicate {
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

/// [REFACTORED] 顶层代码生成函数，现在使用 DiagnosticBag。
pub fn codegen(program: &hir::Program, diagnostics: &mut DiagnosticBag) -> Option<String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    codegen.run(program, diagnostics)
}