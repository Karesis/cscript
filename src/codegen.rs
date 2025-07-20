// src/codegen.rs

use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, BasicValueEnum, IntValue, BasicValue, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use std::collections::HashMap;
use std::sync::Arc;

use crate::analyzer::{hir, types::SemanticType};
use crate::parser::ast::{BinaryOp, UnaryOp, LiteralValue};

#[derive(Debug)]
pub enum CodeGenError {
    GenerationError(String),
}

/// 代码生成器
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    
    functions: HashMap<String, FunctionValue<'ctx>>,
    variables: HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("cscript_module");
        let builder = context.create_builder();
        Self {
            context, module, builder,
            functions: HashMap::new(),
            variables: HashMap::new(),
            current_function: None,
            loop_stack: Vec::new(),
        }
    }

    pub fn run(mut self, program: &hir::Program) -> Result<String, CodeGenError> {
        for func in &program.functions {
            self.visit_function(func)?;
        }
        Ok(self.module.print_to_string().to_string())
    }
    
    fn to_llvm_type(&self, ty: &SemanticType) -> BasicTypeEnum<'ctx> {
        match ty {
            SemanticType::Int { .. } => self.context.i32_type().into(),
            SemanticType::Bool => self.context.bool_type().into(),
            SemanticType::Char => self.context.i8_type().into(),
            SemanticType::Ptr(base) => self.to_llvm_type(base).ptr_type(AddressSpace::default()).into(),
            _ => unimplemented!("LLVM type for {:?} is not implemented", ty),
        }
    }

    fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        name: &str,
        ty: T,
    ) -> Result<PointerValue<'ctx>, BuilderError> {
        let builder = self.context.create_builder();
        let entry = self.current_function.unwrap().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        builder.build_alloca(ty, name)
    }

    fn visit_function(&mut self, func: &hir::Function) -> Result<(), CodeGenError> {
        // [FIXED] 明确指定我们想要收集的目标类型是 Vec<BasicMetadataTypeEnum>
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = func.params
            .iter()
            .map(|p| self.to_llvm_type(&p.var_type).into()) // 现在 .into() 知道目标是 BasicMetadataTypeEnum
            .collect();

        // 为了传递给 fn_type，我们需要一个 slice
        let param_types_slice: &[inkwell::types::BasicMetadataTypeEnum<'ctx>] = &param_types;

        // 现在 param_types_slice 的类型是正确的，下面的调用不再报错
        let fn_type = match func.return_type {
            SemanticType::Void => self.context.void_type().fn_type(param_types_slice, false),
            _ => self.to_llvm_type(&func.return_type).fn_type(param_types_slice, false),
        };

        let function = self.module.add_function(&func.name.name, fn_type, None);
        self.functions.insert(func.name.name.clone(), function);
        self.current_function = Some(function);

        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);
        
        self.variables.clear();

        for (hir_param, llvm_param) in func.params.iter().zip(function.get_param_iter()) {
            let param_name = &hir_param.name.name;
            let llvm_type = self.to_llvm_type(&hir_param.var_type);
            
            llvm_param.set_name(param_name);
            
            let alloca = self.create_entry_block_alloca(param_name, llvm_type)
                .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
            
            self.builder.build_store(alloca, llvm_param)
                .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
            
            self.variables.insert(hir_param.clone(), alloca);
        }

        self.visit_block(&func.body)?;
        // 检查 builder 当前所在的块是否已经有终结指令
        let last_block = self.builder.get_insert_block().unwrap();
        if last_block.get_terminator().is_none() {
            // 如果没有终结指令
            if func.return_type == SemanticType::Void {
                // 对于 void 函数，添加隐式的 return
                self.builder.build_return(None);
            } else {
                // 对于非 void 函数，这意味着这个块是不可达的
                // 添加 unreachable 指令来满足 LLVM 验证器
                self.builder.build_unreachable();
            }
        }
        
        if function.verify(true) {
            Ok(())
        } else {
            self.module.print_to_stderr();
            Err(CodeGenError::GenerationError(format!("Invalid function generated: {}", func.name.name)))
        }
    }
    fn visit_block(&mut self, block: &hir::Block) -> Result<(), CodeGenError> {
        for stmt in &block.stmts { self.visit_statement(stmt)?; }
        Ok(())
    }

    fn visit_statement(&mut self, stmt: &hir::Statement) -> Result<(), CodeGenError> {
        match stmt {
            hir::Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    let llvm_value = self.visit_expression(expr)?;
                    self.builder.build_return(Some(&llvm_value));
                } else {
                    self.builder.build_return(None);
                }
            }
            hir::Statement::VarDecl(var_decl) => {
                let var_name = &var_decl.name.name;
                let llvm_type = self.to_llvm_type(&var_decl.var_type);
                let alloca = self.create_entry_block_alloca(var_name, llvm_type)
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
                self.variables.insert(var_decl.clone(), alloca);
                if let Some(initializer) = &var_decl.initializer {
                    let init_val = self.visit_expression(initializer)?;
                    self.builder.build_store(alloca, init_val)
                        .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
                }
            }
            // [IMPLEMENTED] 表达式语句
            hir::Statement::Expr(expr) => {
                self.visit_expression(expr)?;
            }
            // [IMPLEMENTED] If 语句
            hir::Statement::If { condition, then_branch, else_branch, .. } => {
                let function = self.current_function.unwrap();

                let then_bb = self.context.append_basic_block(function, "then");
                let merge_bb = self.context.append_basic_block(function, "merge");
                let else_bb = else_branch.as_ref().map(|_| self.context.append_basic_block(function, "else")).unwrap_or(merge_bb);
                
                let cond_val = self.visit_expression(condition)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb);

                // 构建 then 块
                self.builder.position_at_end(then_bb);
                self.visit_block(then_branch)?;
                // 恢复为简单的跳转逻辑
                if then_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb);
                }

                // 构建 else 块 (如果存在)
                if let Some(else_b) = else_branch {
                    self.builder.position_at_end(else_bb);
                    self.visit_block(else_b)?;
                    if else_bb.get_terminator().is_none() {
                        self.builder.build_unconditional_branch(merge_bb);
                    }
                }

                // 将 builder 定位到 merge 块
                self.builder.position_at_end(merge_bb);
            }
            // [IMPLEMENTED] While 循环
            hir::Statement::While { condition, body, .. } => {
                let function = self.current_function.unwrap();

                // 1. 创建基本块
                let cond_bb = self.context.append_basic_block(function, "loop_cond");
                let body_bb = self.context.append_basic_block(function, "loop_body");
                let after_bb = self.context.append_basic_block(function, "after_loop");
                
                // 2. 将 break/continue 目标入栈
                self.loop_stack.push((cond_bb, after_bb));

                // 3. 从当前位置无条件跳转到循环条件检查
                self.builder.build_unconditional_branch(cond_bb);

                // 4. 构建循环条件块
                self.builder.position_at_end(cond_bb);
                let cond_val = self.visit_expression(condition)?.into_int_value();
                self.builder.build_conditional_branch(cond_val, body_bb, after_bb);

                // 5. 构建循环体块
                self.builder.position_at_end(body_bb);
                self.visit_block(body)?;
                self.builder.build_unconditional_branch(cond_bb); // 循环体结束后跳回条件检查

                // 6. 将 builder 定位到循环结束后的块
                self.builder.position_at_end(after_bb);
                
                // 7. 将 break/continue 目标出栈
                self.loop_stack.pop();
            }
            // [IMPLEMENTED] Break 和 Continue
            hir::Statement::Break(_) => {
                if let Some((_, break_bb)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*break_bb);
                }
            }
            hir::Statement::Continue(_) => {
                if let Some((continue_bb, _)) = self.loop_stack.last() {
                    self.builder.build_unconditional_branch(*continue_bb);
                }
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expr: &hir::Expression) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => match literal {
                LiteralValue::Integer(val) => Ok(self.context.i32_type().const_int(*val as u64, false).into()),
                LiteralValue::Bool(b) => Ok(self.context.bool_type().const_int(if *b { 1 } else { 0 }, false).into()),
                LiteralValue::String(s) => Ok(self.builder.build_global_string_ptr(s, "str_literal")
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))?
                    .as_pointer_value()
                    .into()),
            },
            hir::ExprKind::Variable(var_decl) => {
                let var_ptr = self.variables.get(var_decl)
                    .ok_or_else(|| CodeGenError::GenerationError(format!("Variable '{}' not found in codegen map", var_decl.name.name)))?;
                let var_type = self.to_llvm_type(&var_decl.var_type);
                let loaded_val = self.builder.build_load(var_type, *var_ptr, &var_decl.name.name)
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
                Ok(loaded_val)
            }
            // [FIXED] UnaryOp 现在只处理纯粹的一元运算
            hir::ExprKind::UnaryOp { op, right } => {
                let right_val = self.visit_expression(right)?;
                let result = match op {
                    UnaryOp::Negate => self.builder.build_int_neg(right_val.into_int_value(), "negtmp").map(Into::into),
                    UnaryOp::Not => {
                        let bool_val = right_val.into_int_value();
                        let zero = self.context.bool_type().const_zero();
                        self.builder.build_int_compare(IntPredicate::EQ, bool_val, zero, "nottmp").map(Into::into)
                    }
                    // AddressOf 和 Dereference 已经移到它们自己的分支
                    _ => unreachable!("AddressOf/Dereference should be handled in their own ExprKind variants"),
                };
                result.map_err(|e| CodeGenError::GenerationError(e.to_string()))
            }
            // [ADDED] 为 AddressOf 添加独立的 match 分支
            hir::ExprKind::AddressOf(expr_to_addr) => {
                self.visit_lvalue(expr_to_addr).map(|ptr| ptr.as_basic_value_enum())
            }
            // [ADDED] 为 Dereference 添加独立的 match 分支
            hir::ExprKind::Dereference(ptr_expr) => {
                let ptr_val = self.visit_expression(ptr_expr)?.into_pointer_value();
                let ty = self.to_llvm_type(&expr.resolved_type);
                self.builder.build_load(ty, ptr_val, "dereftmp")
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))
            }
            hir::ExprKind::BinaryOp { op, left, right } => {
                let left_val = self.visit_expression(left)?;
                let right_val = self.visit_expression(right)?;

                let result = match op {
                    BinaryOp::Add => self.builder.build_int_add(left_val.into_int_value(), right_val.into_int_value(), "addtmp").map(Into::into),
                    BinaryOp::Subtract => self.builder.build_int_sub(left_val.into_int_value(), right_val.into_int_value(), "subtmp").map(Into::into),
                    BinaryOp::Multiply => self.builder.build_int_mul(left_val.into_int_value(), right_val.into_int_value(), "multmp").map(Into::into),
                    BinaryOp::Divide => self.builder.build_int_signed_div(left_val.into_int_value(), right_val.into_int_value(), "divtmp").map(Into::into),
                    BinaryOp::Modulo => self.builder.build_int_signed_rem(left_val.into_int_value(), right_val.into_int_value(), "remtmp").map(Into::into),
                    
                    op @ (BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Gt | BinaryOp::Gte | BinaryOp::Lt | BinaryOp::Lte) => {
                        let predicate = match op {
                            BinaryOp::Eq => IntPredicate::EQ, BinaryOp::NotEq => IntPredicate::NE,
                            BinaryOp::Gt => IntPredicate::SGT, BinaryOp::Gte => IntPredicate::SGE,
                            BinaryOp::Lt => IntPredicate::SLT, BinaryOp::Lte => IntPredicate::SLE,
                            _ => unreachable!(),
                        };
                        self.builder.build_int_compare(predicate, left_val.into_int_value(), right_val.into_int_value(), "cmptmp").map(Into::into)
                    }
                    BinaryOp::And => self.builder.build_and(left_val.into_int_value(), right_val.into_int_value(), "andtmp").map(Into::into),
                    BinaryOp::Or => self.builder.build_or(left_val.into_int_value(), right_val.into_int_value(), "ortmp").map(Into::into),
                };
                result.map_err(|e| CodeGenError::GenerationError(e.to_string()))
            }
            hir::ExprKind::Assignment { left, right } => {
                let ptr_to_store = self.visit_lvalue(left)?;
                let val_to_store = self.visit_expression(right)?;
                self.builder.build_store(ptr_to_store, val_to_store)
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;
                Ok(val_to_store)
            }
            hir::ExprKind::FunctionCall { name, args } => {
                // [FIXED] 通过解引用 `*` 来复制 FunctionValue，而不是借用它。
                // 这会立即结束对 self.functions 的不可变借用。
                let function = *self.functions.get(&name.name)
                    .ok_or_else(|| CodeGenError::GenerationError(format!("Function '{}' not found", name.name)))?;

                // 现在 self 是自由的，我们可以在循环中安全地可变借用它。
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.visit_expression(arg)?);
                }
                
                let hir_args: Vec<BasicMetadataValueEnum<'ctx>> = evaluated_args
                    .into_iter()
                    .map(|val| val.into())
                    .collect();
                
                // 注意这里我们直接使用复制出来的 function 值，而不是 *function
                let call_result = self.builder.build_call(function, &hir_args, "calltmp")
                    .map_err(|e| CodeGenError::GenerationError(e.to_string()))?;

                Ok(call_result
                    .try_as_basic_value()
                    .left()
                    .unwrap_or_else(|| self.context.i32_type().const_zero().into()))
            }
        }
    }

    /// 辅助函数：访问一个左值表达式并返回其内存地址
    fn visit_lvalue(&mut self, expr: &hir::Expression) -> Result<PointerValue<'ctx>, CodeGenError> {
        match &expr.kind {
            hir::ExprKind::Variable(var_decl) => {
                self.variables.get(var_decl).cloned()
                    .ok_or_else(|| CodeGenError::GenerationError(format!("Variable '{}' pointer not found", var_decl.name.name)))
            }
            hir::ExprKind::Dereference(ptr_expr) => {
                Ok(self.visit_expression(ptr_expr)?.into_pointer_value())
            }
            _ => Err(CodeGenError::GenerationError("Invalid l-value".to_string())),
        }
    }
}

pub fn codegen(program: &hir::Program) -> Result<String, CodeGenError> {
    let context = Context::create();
    let codegen = CodeGen::new(&context);
    codegen.run(program)
}