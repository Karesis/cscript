// In src/codegen/mod.rs

// 1. 声明所有子模块
mod expression;
mod statement;
mod globals;
mod function;
mod lvalue;
mod utils;
mod types;
#[cfg(test)]
mod test;

// 2. 导入依赖
use crate::analyzer::hir;
use crate::reporter::{CompilerError, CodeGenError}; // [MODIFIED]
use crate::analyzer::types::SemanticType;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{StructType, BasicType, BasicTypeEnum, BasicMetadataTypeEnum, AnyTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, GlobalValue};
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::sync::Arc;

// --- 核心抽象 ---

/// CodeGenCtx: 一个精简的、临时的上下文视图。
/// 它只包含真正“上下文相关”的局部状态，以及一个回到“所有者”的引用。
pub struct CodeGenCtx<'a, 'ctx> {
    pub codegen: &'a mut CodeGen<'ctx>,
    
    // 函数内的、频繁变化的局部状态
    // [MODIFIED] current_function 必须是 Option，以支持全局上下文
    pub current_function: Option<FunctionValue<'ctx>>,
    pub variables: &'a mut HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
    pub loop_stack: &'a mut Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}


/// [REFACTORED] 代码生成 Trait
/// generate 方法现在返回 Result，用于错误传播。
pub trait GenerateHIR<'a, 'ctx> {
    type Output;
    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Result<Self::Output, CodeGenError>;
}

/// [REFACTORED] 代码生成器 (CodeGen)
/// 状态的唯一所有者。
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    diagnostics: Vec<CompilerError>,
    
    // 全局状态
    functions: HashMap<String, FunctionValue<'ctx>>,
    struct_types: HashMap<String, StructType<'ctx>>,
    global_variables: HashMap<Arc<hir::VarDecl>, GlobalValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("cscript_module");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            diagnostics: Vec::new(), // 初始化为空
            functions: HashMap::new(),
            struct_types: HashMap::new(),
            global_variables: HashMap::new(),
        }
    }

    /// [REFACTORED] 代码生成的顶层入口函数
    /// 它消费 CodeGen，并返回 LLVM IR 字符串或所有错误的列表。
    pub fn run(mut self, program: &hir::Program) -> Result<String, Vec<CompilerError>> {
        // PASS 0: 预定义所有结构体类型
        types::pass0_define_struct_types(&mut self, program);

        // PASS 1: 生成全局变量
        globals::generate_globals(&mut self, program);
        
        // PASS 2: 生成函数声明
        function::generate_function_declarations(&mut self, program);
        
        // PASS 3: 生成函数体
        function::generate_function_bodies(&mut self, program);

        // 如果在生成过程中出现任何错误，则提前失败
        if !self.diagnostics.is_empty() {
            return Err(self.diagnostics);
        }

        // 验证生成的 LLVM 模块
        if let Err(e) = self.module.verify() {
            self.diagnostics.push(CodeGenError::LLVMVerificationFailed {
                message: e.to_string(),
            }.into());
            return Err(self.diagnostics);
        }

        // 成功，返回 LLVM IR 的字符串表示
        Ok(self.module.print_to_string().to_string())
    }

    pub(super) fn to_llvm_type(&self, ty: &SemanticType) -> BasicTypeEnum<'ctx> {
        match ty {
            SemanticType::Integer { width, .. } => self.context.custom_width_int_type(*width as u32).into(),
            SemanticType::Float { width } => match width {
                32 => self.context.f32_type().into(),
                64 => self.context.f64_type().into(),
                _ => unreachable!(),
            },
            SemanticType::Bool => self.context.bool_type().into(),
            SemanticType::Char => self.context.i8_type().into(),
            SemanticType::Struct { name, .. } => {
                self.struct_types
                    .get(&name.name)
                    .expect("Struct type should have been defined in Pass 0")
                    .as_basic_type_enum()
            }

            // [FIX #1] 修正 Ptr 类型的生成方式
            SemanticType::Ptr(_) => {
                // LLVM 15+ 使用“不透明指针”，所有指针类型都是同一个 `ptr` 类型。
                // 我们不再在指针类型中编码它指向什么，而是直接使用 Context 创建通用指针。
                self.context.ptr_type(AddressSpace::default()).into()
            }
            
            // [FIX #2 & #3] 修正 Function 类型的生成方式
            SemanticType::Function { ret, params } => {
                let param_types: Vec<BasicMetadataTypeEnum> = params
                    .iter()
                    .map(|p| self.to_llvm_type(p).into())
                    .collect();

                // 直接在 if/else 中构建 FunctionType，确保类型统一
                let fn_type = if ret.is_void() {
                    self.context.void_type().fn_type(&param_types, false)
                } else {
                    let basic_ret_type = self.to_llvm_type(ret);
                    basic_ret_type.fn_type(&param_types, false)
                };
                
                // 最终返回函数指针类型
                fn_type.ptr_type(AddressSpace::default()).as_basic_type_enum()
            }
            SemanticType::Void => unreachable!("Void is not a basic type and should be handled separately"),
        }
    }

    /// [CORRECTED] 创建一个用于【函数体】生成的“全功能”上下文。
    pub(super) fn create_fn_ctx<'a>(
        &'a mut self,
        function: FunctionValue<'ctx>,
        local_vars: &'a mut HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
        loop_stack: &'a mut Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    ) -> CodeGenCtx<'a, 'ctx> {
        CodeGenCtx {
            codegen: self,
            current_function: Some(function),
            variables: local_vars,
            loop_stack,
        }
    }

    /// [CORRECTED] 创建一个用于【全局变量初始化】的“精简”上下文。
    pub(super) fn create_global_ctx<'a>(
        &'a mut self,
        // 全局上下文也需要一个临时的、空的局部状态表来满足类型定义
        local_vars: &'a mut HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
        loop_stack: &'a mut Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    ) -> CodeGenCtx<'a, 'ctx> {
        CodeGenCtx {
            codegen: self,
            current_function: None,
            variables: local_vars,
            loop_stack,
        }
    }
}

/// [REFACTORED] 顶层代码生成函数
pub fn codegen(program: &hir::Program) -> Result<String, Vec<CompilerError>> {
    let context = Context::create();
    let codegen = CodeGen::new(&context);
    codegen.run(program)
}