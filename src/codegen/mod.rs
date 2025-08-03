pub mod expression;
pub mod statement;
pub mod globals;
pub mod function;
pub mod lvalue;
pub mod utils;

use crate::analyzer::hir;
use crate::diagnostics::{
    codes::{E0300_LLVM_VERIFICATION_FAILED, E0301_INTERNAL_CODEGEN_ERROR},
    Diagnostic, DiagnosticBag, Label,
};
use crate::lexer::Span;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{FunctionValue, PointerValue, GlobalValue};
use std::collections::HashMap;
use std::sync::Arc;

// 内部错误模型
#[derive(Debug)]
pub enum CodeGenError {
    LLVMVerificationFailed { message: String },
    InternalError { message: String, span: Option<Span> },
}

// CodeGenError 到 Diagnostic 的转换桥梁
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

// 代码生成上下文 (CodeGenCtx)
// 这个结构体将作为“可变借用”的上下文，传递给所有 GenerateHIR 的实现。
// 它包含了所有生成代码时需要的共享状态和工具。
pub struct CodeGenCtx<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub diagnostics: &'a mut DiagnosticBag,

    // 状态追踪
    pub functions: &'a mut HashMap<String, FunctionValue<'ctx>>,
    pub global_variables: &'a mut HashMap<Arc<hir::VarDecl>, GlobalValue<'ctx>>,
    pub variables: &'a mut HashMap<Arc<hir::VarDecl>, PointerValue<'ctx>>,
    pub current_function: &'a mut Option<FunctionValue<'ctx>>,
    pub loop_stack: &'a mut Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    
    // 我们还需要一个 struct 类型定义的映射表
    pub struct_types: &'a mut HashMap<String, StructType<'ctx>>,
}

// 2. 代码生成 Trait
// 每个可以被翻译成 LLVM IR 的 HIR 节点都应该实现这个 Trait
pub trait GenerateHIR<'a, 'ctx> {
    // 对于表达式，Output 是生成的值 (e.g., BasicValueEnum)
    // 对于语句，Output 是 ()
    type Output;

    // `&self` 是 HIR 节点本身, e.g., hir::Expression
    // `ctx` 是可变的上下文，允许修改 LLVM IR 和编译器状态
    fn generate(&self, ctx: &mut CodeGenCtx<'a, 'ctx>) -> Option<Self::Output>;
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

    // 辅助函数，用于轻松创建一个可变的 CodeGenCtx
    // 这段胶水代码是连接“所有者”和“借用者”的关键
    fn create_ctx<'a>(&'a mut self, diagnostics: &'a mut DiagnosticBag) -> CodeGenCtx<'a, 'ctx> {
        CodeGenCtx {
            // & ?
            context: self.context,
            module: &self.module,
            builder: &self.builder,
            diagnostics,
            functions: &mut self.functions,
            global_variables: &mut self.global_variables,
            variables: &mut self.variables,
            current_function: &mut self.current_function,
            loop_stack: &mut self.loop_stack,
            struct_types: &mut self.struct_types,
        }
    }

    pub fn run(&mut self, program: &hir::Program, diagnostics: &mut DiagnosticBag) -> Option<String> {
        let mut ctx = self.create_ctx(diagnostics);

        // PASS 0: 全局变量
        globals::generate_globals(program, &mut ctx)?;
        
        // PASS 1: 函数声明
        function::generate_function_declarations(program, &mut ctx)?;
        
        // PASS 2: 函数定义
        function::generate_function_bodies(program, &mut ctx)?;

        // 验证生成的模块
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
}

/// 顶层代码生成函数
pub fn codegen(program: &hir::Program, diagnostics: &mut DiagnosticBag) -> Option<String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    codegen.run(program, diagnostics)
}
