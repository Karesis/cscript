// In src/analyzer/mod.rs

// 1. 声明所有模块
pub mod hir;
pub mod types;
mod symbols;
mod expression;
mod statement;
mod globals;
mod function;
mod frame;
#[cfg(test)]
mod test;

// 2. 导入依赖
use crate::parser::ast;
use crate::reporter::{CompilerError, SemanticError}; // [MODIFIED] 导入新的错误报告系统
use symbols::SymbolTable;
use types::SemanticType;
use frame::FrameManager;

// --- 核心抽象：上下文与 Trait ---

/// [REFACTORED]
/// 分析上下文（AnalysisContext），在递归下降分析中传递的可变状态的借用。
/// 它不再直接管理错误收集，使得 `lower` 函数的职责更单一。
pub struct AnalysisContext<'a> {
    pub symbol_table: &'a mut SymbolTable,
    pub frame_manager: &'a mut FrameManager,
    pub current_function_return_type: &'a Option<SemanticType>,
    pub loop_depth: &'a mut u32,
    /// 在类型检查中，上下文可能期望一个特定的类型，用于类型推断和错误报告。
    pub expected_type: Option<&'a SemanticType>,
}

/// [REFACTORED]
/// `Lower` Trait 定义了将 AST 节点降级（转换）为 HIR 节点的统一接口。
/// 它现在返回一个 `Result`，使得错误处理流程更加明确和健壮。
pub trait Lower {
    type Output;
    fn lower(&self, ctx: &mut AnalysisContext<'_>) -> Result<Self::Output, SemanticError>;
}

// --- Analyzer 结构体与实现 ---

/// [REFACTORED]
/// Analyzer 是语义分析器的核心状态机。
/// 它现在拥有自己的诊断收集器 `diagnostics`，不再有外部生命周期依赖。
#[derive(Default)]
pub struct Analyzer {
    symbol_table: SymbolTable,
    frame_manager: FrameManager,
    current_function_return_type: Option<SemanticType>,
    loop_depth: u32,
    diagnostics: Vec<CompilerError>,
}

impl Analyzer {
    /// 创建一个新的 Analyzer 实例。
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            frame_manager: FrameManager::new(),
            diagnostics: Vec::new(),
            ..Default::default()
        }
    }
    
    /// 根据当前 Analyzer 的状态创建一个临时的分析上下文。
    fn create_ctx<'a>(&'a mut self, expected_type: Option<&'a SemanticType>) -> AnalysisContext<'a> {
        AnalysisContext {
            symbol_table: &mut self.symbol_table,
            frame_manager: &mut self.frame_manager,
            current_function_return_type: &self.current_function_return_type,
            loop_depth: &mut self.loop_depth,
            expected_type,
        }
    }

    /// [REFACTORED] 新的、干净的主入口函数。
    /// 它消费（takes ownership of）Analyzer，并返回一个包含 HIR 程序或所有错误的 Result。
    pub fn analyze(
        mut self,
        program: &ast::Program,
    ) -> Result<hir::Program, Vec<CompilerError>> {
        // --- PASS 1: 符号收集 ---
        globals::pass1_collect_symbols(&mut self, program);

        // 如果 Pass 1 出现任何错误（例如符号重定义），则没有必要继续进行 Pass 2。
        // 这是一个重要的“快速失败”策略，可以避免后续产生大量误导性的级联错误。
        if !self.diagnostics.is_empty() {
            return Err(self.diagnostics);
        }

        // --- PASS 2: 完整分析与降级 ---
        let hir_globals = globals::pass2_lower_global_initializers(&mut self, program);
        let hir_functions = function::pass2_lower_functions(&mut self, program);
        
        // [MODIFIED] 正式调用我们已经重构好的 extern 函数收集函数
        let hir_extern_functions = function::pass2_collect_extern_declarations(&mut self, program);

        // 如果在 Pass 2 中收集到了任何错误，则分析失败。
        if !self.diagnostics.is_empty() {
            Err(self.diagnostics)
        } else {
            // 分析成功，构建并返回完整的 HIR 程序。
            Ok(hir::Program {
                functions: hir_functions,
                globals: hir_globals,
                // [MODIFIED] 使用收集到的结果填充
                extern_functions: hir_extern_functions,
            })
        }
    }
}