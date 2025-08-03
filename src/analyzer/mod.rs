// In src/analyzer/mod.rs

// 1. 声明所有模块
pub mod hir;
mod semantic_error;
mod symbols;
pub mod types;
mod expression;
mod statement;
mod globals;
mod functions;

// 2. 导入依赖
use crate::diagnostics::DiagnosticBag;
use crate::parser::ast;
use symbols::SymbolTable;
use types::SemanticType;

// --- 核心抽象：上下文与 Trait ---
pub struct AnalysisContext<'a> {
    pub symbol_table: &'a mut SymbolTable,
    pub diagnostics: &'a mut DiagnosticBag,
    pub current_function_return_type: &'a Option<SemanticType>,
    pub loop_depth: &'a mut u32,
    pub expected_type: Option<&'a SemanticType>,
}
pub trait Lower {
    type Output;
    fn lower<'a>(&self, ctx: &mut AnalysisContext<'a>) -> Option<Self::Output>;
}

// --- Analyzer 结构体与实现 ---
pub struct Analyzer<'a> {
    symbol_table: SymbolTable,
    current_function_return_type: Option<SemanticType>,
    loop_depth: u32,
    // Analyzer 需要持有 diagnostics 的可变借用
    diagnostics: &'a mut DiagnosticBag,
}

impl<'a> Analyzer<'a> {
    pub fn new(diagnostics: &'a mut DiagnosticBag) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            current_function_return_type: None,
            loop_depth: 0,
            diagnostics,
        }
    }
    
    fn create_ctx<'b>(&'b mut self, expected_type: Option<&'b SemanticType>) -> AnalysisContext<'b> {
        AnalysisContext {
            symbol_table: &mut self.symbol_table,
            diagnostics: self.diagnostics,
            current_function_return_type: &self.current_function_return_type,
            loop_depth: &mut self.loop_depth,
            expected_type,
        }
    }

    // [REFACTORED] 新的、干净的主入口函数
    pub fn analyze(&mut self, program: &ast::Program) -> Option<hir::Program> {
        // PASS 1: 符号收集
        globals::pass1_collect_symbols(self, program)?;
        if self.diagnostics.has_errors() { return None; }

        // PASS 2: 完整分析与降级
        let hir_globals = globals::pass2_lower_global_initializers(self, program)?;
        let hir_functions = functions::pass2_lower_functions(self, program)?;
        let hir_extern_functions = functions::pass2_collect_extern_declarations(self, program);
        
        if self.diagnostics.has_errors() {
            None
        } else {
            Some(hir::Program {
                functions: hir_functions,
                globals: hir_globals,
                extern_functions: hir_extern_functions,
            })
        }
    }
}