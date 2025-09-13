// src/analyzer/symbols.rs

//! 定义了符号表（SymbolTable）和其中存储的符号信息（SymbolInfo）。
//! 符号表是编译器在语义分析阶段用于跟踪标识符（变量、函数、类型等）的核心数据结构。
//! 它采用作用域栈（a stack of scopes）来正确处理词法作用域。

use crate::analyzer::hir;
use crate::analyzer::types::SemanticType;
use crate::utils::Span;
use crate::parser::ast::Ident;
use crate::reporter::SemanticError; 
use std::collections::HashMap;
use std::sync::Arc;

/// 存储在符号表中的具体符号信息。
#[derive(Debug, Clone)]
pub enum SymbolInfo {
    /// 变量符号，直接链接到其 HIR 声明节点。
    Variable { decl: Arc<hir::VarDecl> },
    /// 函数符号，存储其签名信息。
    Function { decl: Arc<hir::FunctionDecl> },
    /// 类型符号，例如结构体定义。
    Type { ty: SemanticType },
}

// 为 SymbolInfo 添加一个辅助方法，用于获取符号定义的位置。
impl SymbolInfo {
    /// 获取此符号定义时的位置（Span）。
    /// 这对于生成高质量的“符号重定义”错误至关重要。
    fn get_span(&self) -> Span {
        match self {
            SymbolInfo::Variable { decl } => decl.name.span.clone(),
            SymbolInfo::Type { ty } => {
                if let SemanticType::Struct { name, .. } = ty {
                    name.span.clone()
                } else {
                    Span::default() // 其他类型目前没有位置信息
                }
            }
            SymbolInfo::Function { decl } => decl.name.span.clone(),
        }
    }
}


/// 代表一个独立的作用域，例如一个函数体或一个 if 代码块。
type Scope = HashMap<String, SymbolInfo>;

/// 符号表，使用一个栈来管理嵌套的作用域。
#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    /// 创建一个新的符号表，并自动进入全局作用域。
    pub fn new() -> Self {
        let mut table = SymbolTable::default();
        // 初始化时自动创建并进入全局作用域
        table.enter_scope();
        table
    }

    /// 进入一个新的（嵌套的）作用域。
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// 退出当前作用域。
    /// 为了安全，此操作不会允许全局作用域被退出。
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// 在当前作用域中定义一个新符号。
    /// 如果符号已存在于当前作用域，将返回一个 `Redefinition` 错误。
    pub fn add_symbol(&mut self, name: &Ident, info: SymbolInfo) -> Result<(), SemanticError> {
        let current_scope = self
            .scopes
            .last_mut()
            .expect("符号表应该总是至少有一个作用域");

        if let Some(existing_info) = current_scope.get(&name.name) {
            // [REFACTORED] 返回结构化的 Redefinition 错误
            let original_span = existing_info.get_span();
            Err(SemanticError::Redefinition {
                name: name.name.clone(),
                span: name.span.clone().into(),
                original_span: original_span.into(),
            })
        } else {
            current_scope.insert(name.name.clone(), info);
            Ok(())
        }
    }

    /// 从最内层作用域开始，向外层查找一个符号。
    /// 如果找到，则返回其信息的引用；否则返回 `None`。
    pub fn lookup_symbol(&self, name: &Ident) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&name.name) {
                return Some(info);
            }
        }
        None
    }

    /// 在全局作用域（最外层）更新或插入一个符号。
    /// 这在处理全局变量的两遍扫描法（Pass 1 收集，Pass 2 填充）中非常有用。
    pub fn update_global_symbol(&mut self, name: &Ident, info: SymbolInfo) {
        if let Some(global_scope) = self.scopes.first_mut() {
            global_scope.insert(name.name.clone(), info);
        }
    }
}