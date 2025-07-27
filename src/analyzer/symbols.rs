// src/analyzer/symbols.rs

use crate::parser::ast::Ident;
use crate::analyzer::types::SemanticType; 
use crate::analyzer::hir;
use std::sync::Arc;
use std::collections::HashMap;

/// 符号表中存储的符号信息
#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Variable { decl: Arc<hir::VarDecl> },
    Function {
        return_type: SemanticType,    // [FIXED] 使用 SemanticType
        params: Vec<(SemanticType, String)>, // [FIXED] 使用 SemanticType
    },
    // [NEW] 新增一个变体，用于存储用户定义的类型信息，比如 struct。
    Type { ty: SemanticType },
}

/// 代表一个独立的作用域，例如一个函数体或一个 if 代码块
type Scope = HashMap<String, SymbolInfo>;

/// 符号表，使用一个栈来管理嵌套的作用域
#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    /// 创建一个新的符号表，并自动进入全局作用域
    pub fn new() -> Self {
        let mut table = SymbolTable::default();
        table.enter_scope(); // Enter the global scope
        table
    }

    /// 进入一个新的作用域（例如，进入一个函数或代码块）
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// 退出当前作用域（例如，离开一个函数或代码块）
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// 在当前作用域中添加一个新符号
    pub fn add_symbol(&mut self, name: &Ident, info: SymbolInfo) -> Result<(), String> {
        let current_scope = self.scopes.last_mut()
            .expect("SymbolTable should always have at least one scope");

        if current_scope.contains_key(&name.name) {
            Err(format!("Symbol '{}' already defined in this scope", name.name))
        } else {
            current_scope.insert(name.name.clone(), info);
            Ok(())
        }
    }

    /// 查找一个符号（从内到外）
    pub fn lookup_symbol(&self, name: &Ident) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&name.name) {
                return Some(info);
            }
        }
        // [DEBUG] 添加打印日志
        println!("[Debug] Symbol '{}' not found in any scope.", name.name);
        None
    }

    /// [NEW] 更新一个已存在的符号，或如果不存在则添加它。
    /// 这在处理全局变量的两遍扫描时非常有用。
    pub fn update_symbol(&mut self, name: &Ident, info: SymbolInfo) {
        // 我们假设全局变量总是在最外层作用域
        if let Some(global_scope) = self.scopes.first_mut() {
            global_scope.insert(name.name.clone(), info);
        }
    }
}