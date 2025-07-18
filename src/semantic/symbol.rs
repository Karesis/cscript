use crate::parser::{Node, TypeSpec};
use crate::diagnostic::Span;
use super::types::Type;
use std::collections::HashMap;

// --- 1. Symbol 定义 ---

/// 定义了一个符号的种类（是变量还是函数）。
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    Function,
}

/// 符号，代表一个被定义的标识符（变量或函数）的所有信息。
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// 符号的种类。
    pub kind: SymbolKind,
    /// 符号的语义类型，这是经过解析和规范化的。
    pub ty: Type,
    /// 这个符号是在源代码的哪个位置被定义的。
    /// 这对于生成高质量的错误信息至关重要（例如：“`x` 在这里已经被定义过了”）。
    pub defined_at: Span,
}

// --- 2. SymbolTable 定义 ---

/// 符号表，负责管理作用域和符号的定义与查找。
///
/// 它的核心是一个作用域栈（`scopes`），每个作用域都是一个从“名字”到“符号信息”的映射。
/// 这种栈式结构天然地支持了 C 语言的嵌套作用域和变量遮蔽规则。
#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    /// 创建一个新的符号表。
    ///
    /// 在创建时，会自动初始化并进入全局作用域（第 0 层）。
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        scopes.push(HashMap::new()); // 推入全局作用域
        Self { scopes }
    }

    /// 进入一个新的作用域（例如，进入一个函数体或 `{}` 代码块）。
    ///
    /// 这对应于向作用域栈中推入一个新的、空的哈希表。
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// 离开当前作用域。
    ///
    /// 这对应于从作用域栈中弹出一个哈希表。
    /// 我们会检查以确保不会意外地离开最外层的全局作用域。
    pub fn leave_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            // 这是一个内部逻辑错误，理论上不应该发生
            // This is an internal logic error and should not happen.
            panic!("试图离开全局作用域！");
        }
    }

    /// 在当前作用域中定义一个新符号。
    ///
    /// - `name`: 符号的名称（例如 "x"）。
    /// - `symbol`: 包含该符号所有信息的 Symbol 结构体。
    ///
    /// 如果在**当前**作用域中，同名符号已存在，则定义失败，返回 `false`。
    /// 成功定义则返回 `true`。
    pub fn define(&mut self, name: String, symbol: Symbol) -> bool {
        // `last_mut()` 获取栈顶的可变引用，即当前作用域
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(&name) {
                return false; // 重复定义
            }
            current_scope.insert(name, symbol);
            true
        } else {
            // 理论上不可能发生，因为总有一个全局作用域
            unreachable!();
        }
    }

    /// 查找一个符号。
    ///
    /// 它会从当前作用域（栈顶）开始，逐层向外（向栈底）查找。
    /// 找到的第一个匹配的符号即为在当前位置可见的符号。
    /// 如果在所有可见作用域中都找不到，则返回 `None`。
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        // `iter().rev()` 从栈顶向栈底反向迭代
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol); // 找到了，立即返回
            }
        }
        None // 找遍了所有作用域都没找到
    }
}