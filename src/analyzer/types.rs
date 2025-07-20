// src/analyzer/types.rs

use std::sync::Arc;

// [FIXED] 将 Type 重命名为 SemanticType，消除全局命名冲突
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticType {
    // 基本类型
    Void,
    Bool,
    Char,
    Int { width: u8, is_signed: bool },

    // 指针类型
    Ptr(Arc<SemanticType>),

    // 函数类型
    Function {
        ret: Arc<SemanticType>,
        params: Vec<SemanticType>,
    },
}

impl SemanticType {
    /// 一个辅助函数，用于获取类型的字节大小
    pub fn size_of(&self) -> usize {
        match self {
            SemanticType::Void => 0,
            SemanticType::Bool => 1,
            SemanticType::Char => 1,
            SemanticType::Int { width, .. } => (*width / 8) as usize,
            SemanticType::Ptr(_) => 8, 
            SemanticType::Function { .. } => 8,
        }
    }
}