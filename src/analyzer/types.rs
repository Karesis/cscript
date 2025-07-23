// src/analyzer/types.rs

use std::sync::Arc;

// [REFACTORED] SemanticType 现在能够精确地描述所有原生数字类型。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticType {
    // 基本类型
    Void,
    Bool,
    Char,

    // [MODIFIED] Integer 变体现在可以表示所有位宽和符号性的整数。
    Integer { width: u8, is_signed: bool },

    // [NEW] 新增 Float 变体，用于表示浮点数。
    Float { width: u8 },

    // 指针类型
    Ptr(Arc<SemanticType>),

    // 函数类型
    Function {
        ret: Arc<SemanticType>,
        params: Vec<SemanticType>,
    },
}

impl SemanticType {
    /// 一个辅助函数，用于获取类型的字节大小。
    pub fn size_of(&self) -> usize {
        match self {
            SemanticType::Void => 0,
            SemanticType::Bool => 1,
            SemanticType::Char => 1,
            // [MODIFIED] 现在可以正确计算所有整数类型的大小。
            SemanticType::Integer { width, .. } => (*width / 8) as usize,
            // [NEW] 新增对浮点数大小的计算。
            SemanticType::Float { width, .. } => (*width / 8) as usize,
            // 指针和函数指针的大小取决于目标平台架构，这里我们假定为 64 位。
            SemanticType::Ptr(_) => 8, 
            SemanticType::Function { .. } => 8,
        }
    }
}
