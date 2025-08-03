// src/analyzer/types.rs
use crate::parser::ast::Ident;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

// [REFACTORED] SemanticType 现在能够精确地描述所有原生数字类型。
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType {
    // 基本类型
    Void,
    Bool,
    Char,

    // [MODIFIED] Integer 变体现在可以表示所有位宽和符号性的整数。
    Integer { width: u8, is_signed: bool },

    // [NEW] 新增 Float 变体，用于表示浮点数。
    Float { width: u8 },

    // [NEW] 新增 Struct 变体，这是一个信息极其丰富的节点
    Struct {
        name: Ident,
        // A struct contains a vector of its fields (name and resolved type).
        fields: Vec<(Ident, Arc<SemanticType>)>,
        // The total size of the struct in bytes.
        size: usize,
        // A map from field name to its byte offset within the struct.
        offsets: HashMap<String, usize>,
    },

    // 指针类型
    Ptr(Arc<SemanticType>),

    // 函数类型
    Function {
        ret: Arc<SemanticType>,
        params: Vec<SemanticType>,
    },
}

// [NEW] 手动为 SemanticType 实现 Hash trait。
impl Hash for SemanticType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // 首先哈希枚举变体的“身份”，以区分它们
        core::mem::discriminant(self).hash(state);
        // 然后根据具体的变体来哈希其内部的值
        match self {
            SemanticType::Void => {}
            SemanticType::Bool => {}
            SemanticType::Char => {}
            SemanticType::Integer { width, is_signed } => {
                width.hash(state);
                is_signed.hash(state);
            }
            SemanticType::Float { width } => {
                width.hash(state);
            }
            SemanticType::Struct { name, fields, size, offsets } => {
                name.hash(state);
                fields.hash(state);
                size.hash(state);
                
                // 这是处理 HashMap 哈希的关键逻辑
                let mut ordered_offsets: Vec<_> = offsets.iter().collect();
                // 1. 按照键进行排序，以保证哈希的确定性
                ordered_offsets.sort_by_key(|(k, _)| *k);
                // 2. 对排序后的列表进行哈希
                ordered_offsets.hash(state);
            }
            SemanticType::Ptr(base) => {
                base.hash(state);
            }
            SemanticType::Function { ret, params } => {
                ret.hash(state);
                params.hash(state);
            }
        }
    }
}

// [NEW] 手动实现 Eq。PartialEq 已经可以自动派生（或手动实现），
// 但 Eq 是一个标记 trait，表示 `a == a` 总是为真。
// 对于我们的目的，我们可以安全地为 SemanticType 实现它。
impl Eq for SemanticType {}

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
            // [NEW] 结构体的大小是在语义分析阶段提前计算好的
            SemanticType::Struct { size, .. } => *size,
            // 指针和函数指针的大小取决于目标平台架构，这里我们假定为 64 位。
            SemanticType::Ptr(_) => 8, 
            SemanticType::Function { .. } => 8,
        }
    }
}
