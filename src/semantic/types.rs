use std::fmt;

/// 语义分析阶段使用的规范化类型表示。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    Bool,
    Void,
    Pointer(Box<Type>),
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    /// 一个特殊的类型，用于在类型检查出错时，中断后续的连锁错误。
    Error,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Char => write!(f, "char"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Pointer(inner) => write!(f, "{}*", inner),
            Type::Function { .. } => write!(f, "function"), // 简化显示
            Type::Error => write!(f, "<type error>"),
        }
    }
}