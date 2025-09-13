//! src/analyzer/types.rs
//!
//! 定义了编译器的核心类型系统 `SemanticType`。
//! `SemanticType` 是对 AST 中类型注解的完全解析和注解版本，
//! 它包含了代码生成所需的所有底层细节，如位宽、符号性、结构体布局等。

use crate::analyzer::symbols::{SymbolInfo, SymbolTable};
use crate::parser::ast::{Ident, Type as AstType};
use crate::reporter::SemanticError; 
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::fmt;

/// 代表一个经过完整语义分析的类型。
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType {
    /// 空类型，通常用于没有返回值的函数。
    Void,
    /// 布尔类型。
    Bool,
    /// 字符类型。
    Char,
    /// 整数类型，精确描述了其位宽和符号性。
    Integer { width: u8, is_signed: bool },
    /// 浮点数类型，精确描述了其位宽。
    Float { width: u8 },
    /// 结构体类型，包含其完整的内存布局信息。
    Struct {
        /// 结构体名称。
        name: Ident,
        /// 包含字段名和其解析后类型的向量。
        fields: Vec<(Ident, Arc<SemanticType>)>,
        /// 结构体在内存中的总大小（字节）。
        size: usize,
        /// 字段名到其在结构体内部字节偏移量的映射。
        offsets: HashMap<String, usize>,
    },
    /// 指针类型，指向一个基类型。
    Ptr(Arc<SemanticType>),
    /// 函数类型，描述了其签名。
    Function {
        ret: Arc<SemanticType>,
        params: Vec<SemanticType>,
    },
}

/// 手动为 SemanticType 实现 Hash trait，以便在需要哈希的集合中使用。
impl Hash for SemanticType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // 首先哈希枚举变体的“身份”，以区分它们
        core::mem::discriminant(self).hash(state);
        // 然后根据具体的变体来哈希其内部的值
        match self {
            SemanticType::Void | SemanticType::Bool | SemanticType::Char => {}
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

                // 为保证哈希的确定性，必须对 HashMap 的内容进行排序后再哈希
                let mut ordered_offsets: Vec<_> = offsets.iter().collect();
                ordered_offsets.sort_by_key(|(k, _)| *k);
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

/// 手动实现 Eq。PartialEq 可以自动派生，但 Eq 是一个标记 trait，
/// 表示 `a == a` 总是为真。对于我们的类型系统，可以安全地实现它。
impl Eq for SemanticType {}

impl SemanticType {
    /// 创建一个默认的 32 位有符号整数类型 (i32)。
    pub fn i32() -> Self {
        SemanticType::Integer { width: 32, is_signed: true }
    }

    /// 创建一个默认的 64 位浮点数类型 (f64)。
    pub fn f64() -> Self {
        SemanticType::Float { width: 64 }
    }

    /// 计算并返回该类型在内存中占用的字节大小。
    pub fn size_of(&self) -> usize {
        match self {
            SemanticType::Void => 0,
            SemanticType::Bool => 1,
            SemanticType::Char => 1,
            SemanticType::Integer { width, .. } => (*width / 8) as usize,
            SemanticType::Float { width, .. } => (*width / 8) as usize,
            // 结构体的大小是在分析阶段提前计算好的
            SemanticType::Struct { size, .. } => *size,
            // 指针和函数指针的大小取决于目标平台架构，这里我们假定为 64 位。
            SemanticType::Ptr(_) => 8,
            SemanticType::Function { .. } => 8,
        }
    }

    /// 检查此类型是否为任意整数或浮点数。
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Integer { .. } | Self::Float { .. })
    }

    /// 检查此类型是否为布尔类型。
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    /// 检查类型是否为 Void。
    pub fn is_void(&self) -> bool {
        matches!(self, SemanticType::Void)
    }

    /// 检查此类型是否为任意整数。
    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Integer { .. })
    }

    /// 检查类型是否为一个字符
    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char)
    }
}

// 为 SemanticType 实现 Display trait
impl fmt::Display for SemanticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemanticType::Void => write!(f, "void"),
            SemanticType::Bool => write!(f, "bool"),
            SemanticType::Char => write!(f, "char"),
            SemanticType::Integer { width, is_signed } => {
                let prefix = if *is_signed { 'i' } else { 'u' };
                write!(f, "{}{}", prefix, width)
            }
            SemanticType::Float { width } => write!(f, "f{}", width),
            // 直接委托给 ast::Ident 的 Display 实现
            SemanticType::Struct { name, .. } => write!(f, "{}", name),
            // 递归打印指针的基类型
            SemanticType::Ptr(base) => write!(f, "*{}", base),
            SemanticType::Function { ret, params } => {
                let params_str: Vec<String> = params.iter().map(|p| p.to_string()).collect();
                write!(f, "fn({}) -> {}", params_str.join(", "), ret)
            }
        }
    }
}

/// [REFACTORED]
/// 将 AST 中的类型表示 (`ast::Type`) 解析为 `SemanticType`。
///
/// 这个函数会查询符号表来解析用户定义的类型（如结构体）。
/// 如果解析成功，返回 `Ok(SemanticType)`。
/// 如果类型未找到或发现一个不是类型的符号，返回 `Err(SemanticError)`。
pub fn resolve_ast_type(
    ast_type: &AstType,
    symbol_table: &SymbolTable,
) -> Result<SemanticType, SemanticError> {
    match ast_type {
        AstType::Void => Ok(SemanticType::Void),
        AstType::Bool => Ok(SemanticType::Bool),
        AstType::Char => Ok(SemanticType::Char),
        AstType::Int | AstType::I32 => Ok(SemanticType::Integer { width: 32, is_signed: true }),
        AstType::I8 => Ok(SemanticType::Integer { width: 8, is_signed: true }),
        AstType::I16 => Ok(SemanticType::Integer { width: 16, is_signed: true }),
        AstType::I64 => Ok(SemanticType::Integer { width: 64, is_signed: true }),
        AstType::U8 => Ok(SemanticType::Integer { width: 8, is_signed: false }),
        AstType::U16 => Ok(SemanticType::Integer { width: 16, is_signed: false }),
        AstType::U32 => Ok(SemanticType::Integer { width: 32, is_signed: false }),
        AstType::U64 => Ok(SemanticType::Integer { width: 64, is_signed: false }),
        AstType::F32 => Ok(SemanticType::Float { width: 32 }),
        AstType::F64 => Ok(SemanticType::Float { width: 64 }),
        
        AstType::Struct(ident) => {
            // 在符号表中查找类型名称
            let symbol_info = symbol_table
                .lookup_symbol(ident)
                .ok_or_else(|| SemanticError::TypeNotFound {
                    type_name: ident.clone(),
                    span: ident.span.clone().into(),
                })?;

            // 验证找到的符号确实是一个类型，并且是一个结构体
            if let SymbolInfo::Type { ty } = symbol_info {
                if let SemanticType::Struct { .. } = ty {
                    Ok(ty.clone())
                } else {
                    Err(SemanticError::SymbolIsNotAStruct { 
                        name: ident.clone(),       
                        span: ident.span.clone().into(),
                    })
                }
            } else {
                Err(SemanticError::SymbolIsNotAStruct { 
                    name: ident.clone(),
                    span: ident.span.clone().into(),
                })
            }
        }
        
        AstType::Ptr(base) => {
            // 递归解析指针的基类型，`?` 操作符会自动传播错误
            let resolved_base = resolve_ast_type(base, symbol_table)?;
            Ok(SemanticType::Ptr(Arc::new(resolved_base)))
        }
    }
}