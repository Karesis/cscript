use super::types::SemanticType;

// 栈帧布局管理器
// 负责在函数分析期间，为局部变量和参数分配栈空间并计算偏移量。
#[derive(Debug, Default)]
pub struct FrameManager {
    current_offset: i32,
    // 可以在这里添加更多信息，例如栈帧总大小等
}

impl FrameManager {
    pub fn new() -> Self {
        Self { current_offset: 0 }
    }

    /// 在开始分析一个新函数时，重置栈帧。
    pub fn begin_frame(&mut self) {
        self.current_offset = 0;
    }

    /// 为一个新变量分配空间，并返回其相对于基址指针（base pointer）的偏移量。
    pub fn allocate(&mut self, var_type: &SemanticType) -> i32 {
        let size = var_type.size_of() as i32;
        // 简单的内存对齐（通常对齐到 size 的大小，但至少 4 或 8 字节）
        let alignment = if size >= 8 { 8 } else if size >= 4 { 4 } else { 1 };

        // 偏移量向下增长（栈地址从高到低）
        self.current_offset -= size;
        // 确保偏移量是按 alignment 对齐的
        self.current_offset &= !(alignment - 1);
        
        self.current_offset
    }
}