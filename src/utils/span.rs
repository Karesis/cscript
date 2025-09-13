use chumsky::span::Span as ChumskySpan;
use miette::SourceSpan;
use std::ops::Range;


/// 代表源代码中的一个位置范围，包含起始和结束的字节索引。
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Into<std::ops::Range<usize>> for Span {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

// 为你的 Span 结构体实现 chumsky 所需的 trait
impl ChumskySpan for Span {
    // 1. 定义关联类型 `Context`
    // 文档说："Span contexts have no inherent meaning to Chumsky... For example, Range<usize>’s
    // implementation of Span simply uses () as its context."
    // 因为你的 Span 目前只处理单个源文件，没有文件ID等上下文信息，所以我们用单元类型 `()` 就好。
    type Context = ();
    
    // 2. 定义关联类型 `Offset`
    // 文档说："A type representing a span’s start or end offset... Typically, usize is used."
    // 你的 Span 使用 `usize` 来表示起始和结束，所以这里就是 `usize`。
    type Offset = usize;

    // 3. 实现 `new` 方法
    // 这个方法描述了如何从一个 `Context` 和一个 `Range` 来创建你的 Span。
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        // 我们的 Span 暂时不使用 context，所以直接忽略它。
        // 直接用 range 的 start 和 end 来构造你自己的 Span。
        Self {
            start: range.start,
            end: range.end,
        }
    }

    // 4. 实现 `context` 方法
    // 返回这个 Span 的上下文。因为我们定义了 `Context` 是 `()`，所以这里就返回 `()`。
    fn context(&self) -> Self::Context {
        ()
    }

    // 5. 实现 `start` 方法
    // 返回 Span 的起始位置。这正好对应你结构体里的 `start` 字段。
    fn start(&self) -> Self::Offset {
        self.start
    }

    // 6. 实现 `end` 方法
    // 返回 Span 的结束位置。这正好对应你结构体里的 `end` 字段。
    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        // miette 的 SourceSpan 由“起始点”和“长度”构成
        // 我们的 Span 有“起始点”(start) 和“结束点”(end)
        // 所以，长度 = 结束点 - 起始点
        Self::new(span.start.into(), (span.end - span.start).into())
    }
}