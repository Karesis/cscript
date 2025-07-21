pub mod codes;

use ariadne::{Color, Label as AriadneLabel, Report, ReportKind, Source};
use std::mem;
use crate::lexer::Span;
use codes::ErrorCode; // 从子模块中导入 ErrorCode 结构体

// --- DiagnosticLevel 和 Label 保持不变 ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

impl Label {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

// --- Diagnostic 结构体：核心重构区域 ---

#[derive(Debug, Clone)]
pub struct Diagnostic {
    // [MODIFIED] 这些核心字段现在直接从 ErrorCode 中获取
    code: &'static str,
    level: DiagnosticLevel,
    // message 仍然是可变的 String，以便我们可以添加动态信息（如具体的类型名）
    message: String,

    // 这部分保持不变
    labels: Vec<Label>,
    notes: Vec<String>,
}

impl Diagnostic {
    // [MODIFIED] 主构造函数现在接收一个 ErrorCode 引用作为其核心输入。
    pub fn new(error_code: &'static ErrorCode, primary_label: Label) -> Self {
        Self {
            code: error_code.code,
            level: error_code.level,
            message: error_code.message.to_string(), // 从 ErrorCode 获取默认消息
            labels: vec![primary_label],
            notes: Vec::new(),
        }
    }

    // [MODIFIED] error 和 warning 辅助函数也改为使用 ErrorCode。
    pub fn error(error_code: &'static ErrorCode, primary_label: Label) -> Self {
        // 增加一个断言，确保只用 Error 级别的代码来创建 Error 诊断
        assert!(matches!(error_code.level, DiagnosticLevel::Error), "Tried to create an error diagnostic with a non-error code.");
        Self::new(error_code, primary_label)
    }

    pub fn warning(error_code: &'static ErrorCode, primary_label: Label) -> Self {
        assert!(matches!(error_code.level, DiagnosticLevel::Warning), "Tried to create a warning diagnostic with a non-warning code.");
        Self::new(error_code, primary_label)
    }

    // [NEW] 增加一个非常有用的方法，用于覆盖默认消息，以包含动态信息。
    // 例如，对于类型不匹配错误，我们可以用它来插入具体的类型名。
    pub fn with_dynamic_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }
    
    // [REMOVED] `with_code` 方法不再需要，因为错误码在创建时就已经设定。

    pub fn with_secondary_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn code(&self) -> &str {
        self.code
    }
}

// --- DiagnosticBag 保持不变 ---

#[derive(Debug, Default)]
pub struct DiagnosticBag {
    source: String,
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            diagnostics: Vec::new(),
        }
    }

    pub fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.level == DiagnosticLevel::Error)
    }

    pub fn print(&mut self, file_name: &str) {
        let diags_to_print = mem::take(&mut self.diagnostics);
        if !diags_to_print.is_empty() {
            print_all(file_name, &self.source, diags_to_print);
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Diagnostic> {
        self.diagnostics.iter()
    }
}

// --- Printer 打印逻辑：微小调整 ---

fn print_all(file_name: &str, source_code: &str, diagnostics: Vec<Diagnostic>) {
    let cache = (file_name, Source::from(source_code));

    for diag in diagnostics {
        if diag.labels.is_empty() { continue; }

        let kind = match diag.level {
            DiagnosticLevel::Error => ReportKind::Error,
            DiagnosticLevel::Warning => ReportKind::Warning,
        };

        let color = match diag.level {
            DiagnosticLevel::Error => Color::Red,
            DiagnosticLevel::Warning => Color::Yellow,
        };
        
        let primary_label_info = &diag.labels[0];

        let mut report = Report::build(kind, (file_name, primary_label_info.span.clone()))
            .with_message(&diag.message)
            // [MODIFIED] `diag.code` 现在是 `&'static str`，不再是 Option，可以直接使用。
            .with_code(diag.code);

        for (i, label_info) in diag.labels.iter().enumerate() {
            let label = AriadneLabel::new((file_name, label_info.span.clone()))
                .with_message(&label_info.message);

            let final_label = if i == 0 {
                label.with_color(color)
            } else {
                label.with_color(Color::Blue)
            };
            report.add_label(final_label);
        }

        for note in &diag.notes {
            report = report.with_note(note);
        }

        report.finish().print(cache.clone()).unwrap();
    }
}
