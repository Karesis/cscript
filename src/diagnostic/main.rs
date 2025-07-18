use ariadne::{Color, Label as AriadneLabel, Report, ReportKind, Source};

use std::mem;
use std::ops;
use super::errors::ErrorCode;

// --- 1. Span: Code Location ---
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn to(self, other: Span) -> Self {
        Self::new(self.start, other.end)
    }

    pub fn into_range(self) -> ops::Range<usize> {
        self.start..self.end
    }
}

// --- 2. Diagnostic & Related Types ---

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
    // 这里创建Label时message只需要传入一个实现了Into<String>的trait的东西即可
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    level: DiagnosticLevel,
    message: String,
    code: Option<ErrorCode>,
    labels: Vec<Label>,
    notes: Vec<String>,
}

impl Diagnostic {
    // 这里的message也是的
    pub fn new(level: DiagnosticLevel, message: impl Into<String>, primary_label: Label) -> Self {
        Self {
            level,
            message: message.into(),
            code: None,
            labels: vec![primary_label],
            notes: Vec::new(),
        }
    }

    pub fn error(message: impl Into<String>, primary_label: Label) -> Self {
        Self::new(DiagnosticLevel::Error, message, primary_label)
    }

    pub fn warning(message: impl Into<String>, primary_label: Label) -> Self {
        Self::new(DiagnosticLevel::Warning, message, primary_label)
    }

    // [MODIFIED & IMPROVED] 使用 #[must_use] 并接受 ErrorCode
    #[must_use]
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    #[must_use]
    pub fn with_secondary_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    #[must_use]
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    // 返回 ErrorCode 类型
    pub fn code(&self) -> Option<ErrorCode> {
        self.code
    }
}

// --- 3. DiagnosticBag: The Collector ---
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
    
    /// Reports a simple error with a primary label.
    pub fn report_error(&mut self, message: impl Into<String>, label: Label) {
        let diagnostic = Diagnostic::error(message, label);
        self.report(diagnostic);
    }
    
    /// Reports an error with a specific error code.
    pub fn report_error_with_code(&mut self, message: impl Into<String>, code: ErrorCode, label: Label) {
        let diagnostic = Diagnostic::error(message, label).with_code(code);
        self.report(diagnostic);
    }

    /// Reports a generic diagnostic.
    pub fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.level == DiagnosticLevel::Error)
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

// --- 4. Printer: Internal Printing Logic ---
fn print_all(file_name: &str, source_code: &str, diagnostics: Vec<Diagnostic>) {
    let cache = (file_name, Source::from(source_code));

    for diag in diagnostics {
        // 使用 .split_first() 更优雅地处理标签
        let Some((primary_label_info, secondary_labels)) = diag.labels.split_first() else {
            continue; // Skip diagnostics without any labels
        };

        let kind = match diag.level {
            DiagnosticLevel::Error => ReportKind::Error,
            DiagnosticLevel::Warning => ReportKind::Warning,
        };

        let color = match diag.level {
            DiagnosticLevel::Error => Color::Red,
            DiagnosticLevel::Warning => Color::Yellow,
        };

        let mut report = Report::build(kind, (file_name, primary_label_info.span.into_range()))
            .with_message(&diag.message);

        if let Some(code) = &diag.code {
            report = report.with_code(code.to_string());
        }
        
        // Add the primary label
        report.add_label(
            AriadneLabel::new((file_name, primary_label_info.span.into_range()))
                .with_message(&primary_label_info.message)
                .with_color(color),
        );
        
        // Add all secondary labels
        for label_info in secondary_labels {
            report.add_label(
                AriadneLabel::new((file_name, label_info.span.into_range()))
                    .with_message(&label_info.message)
                    .with_color(Color::Blue), // A different color for context
            );
        }

        for note in &diag.notes {
            report = report.with_note(note);
        }

        report.finish().print(cache.clone()).unwrap();
    }
}