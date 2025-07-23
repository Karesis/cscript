use super::types::SemanticType;
use crate::parser::ast::Ident;
use crate::lexer::Span;
use crate::diagnostics::{
    codes::*,
    Diagnostic, DiagnosticBag, Label,
};

use std::fmt::Display; 

/// 语义错误
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticError {
    SymbolNotFound(Ident),
    SymbolAlreadyExists(Ident),
    TypeMismatch {
        expected: SemanticType,
        found: SemanticType,
        span: Span,
    },
    InvalidLValue(Span),
    NotAFunction(Ident),
    WrongArgumentCount {
        expected: usize,
        found: usize,
        span: Span,
    },
    BreakOutsideLoop(Span),
    ContinueOutsideLoop(Span),
    AssignmentToConst(Span),
    IntegerOverflow {
        target_type: SemanticType,
        span: Span,
    },
    InternalError {
        message: String,
        span: Option<Span>,
    },
}
// 为了方便打印 SemanticType
impl Display for SemanticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self) // 暂时用 Debug 格式
    }
}

impl From<SemanticError> for Diagnostic {
    fn from(error: SemanticError) -> Self {
        match error {
            SemanticError::SymbolNotFound(ident) => Diagnostic::error(
                &E0200_SYMBOL_NOT_FOUND,
                Label::new(ident.span, format!("Cannot find symbol '{}' in this scope", ident.name)),
            ),

            SemanticError::SymbolAlreadyExists(ident) => Diagnostic::error(
                &E0201_SYMBOL_ALREADY_EXISTS,
                Label::new(ident.span, format!("Symbol '{}' is already defined in this scope", ident.name)),
            ),

            SemanticError::TypeMismatch { expected, found, span } => Diagnostic::error(
                &E0202_TYPE_MISMATCH,
                Label::new(span, format!("Expected type `{}`, but found type `{}`", expected, found)),
            ),

            // [COMPLETED] 补完所有剩余的错误转换
            SemanticError::InvalidLValue(span) => Diagnostic::error(
                &E0203_INVALID_LVALUE,
                Label::new(span, "This expression cannot be assigned to"),
            ),

            SemanticError::NotAFunction(ident) => Diagnostic::error(
                &E0204_NOT_A_FUNCTION,
                Label::new(ident.span, format!("'{}' is not a function and cannot be called", ident.name)),
            ),

            SemanticError::WrongArgumentCount { expected, found, span } => Diagnostic::error(
                &E0205_WRONG_ARGUMENT_COUNT,
                Label::new(span, format!("This function expects {} argument(s), but {} were provided", expected, found)),
            ),

            SemanticError::BreakOutsideLoop(span) => Diagnostic::error(
                &E0206_BREAK_OUTSIDE_LOOP,
                Label::new(span, "Cannot use `break` outside of a loop"),
            ),

            SemanticError::ContinueOutsideLoop(span) => Diagnostic::error(
                &E0207_CONTINUE_OUTSIDE_LOOP,
                Label::new(span, "Cannot use `continue` outside of a loop"),
            ),

            SemanticError::AssignmentToConst(span) => Diagnostic::error(
                &E0208_ASSIGNMENT_TO_CONST,
                Label::new(span, "Cannot assign to a variable declared as `const`"),
            ),

            SemanticError::IntegerOverflow { target_type, span } => Diagnostic::error(
                &E0209_INTEGER_OVERFLOW,
                Label::new(span, format!("Value is too large for type `{}`", target_type)),
            ),
            
            SemanticError::InternalError { message, span } => {
                // 如果有具体的 span，就用它；如果没有，就用一个默认的空 span。
                let final_span = span.unwrap_or(0..0);
                let label = Label::new(final_span, &message);

                Diagnostic::error(&E0210_INTERNAL_COMPILER_ERROR, label)
                    // 使用 with_dynamic_message 来显示具体的内部错误信息，
                    // 而不是 ErrorCode 中那个通用的 "Internal compiler error"。
                    .with_dynamic_message(message)
            }
        }
    }
}