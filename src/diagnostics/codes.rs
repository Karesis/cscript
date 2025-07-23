// src/diagnostics/errors.rs

use crate::diagnostics::DiagnosticLevel;

/// Represents a specific error code with its associated information.
/// This struct serves as the single source of truth for all compiler diagnostics.
#[derive(Debug, Clone)]
pub struct ErrorCode {
    pub code: &'static str,
    pub level: DiagnosticLevel,
    pub message: &'static str,
    pub explanation: &'static str,
}

/* 
E00xx: 词法分析 (Lexical Analysis) 错误。

E01xx: 语法分析 (Parsing / Syntax) 错误。

E02xx: 语义分析 (Semantic Analysis) 错误。

E03xx: 代码生成 (Code Generation) 错误。
*/
// --- E00xx: Lexical Analysis Errors ---

pub const E0000_UNRECOGNIZED_CHAR: ErrorCode = ErrorCode {
    code: "E0000",
    level: DiagnosticLevel::Error,
    message: "Unrecognized character",
    explanation: "The compiler encountered a character that is not part of the CScript language definition. \
                  This can happen due to typos or trying to use unsupported symbols."
};

// --- E01xx: Syntax Analysis (Parsing) Errors ---

// [NEW] Generic syntax error.
pub const E0100_SYNTAX_ERROR: ErrorCode = ErrorCode {
    code: "E0100",
    level: DiagnosticLevel::Error,
    message: "Syntax error",
    explanation: "The arrangement of tokens does not match any known grammar rule in CScript. \
                  This is a general-purpose syntax error. Check for missing semicolons, mismatched brackets, or incorrect statement structures."
};

// [NEW] Specific error for invalid integer literals.
pub const E0101_INTEGER_LITERAL_INVALID: ErrorCode = ErrorCode {
    code: "E0101",
    level: DiagnosticLevel::Error,
    message: "Invalid integer literal",
    explanation: "The integer literal is malformed or its value is too large to fit into a 64-bit signed integer. \
                  CScript supports integer values from -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807."
};

// --- E02xx: Semantic Analysis Errors ---

pub const E0200_SYMBOL_NOT_FOUND: ErrorCode = ErrorCode {
    code: "E0200",
    level: DiagnosticLevel::Error,
    message: "Symbol not found",
    explanation: "The compiler could not find the definition for this variable or function in the current scope \
                  or any enclosing scopes. Make sure it is declared before use and check for typos."
};

pub const E0201_SYMBOL_ALREADY_EXISTS: ErrorCode = ErrorCode {
    code: "E0201",
    level: DiagnosticLevel::Error,
    message: "Symbol is already defined",
    explanation: "A variable or function with this name has already been declared in the current scope. \
                  Each identifier must be unique within its scope."
};

pub const E0202_TYPE_MISMATCH: ErrorCode = ErrorCode {
    code: "E0202",
    level: DiagnosticLevel::Error,
    message: "Type mismatch",
    explanation: "The type of an expression does not match the type expected by the context. For example, assigning \
                  a boolean to an integer variable, or using a non-boolean value in an `if` condition."
};

pub const E0203_INVALID_LVALUE: ErrorCode = ErrorCode {
    code: "E0203",
    level: DiagnosticLevel::Error,
    message: "Invalid left-hand side of assignment",
    explanation: "The expression on the left-hand side of an assignment (`=`) must be a valid 'l-value', which \
                  represents a modifiable memory location (like a variable or a pointer dereference). Literals (like `5`) \
                  or arithmetic results (like `x + 1`) cannot be assigned to."
};

pub const E0204_NOT_A_FUNCTION: ErrorCode = ErrorCode {
    code: "E0204",
    level: DiagnosticLevel::Error,
    message: "Cannot call a non-function",
    explanation: "The identifier being used with parentheses `()` is not a function. It might be a variable or another kind of symbol."
};

pub const E0205_WRONG_ARGUMENT_COUNT: ErrorCode = ErrorCode {
    code: "E0205",
    level: DiagnosticLevel::Error,
    message: "Incorrect number of arguments in function call",
    explanation: "The number of arguments provided in the function call does not match the number of parameters in the function's definition."
};

pub const E0206_BREAK_OUTSIDE_LOOP: ErrorCode = ErrorCode {
    code: "E0206",
    level: DiagnosticLevel::Error,
    message: "`break` statement outside of a loop",
    explanation: "The `break` keyword can only be used inside the body of a `while` loop to exit it prematurely."
};

pub const E0207_CONTINUE_OUTSIDE_LOOP: ErrorCode = ErrorCode {
    code: "E0207",
    level: DiagnosticLevel::Error,
    message: "`continue` statement outside of a loop",
    explanation: "The `continue` keyword can only be used inside the body of a `while` loop to skip to the next iteration."
};

pub const E0208_ASSIGNMENT_TO_CONST: ErrorCode = ErrorCode {
    code: "E0208",
    level: DiagnosticLevel::Error,
    message: "Cannot assign to a `const` variable",
    explanation: "Variables declared with the `const` qualifier are read-only and cannot be modified after their initial declaration."
};

pub const E0209_INTEGER_OVERFLOW: ErrorCode = ErrorCode {
    code: "E0209",
    level: DiagnosticLevel::Error,
    message: "Integer literal overflows its target type",
    explanation: "The value of the integer literal is too large or too small to be represented by the type it is being assigned to. \
                  For example, trying to assign 256 to a u8 (which has a maximum value of 255)."
};

pub const E0210_INTERNAL_COMPILER_ERROR: ErrorCode = ErrorCode {
    code: "E0210",
    level: DiagnosticLevel::Error,
    message: "Internal compiler error",
    explanation: "An unexpected error occurred within the compiler's internal logic. This indicates a bug in the CScript compiler itself. \
                  Please report this issue with the source code that caused it."
};

// --- E03xx: Code Generation Errors ---

pub const E0300_LLVM_VERIFICATION_FAILED: ErrorCode = ErrorCode {
    code: "E0300",
    level: DiagnosticLevel::Error,
    message: "LLVM module verification failed",
    explanation: "This is a critical internal compiler error. The generated LLVM Intermediate Representation (IR) is invalid. \
                  This likely indicates a bug in the CScript compiler's code generation logic."
};

pub const E0301_INTERNAL_CODEGEN_ERROR: ErrorCode = ErrorCode {
    code: "E0301",
    level: DiagnosticLevel::Error,
    message: "Internal code generation error",
    explanation: "An unexpected error occurred within the code generator. This likely indicates a bug in the CScript compiler."
};