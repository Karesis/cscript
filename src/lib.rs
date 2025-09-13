pub mod reporter;
pub mod lexer;
pub mod parser;
pub mod analyzer;
pub mod codegen;
mod utils;

use reporter::CompilerError;

// 2. 定义一个顶层的、干净的公共 API
/// 编译 CScript 源代码字符串。
///
/// # Arguments
/// * `source` - 要编译的源代码。
///
/// # Returns
/// * `Ok(String)` 包含生成的 LLVM IR。
/// * `Err(Vec<CompilerError>)` 包含所有遇到的编译错误。
// In src/lib.rs
pub fn compile(source: &str) -> Result<String, Vec<CompilerError>> {
    let mut all_errors = Vec::new();

    // 1. 词法分析
    // lexer 总是返回一个 token 向量，即使有错误。
    let (tokens, lexer_errors) = lexer::lex(source);
    all_errors.extend(lexer_errors);

    // 2. 解析
    // 即使有词法错误，我们也把 tokens 传递给解析器，它可能会发现更多问题。
    let (ast, parser_errors) = parser::parse(source.len(), tokens);
    all_errors.extend(parser_errors);

    // [关键的检查点]
    // 如果在词法或解析阶段出现了任何错误，或者解析器没有返回 AST，
    // 那么进行语义分析就没有意义了。我们在这里提前返回所有收集到的错误。
    if !all_errors.is_empty() {
        return Err(all_errors);
    }
    
    // 只有在确定没有词法/语法错误后，我们才能安全地 unwrap AST。
    let ast = ast.expect("Parser should produce an AST if there were no parsing errors");

    // 3. 语义分析
    let analyzer = analyzer::Analyzer::new();
    // analyzer.analyze 返回 Result<_, Vec<CompilerError>>。
    // 如果它返回 Err，`?` 会将这个 Err(vec) 直接作为整个函数的返回值。
    let hir = analyzer.analyze(&ast)?;

    // 4. 代码生成
    // codegen::codegen 也返回 Result<_, Vec<CompilerError>>。
    codegen::codegen(&hir)
}