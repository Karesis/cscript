// src/main.rs

// [MODIFIED] 引入所有需要的模块，特别是我们新的诊断系统
use cscript::analyzer::Analyzer;
use cscript::codegen;
use cscript::diagnostics::DiagnosticBag;
use cscript::parser;
use clap::Parser;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

/// 一个用 Rust 编写的、基于 LLVM 的 C 语言编译器
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// 需要编译的源文件路径
    input_file: String,

    /// 输出文件的路径。
    /// 如果不提供，将根据输入文件名自动生成 (例如: test.c -> test)
    #[arg(short, long)]
    output_file: Option<String>,

    /// 只生成 LLVM IR，而不编译成可执行文件
    #[arg(long, short = 'S')]
    emit_llvm: bool,
}

// [REMOVED] 旧的、手动的错误打印函数已被 DiagnosticBag::print() 取代，不再需要。

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let input_path = Path::new(&cli.input_file);
    let source_name = input_path.to_str().unwrap_or("unknown_file");

    // 动态决定输出文件名
    let output_path = match cli.output_file {
        Some(path) => path,
        None => input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "a.out".to_string()),
    };

    let source_code = fs::read_to_string(input_path)
        .map_err(|e| format!("Failed to read file '{}': {}", cli.input_file, e))?;

    // --- [NEW] 统一的编译管道 ---

    // 1. 创建“诊断背包”，它将收集所有阶段的错误
    let mut diagnostics = DiagnosticBag::new(&source_code);

    // 2. 语法分析 (同时包含了词法分析)
    // parser::parse 会处理词法和语法错误，并将它们报告给 diagnostics。
    let ast = match parser::parse(&source_code, &mut diagnostics) {
        Some(ast) => ast,
        None => {
            // 如果 parse 返回 None，说明有错误发生
            eprintln!("Compilation failed during parsing.");
            diagnostics.print(source_name); // 打印所有收集到的错误
            process::exit(1); // 退出
        }
    };

    // 3. 语义分析
    let mut analyzer = Analyzer::new();
    let hir = match analyzer.analyze(&ast, &mut diagnostics) {
        Some(hir) => hir,
        None => {
            eprintln!("Compilation failed during semantic analysis.");
            diagnostics.print(source_name);
            process::exit(1);
        }
    };

    // 4. 代码生成
    let llvm_ir = match codegen::codegen(&hir, &mut diagnostics) {
        Some(ir) => ir,
        None => {
            eprintln!("Compilation failed during code generation.");
            diagnostics.print(source_name);
            process::exit(1);
        }
    };

    // --- 如果编译成功，则继续执行后续步骤 ---

    if cli.emit_llvm {
        fs::write(&output_path, llvm_ir)?;
        println!("Successfully generated LLVM IR at '{}'", output_path);
    } else {
        let temp_ll_file = format!("{}.ll", output_path);
        fs::write(&temp_ll_file, llvm_ir)?;

        println!("Invoking clang to compile and link...");
        let clang_status = Command::new("clang")
            .arg(&temp_ll_file)
            .arg("-o")
            .arg(&output_path)
            .status()?;

        if !clang_status.success() {
            // 如果 clang 失败，也将其视为一个错误
            return Err(format!("Clang failed with status: {}", clang_status).into());
        }

        fs::remove_file(&temp_ll_file)?;
        println!("Successfully compiled executable at '{}'", output_path);
    }

    Ok(())
}
