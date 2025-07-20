// src/main.rs

use cscript::{parser, analyzer, codegen};
use clap::Parser;
use std::fs;
use std::path::Path;
use std::process::Command;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::Rich;
use cscript::lexer::{Span, Token};
use cscript::analyzer::main::Analyzer;

/// 一个用 Rust 编写的、基于 LLVM 的 C 语言编译器
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// 需要编译的源文件路径
    input_file: String,

    /// 输出文件的路径。
    /// [UPDATED] 如果不提供，将根据输入文件名自动生成 (例如: test.c -> test)
    #[arg(short, long)]
    output_file: Option<String>,

    /// 只生成 LLVM IR，而不编译成可执行文件
    #[arg(long, short = 'S')]
    emit_llvm: bool,
}

fn print_parse_errors(source_name: &str, source_code: &str, parse_errs: Vec<Rich<Token, Span>>) -> std::io::Result<()> {
    for e in parse_errs {
        let span = (source_name, e.span().clone());
        Report::build(ReportKind::Error, span.clone())
            .with_code(3)
            .with_message(e.reason().to_string())
            .with_label(
                Label::new(span)
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((source_name, Source::from(source_code)))?;
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let input_path = Path::new(&cli.input_file);
    let source_name = input_path.to_str().unwrap_or("unknown_file");

    // [ADDED] 动态决定输出文件名
    let output_path = match cli.output_file {
        // 如果用户通过 -o 提供了文件名，则使用它
        Some(path) => path,
        // 否则，根据输入文件名生成默认名
        None => {
            input_path
                .file_stem() // 获取文件名（不含扩展名），例如 "test.c" -> "test"
                .and_then(|s| s.to_str()) // 转换为 &str
                .map(|s| s.to_string()) // 转换为 String
                .unwrap_or_else(|| "a.out".to_string()) // 如果失败，则使用 "a.out" 作为后备
        }
    };

    let source_code = fs::read_to_string(input_path)
        .map_err(|e| format!("Failed to read file '{}': {}", cli.input_file, e))?;

    let (ast, parse_errs) = parser::parse_source(&source_code);

    if !parse_errs.is_empty() {
        print_parse_errors(source_name, &source_code, parse_errs)?;
        return Err("Parsing failed.".into());
    }
    
    let ast = ast.unwrap();

    let mut analyzer = Analyzer::new();
    let hir = match analyzer.analyze(&ast) {
        Ok(hir) => hir,
        Err(semantic_errs) => {
            eprintln!("Semantic Analysis Errors:");
            for e in semantic_errs {
                 eprintln!("- {:?}", e);
            }
            return Err("Semantic analysis failed.".into());
        }
    };

    let llvm_ir = codegen::codegen(&hir)
        .map_err(|e| format!("CodeGen failed: {:?}", e))?;

    if cli.emit_llvm {
        // [UPDATED] 使用我们动态生成的输出路径
        fs::write(&output_path, llvm_ir)?;
        println!("Successfully generated LLVM IR at '{}'", output_path);
    } else {
        // [UPDATED] 使用我们动态生成的输出路径
        let temp_ll_file = format!("{}.ll", output_path);
        fs::write(&temp_ll_file, llvm_ir)?;

        println!("Invoking clang to compile and link...");
        let clang_status = Command::new("clang")
            .arg(&temp_ll_file)
            .arg("-o")
            .arg(&output_path) // 输出最终的可执行文件
            .status()?;

        if !clang_status.success() {
            return Err(format!("Clang failed with status: {}", clang_status).into());
        }

        fs::remove_file(&temp_ll_file)?;
        println!("Successfully compiled executable at '{}'", output_path);
    }

    Ok(())
}